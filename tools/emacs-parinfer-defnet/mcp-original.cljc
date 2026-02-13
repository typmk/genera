(ns defport.protocols.mcp
  "Model Context Protocol (MCP) adapter for defport.

  Implements the MCP protocol (2025-06-18 spec) as a ProtocolAdapter.
  Handles JSON-RPC 2.0 message routing, tool calls, resource access, and prompts.

  Platform-agnostic using reader conditionals for JVM/Node.js compatibility."
  (:require [defport.core :as core]
            [defport.util.protocol :as proto-util]
            [defport.util.pagination :as pagination]
            [defport.util.progress :as progress]
            [defport.util.content :as content]
            [defport.util.batch :as batch]
            [cheshire.core :as json]))

;; ============================================================================
;; Configuration
;; ============================================================================

(def ^:private refactoring-enabled?
  "Check if dangerous refactoring tools are enabled via environment variable.
  Set DEFPORT_ENABLE_REFACTORING=true to enable."
  (Boolean/parseBoolean (or #?(:clj (System/getenv "DEFPORT_ENABLE_REFACTORING")
                               :cljs (when (exists? js/process)
                                      (aget (.-env js/process) "DEFPORT_ENABLE_REFACTORING")))
                           "false")))

;; ============================================================================
;; Protocol State Management
;; ============================================================================

(defonce seen-request-ids* (atom #{}))
(defonce active-operations* (atom {}))
(defonce resource-subscriptions* (atom {}))
(defonce change-notifications-enabled* (atom {:tools false :prompts false :resources false}))
(defonce elicitation-state* (atom {}))
(defonce session-log-levels* (atom {}))
(defonce client-roots* (atom []))
(defonce sampling-state* (atom {}))

(defn reset-protocol-state!
  "Reset protocol state (for testing or reconnection).
  Clears all tracked request IDs, active operations, and subscriptions."
  []
  (reset! seen-request-ids* #{})
  (reset! active-operations* {})
  (reset! resource-subscriptions* {})
  (reset! change-notifications-enabled* {:tools false :prompts false :resources false})
  (reset! elicitation-state* {})
  (reset! session-log-levels* {})
  (reset! client-roots* [])
  (reset! sampling-state* {}))

(defn validate-request-id
  "Validate that a request ID is unique within the session.
  Returns true if valid (or nil for notifications), false if duplicate."
  [request-id]
  (if (nil? request-id)
    true  ; Notifications don't have IDs
    (if (contains? @seen-request-ids* request-id)
      false  ; Duplicate
      (do
        (swap! seen-request-ids* conj request-id)
        true))))

;; ============================================================================
;; Operation Cancellation Support
;; ============================================================================

(defn register-operation
  "Register an operation for cancellation tracking.
  Returns the call-id for chaining."
  [call-id]
  (swap! active-operations* assoc call-id (atom false))
  call-id)

(defn cancel-operation
  "Mark an operation as cancelled."
  [call-id]
  (when-let [cancelled-flag (get @active-operations* call-id)]
    (reset! cancelled-flag true)))

(defn is-cancelled?
  "Check if an operation is cancelled."
  [call-id]
  (when-let [cancelled-flag (get @active-operations* call-id)]
    @cancelled-flag))

(defn unregister-operation
  "Unregister an operation when complete."
  [call-id]
  (swap! active-operations* dissoc call-id))

;; ============================================================================
;; Resource Subscription Support
;; ============================================================================

(defn subscribe-to-resource
  "Subscribe to resource updates.
  Returns subscription ID for tracking."
  [uri]
  (let [sub-id (proto-util/generate-call-id)
        subscribers (get @resource-subscriptions* uri #{})]
    (swap! resource-subscriptions* assoc uri (conj subscribers sub-id))
    sub-id))

(defn unsubscribe-from-resource
  "Unsubscribe from resource updates."
  [uri sub-id]
  (let [subscribers (get @resource-subscriptions* uri #{})]
    (swap! resource-subscriptions* assoc uri (disj subscribers sub-id))
    ;; Clean up empty subscription sets
    (when (empty? (get @resource-subscriptions* uri))
      (swap! resource-subscriptions* dissoc uri))))

(defn get-resource-subscribers
  "Get all subscriber IDs for a resource URI."
  [uri]
  (get @resource-subscriptions* uri #{}))

(defn notify-resource-updated
  "Send resource updated notification to all subscribers.

  transport - Transport instance for sending notifications
  uri - Resource URI that was updated

  Sends notifications/resources/updated to all subscribers via transport."
  [transport uri]
  (when transport
    (let [subscribers (get-resource-subscribers uri)]
      (when (seq subscribers)
        ;; Send notification through transport
        (core/transport-send transport
          {:jsonrpc "2.0"
           :method "notifications/resources/updated"
           :params {:uri uri}})))))

;; ============================================================================
;; Elicitation Support (MCP 2025-06-18)
;; ============================================================================

(defn create-elicitation
  "Create a new elicitation request (server→client user input).

  Args:
    message - Message to present to user
    schema - JSON Schema for requested input (primitives only per MCP spec)

  Returns elicitation ID for tracking.

  Note: This is a synchronous operation from the tool's perspective but
  asynchronous at the protocol level. The tool should block waiting for
  elicit-response! to be called by the client's response handler."
  [message schema]
  (let [elicit-id (proto-util/generate-call-id)
        promise-atom (atom nil)]
    (swap! elicitation-state* assoc elicit-id
           {:message message
            :schema schema
            :timestamp #?(:clj (System/currentTimeMillis)
                         :cljs (.now js/Date))
            :promise promise-atom})
    elicit-id))

(defn get-elicitation
  "Get elicitation state by ID."
  [elicit-id]
  (get @elicitation-state* elicit-id))

(defn elicit-response!
  "Record the client's response to an elicitation request.

  Args:
    elicit-id - Elicitation ID
    action - :accept, :decline, or :cancel
    content - Form data if accepted

  This should be called by the handler that receives the client's response."
  [elicit-id action content]
  (when-let [elicitation (get @elicitation-state* elicit-id)]
    (let [response {:action action :content content}]
      (swap! elicitation-state* update elicit-id assoc
             :action action
             :content content
             :completed true)
      ;; Deliver to promise if it exists
      (when-let [promise-atom (:promise elicitation)]
        (reset! promise-atom response))
      response)))

(defn wait-for-elicitation
  "Block waiting for elicitation response (for use in tool handlers).

  Args:
    elicit-id - Elicitation ID
    timeout-ms - Maximum time to wait (default 60000ms = 1 minute)

  Returns response map with :action and :content, or nil if timeout."
  [elicit-id & [timeout-ms]]
  (let [timeout (or timeout-ms 60000)
        start-time #?(:clj (System/currentTimeMillis)
                     :cljs (.now js/Date))
        elicitation (get @elicitation-state* elicit-id)
        promise-atom (:promise elicitation)]
    (loop []
      (if-let [response @promise-atom]
        response
        (let [elapsed #?(:clj (- (System/currentTimeMillis) start-time)
                        :cljs (- (.now js/Date) start-time))]
          (if (> elapsed timeout)
            nil  ; Timeout
            (do
              #?(:clj (Thread/sleep 100)
                 :cljs (js/setTimeout #() 100))
              (recur))))))))

(defn cancel-elicitation
  "Cancel an elicitation request."
  [elicit-id]
  (swap! elicitation-state* dissoc elicit-id))

;; ============================================================================
;; Logging Support (MCP 2025-06-18)
;; ============================================================================

(def ^:private log-level-order
  "Log level ordering for filtering."
  {:debug 0
   :info 1
   :warning 2
   :error 3})

(defn set-session-log-level!
  "Set minimum log level for a session.

  Args:
    session-id - Session identifier (string or keyword)
    level - Minimum log level (:debug, :info, :warning, :error)

  Messages below this level will not be sent to the client."
  [session-id level]
  (swap! session-log-levels* assoc session-id level))

(defn get-session-log-level
  "Get minimum log level for a session.

  Returns the configured level or :debug (show all) if not set."
  [session-id]
  (get @session-log-levels* session-id :debug))

(defn should-send-log?
  "Check if a log message should be sent based on session's minimum level.

  Args:
    session-id - Session identifier
    level - Log level of the message

  Returns true if message level >= session minimum level."
  [session-id level]
  (let [min-level (get-session-log-level session-id)
        level-value (get log-level-order level 0)
        min-level-value (get log-level-order min-level 0)]
    (>= level-value min-level-value)))

(defn send-log-message
  "Send a log message notification to the client (with level filtering).

  transport - Transport instance for sending notifications
  level - Log level (:debug, :info, :warning, :error)
  message - Log message string
  data - Optional additional data map
  session-id - Optional session ID for filtering (defaults to :default)

  Sends notifications/message to client via transport if level >= session minimum."
  [transport level message & {:keys [data session-id]
                               :or {session-id :default}}]
  (when (and transport (should-send-log? session-id level))
    (core/transport-send transport
      {:jsonrpc "2.0"
       :method "notifications/message"
       :params (cond-> {:level (name level)
                        :logger "defport"
                        :data message}
                 data (assoc :data data))})))

;; ============================================================================
;; Roots Support (MCP 2025-06-18)
;; ============================================================================

(defn handle-roots-list
  "Handle roots/list request - returns client filesystem roots.

  This is typically called BY the server TO the client,
  but we need to track roots the client has shared with us.

  Returns:
    {:roots [{:uri \"file:///workspace\" :name \"Project Root\"}]}"
  [_params _context]
  {:roots @client-roots*})

(defn update-client-roots!
  "Update the list of client roots (called when client notifies us).

  Called when client sends notifications/roots/list_changed."
  [new-roots]
  (reset! client-roots* new-roots))

(defn is-path-in-roots?
  "Check if a file path is within any client root.

  Example:
    (is-path-in-roots? \"/workspace/src/foo.clj\")
    ;; => true if /workspace is a root"
  [file-path]
  (let [roots @client-roots*]
    (boolean
      (some (fn [root]
              (let [root-uri (:uri root)
                    ;; Extract path from file:// URI
                    root-path (if (.startsWith root-uri "file://")
                                (subs root-uri 7)
                                root-uri)]
                (.startsWith file-path root-path)))
            roots))))

(defn validate-file-access
  "Validate that file access is within allowed roots.

  Throws exception if file is outside roots."
  [file-path]
  (when-not (is-path-in-roots? file-path)
    (throw (ex-info "File access denied: outside allowed roots"
                    {:code -32603
                     :file-path file-path
                     :roots @client-roots*}))))

;; ============================================================================
;; Sampling Support (MCP 2025-06-18)
;; ============================================================================

(defn create-sampling-request
  "Create a sampling request to send to client.

  Args:
    messages - Conversation messages (vector of maps with :role and :content)
    opts - Options map with:
      :model-preferences - Optional model hints
      :system-prompt - Optional system prompt
      :max-tokens - Token limit (default 1000)

  Returns:
    Sampling request ID (for tracking response)"
  [messages & [opts]]
  (let [request-id (proto-util/generate-call-id)
        request (cond-> {:messages messages
                         :maxTokens (or (:max-tokens opts) 1000)}
                  (:model-preferences opts)
                  (assoc :modelPreferences (:model-preferences opts))

                  (:system-prompt opts)
                  (assoc :systemPrompt (:system-prompt opts)))]

    ;; Store request
    (swap! sampling-state* assoc request-id
      {:request request
       :status :pending
       :timestamp #?(:clj (System/currentTimeMillis)
                    :cljs (.now js/Date))})

    request-id))

(defn send-sampling-request
  "Send sampling request to client via transport.

  Returns promise that resolves when client responds."
  [transport request-id]
  (let [request (get-in @sampling-state* [request-id :request])]
    ;; Send to client
    (core/transport-send transport
      {:jsonrpc "2.0"
       :id request-id
       :method "sampling/createMessage"
       :params request})

    ;; Create and store promise
    #?(:clj (let [p (promise)]
              (swap! sampling-state* assoc-in [request-id :promise] p)
              p)
       :cljs (let [resolve-fn (atom nil)
                   p (js/Promise.
                       (fn [resolve _reject]
                         (reset! resolve-fn resolve)))]
               (swap! sampling-state* assoc-in [request-id :promise] @resolve-fn)
               p))))

(defn handle-sampling-response
  "Handle client's response to sampling request.

  Called when client returns LLM completion."
  [request-id response]
  (when-let [state (get @sampling-state* request-id)]
    ;; Update state
    (swap! sampling-state* update request-id assoc
      :status :completed
      :response response)

    ;; Resolve promise
    #?(:clj (when-let [p (:promise state)]
              (deliver p response))
       :cljs (when-let [resolve (:promise state)]
                (resolve response)))

    response))

(defn wait-for-sampling-response
  "Block waiting for sampling response (for use in tool handlers).

  Args:
    request-id - Sampling request ID
    timeout-ms - Maximum time to wait (default 60000ms = 1 minute)

  Returns response map, or nil if timeout."
  [request-id & [timeout-ms]]
  (let [timeout (or timeout-ms 60000)
        start-time #?(:clj (System/currentTimeMillis)
                     :cljs (.now js/Date))]
    #?(:clj
       (let [p (get-in @sampling-state* [request-id :promise])]
         (if p
           (deref p timeout nil)
           nil))
       :cljs
       (loop []
         (let [state (get @sampling-state* request-id)
               elapsed #?(:clj (- (System/currentTimeMillis) start-time)
                         :cljs (- (.now js/Date) start-time))]
           (cond
             (= :completed (:status state))
             (:response state)

             (> elapsed timeout)
             nil

             :else
             (do
               (js/setTimeout #() 100)
               (recur))))))))

(defn cancel-sampling-request
  "Cancel a sampling request."
  [request-id]
  (swap! sampling-state* dissoc request-id))

;; ============================================================================
;; Change Notification Support
;; ============================================================================

(defn enable-change-notifications!
  "Enable change notifications for a capability type.

  type - :tools, :prompts, or :resources"
  [type]
  (swap! change-notifications-enabled* assoc type true))

(defn change-notifications-enabled?
  "Check if change notifications are enabled for a type."
  [type]
  (get @change-notifications-enabled* type false))

(defn notify-tools-list-changed
  "Send tools/list_changed notification to client.

  transport - Transport instance for sending notifications

  Applications should call this when tools are added/removed/updated."
  [transport]
  (when (and transport (change-notifications-enabled? :tools))
    (core/transport-send transport
      {:jsonrpc "2.0"
       :method "notifications/tools/list_changed"})))

(defn notify-prompts-list-changed
  "Send prompts/list_changed notification to client.

  transport - Transport instance for sending notifications

  Applications should call this when prompts are added/removed/updated."
  [transport]
  (when (and transport (change-notifications-enabled? :prompts))
    (core/transport-send transport
      {:jsonrpc "2.0"
       :method "notifications/prompts/list_changed"})))

(defn notify-resources-list-changed
  "Send resources/list_changed notification to client.

  transport - Transport instance for sending notifications

  Applications should call this when resources are added/removed/updated."
  [transport]
  (when (and transport (change-notifications-enabled? :resources))
    (core/transport-send transport
      {:jsonrpc "2.0"
       :method "notifications/resources/list_changed"})))

;; ============================================================================
;; Content Formatting
;; ============================================================================

(defn- format-content
  "Format result as MCP content array.

  Handles multiple content types per MCP 2025-06-18 spec:
  - TextContent: Structured data (default) or plain text
  - ImageContent: Base64-encoded images
  - AudioContent: Base64-encoded audio
  - ResourceLink, EmbeddedResource: Resource references

  ObjectContent does NOT exist in the spec - all structured data must use TextContent.

  Args:
    result - Result from tool execution

  Returns:
    Vector of content objects

  Examples:
    ;; Structured data -> TextContent with JSON
    (format-content {:status \"ok\" :data [1 2 3]})
    ;; => [{:type \"text\" :text \"{\\\"status\\\":\\\"ok\\\",...}\"}]

    ;; Image content -> ImageContent (pass through)
    (format-content {:type \"image\" :data \"base64...\" :mimeType \"image/png\"})
    ;; => [{:type \"image\" :data \"base64...\" :mimeType \"image/png\"}]"
  [result]
  (cond
    ;; Image content (has :type \"image\")
    (content/valid-image-content? result)
    [result]

    ;; Audio content (has :type \"audio\")
    (content/valid-audio-content? result)
    [result]

    ;; Text content (has :type \"text\")
    (content/valid-text-content? result)
    [result]

    ;; Structured data -> TextContent with JSON
    :else
    [{:type "text"
      :text (json/generate-string result)}]))

;; ============================================================================
;; MCP Method Handlers
;; ============================================================================

(defn handle-initialize
  "Handle MCP initialize request.
  Returns protocol version and server capabilities."
  [_params context server-info]
  (let [subscriptions-enabled? (get context :enable-subscriptions? true)]
    {:protocolVersion "2025-06-18"
     :serverInfo (or server-info {:name "defport-mcp-server" :version "0.1.0"})
     :capabilities (cond-> {:tools {}
                            :prompts {:listChanged false}
                            :resources {:subscribe subscriptions-enabled?
                                       :listChanged false}
                            :roots {:listChanged false}  ; Roots always available per MCP 2025-06-18
                            :sampling {}  ; Sampling always available per MCP 2025-06-18
                            :elicitation {}  ; Elicitation always available per MCP 2025-06-18
                            :completion {}  ; Completion always available per MCP 2025-06-18
                            :logging {}}  ; Logging always available per MCP 2025-06-18
                     ;; Add refactoring capability if enabled
                     (get context :refactoring-enabled?)
                     (assoc :refactoring {:enabled true}))}))

(defn handle-tools-list
  "Handle tools/list request with pagination.
  Filters dangerous tools unless refactoring is enabled or custom filter provided."
  [params context]
  (let [cursor (:cursor params)
        registry (:port-registry context)
        all-ports (core/list-ports registry)
        refactoring-enabled? (get context :refactoring-enabled? false)
        tool-filter (or (:tool-filter context) identity)

        ;; Filter for tool ports (exclude prompts and resources)
        tool-ports (filter (fn [port-def]
                            (not (or (get-in port-def [:metadata :prompt])
                                    (get-in port-def [:metadata :resource]))))
                          all-ports)

        ;; Apply dangerous tool filtering (hybrid approach)
        filtered-ports (if refactoring-enabled?
                        tool-ports
                        (remove #(get-in % [:metadata :dangerous]) tool-ports))

        ;; Apply custom tool filter if provided
        final-ports (tool-filter filtered-ports)

        ;; Convert ports to MCP tool format
        all-tools (mapv (fn [port-def]
                          (cond-> {:name (name (:id port-def))
                                   :description (:description port-def "")
                                   :inputSchema (:input-schema port-def {})}
                            (:annotations (:metadata port-def))
                            (assoc :annotations (:annotations (:metadata port-def)))))
                        final-ports)
        ;; MCP spec uses page size of 10
        paginated (pagination/paginate-items all-tools cursor {:page-size 10})]
    (cond-> {:tools (:items paginated)}
      (:nextCursor paginated) (assoc :nextCursor (:nextCursor paginated)))))

(defn handle-tools-call
  "Handle tools/call request with progress and cancellation support."
  [params context]
  (let [tool-name (:name params)
        tool-params (:arguments params {})
        registry (:port-registry context)
        call-id (proto-util/generate-call-id)
        progress-token (get-in params [:_meta :progressToken])]

    (if (nil? tool-name)
      {:error {:code -32602 :message "Invalid params: missing tool name"}}

      (let [port (core/get-port registry (keyword tool-name))]
        (if (nil? port)
          {:error {:code -32602 :message (str "Unknown tool: " tool-name)}}

          (try
            (register-operation call-id)

            ;; Check if cancelled before starting
            (if (is-cancelled? call-id)
              {:error {:code -32800 :message "Operation was cancelled"}}

              (let [;; Create progress callback if token provided
                    progress-callback (when progress-token
                                       (progress/create-progress-callback
                                         progress-token
                                         (:transport context)))

                    ;; Create cancellation check
                    cancellation-check (fn [] (is-cancelled? call-id))

                    ;; Build execution context
                    exec-context (assoc context
                                   :params tool-params
                                   :metadata {:call-id call-id
                                             :progress-token progress-token
                                             :progress-callback progress-callback
                                             :cancellation-check cancellation-check})

                    ;; Execute the port
                    result (core/port-execute port exec-context)]

                ;; Check if cancelled during execution
                (if (is-cancelled? call-id)
                  {:error {:code -32800 :message "Operation was cancelled"}}

                  ;; Return result
                  (if-let [err (:error result)]
                    {:error err}
                    ;; MCP expects content array (TextContent, ImageContent, or AudioContent)
                    ;; Use format-content to serialize structured data as TextContent with JSON
                    {:content (or (:content result)
                                  (format-content (:result result)))}))))

            (catch #?(:clj Exception :cljs js/Error) e
              {:error {:code -32603
                       :message (str "Internal error: "
                                     #?(:clj (.getMessage e)
                                        :cljs (.-message e)))}})

            (finally
              (unregister-operation call-id))))))))

(defn handle-tools-call-cancel
  "Handle tools/call/cancel request."
  [params _context]
  (let [call-id (:callId params)]
    (if (nil? call-id)
      {:error {:code -32602 :message "Invalid params: missing callId"}}
      (if (contains? @active-operations* call-id)
        (do
          (cancel-operation call-id)
          {})  ; Success
        {:error {:code -32602 :message (str "Operation not found: " call-id)}}))))

(defn handle-prompts-list
  "Handle prompts/list request with pagination.
  Prompts are ports with :prompt metadata."
  [params context]
  (let [cursor (:cursor params)
        registry (:port-registry context)
        all-ports (core/list-ports registry)
        ;; Filter ports with :prompt metadata
        prompt-ports (filter #(get-in % [:metadata :prompt]) all-ports)
        ;; Convert to MCP prompt format
        all-prompts (mapv (fn [port-def]
                            {:name (name (:id port-def))
                             :description (:description port-def "")
                             :arguments (:prompt-args (:metadata port-def) [])})
                          prompt-ports)
        ;; MCP spec uses page size of 10
        paginated (pagination/paginate-items all-prompts cursor {:page-size 10})]
    (cond-> {:prompts (:items paginated)}
      (:nextCursor paginated) (assoc :nextCursor (:nextCursor paginated)))))

(defn handle-prompts-get
  "Handle prompts/get request."
  [params context]
  (let [prompt-name (:name params)
        prompt-args (:arguments params {})]
    (if (nil? prompt-name)
      {:error {:code -32602 :message "Invalid params: missing prompt name"}}

      (let [registry (:port-registry context)
            port (core/get-port registry (keyword prompt-name))]
        (if (nil? port)
          {:error {:code -32602 :message (str "Unknown prompt: " prompt-name)}}

          (if-not (get-in (core/port-schema port) [:metadata :prompt])
            {:error {:code -32602 :message (str "Not a prompt: " prompt-name)}}

            (try
              (let [exec-context (assoc context :params prompt-args)
                    result (core/port-execute port exec-context)]
                (if-let [err (:error result)]
                  {:error err}
                  ;; Prompts return messages array
                  {:messages (or (:messages result)
                                 [{:role "user"
                                   :content {:type "text"
                                            :text (json/generate-string (:result result))}}])}))

              (catch #?(:clj Exception :cljs js/Error) e
                {:error {:code -32603
                         :message (str "Internal error: "
                                       #?(:clj (.getMessage e)
                                          :cljs (.-message e)))}}))))))))

(defn handle-resources-list
  "Handle resources/list request with pagination.
  Resources are ports with :resource metadata."
  [params context]
  (let [cursor (:cursor params)
        registry (:port-registry context)
        all-ports (core/list-ports registry)
        ;; Filter ports with :resource metadata
        resource-ports (filter #(get-in % [:metadata :resource]) all-ports)
        ;; Convert to MCP resource format
        all-resources (mapv (fn [port-def]
                              {:uri (str "defport://" (name (:id port-def)))
                               :name (name (:id port-def))
                               :description (:description port-def "")
                               :mimeType (get-in port-def [:metadata :mime-type] "application/json")})
                            resource-ports)
        ;; MCP spec uses page size of 10
        paginated (pagination/paginate-items all-resources cursor {:page-size 10})]
    (cond-> {:resources (:items paginated)}
      (:nextCursor paginated) (assoc :nextCursor (:nextCursor paginated)))))

(defn handle-resources-read
  "Handle resources/read request."
  [params context]
  (let [uri (:uri params)]
    (if (nil? uri)
      {:error {:code -32602 :message "Invalid params: missing resource URI"}}

      ;; Parse URI (defport://port-id format)
      (if-let [port-id (when (.startsWith uri "defport://")
                        (keyword (subs uri 10)))]
        (let [registry (:port-registry context)
              port (core/get-port registry port-id)]
          (if (nil? port)
            {:error {:code -32602 :message (str "Unknown resource: " uri)}}

            (if-not (get-in (core/port-schema port) [:metadata :resource])
              {:error {:code -32602 :message (str "Not a resource: " uri)}}

              (try
                (let [exec-context (assoc context :params {:uri uri})
                      result (core/port-execute port exec-context)]
                  (if-let [err (:error result)]
                    {:error err}
                    ;; Resources return contents array
                    {:contents (or (:contents result)
                                   [{:uri uri
                                     :mimeType "application/json"
                                     :text (json/generate-string (:result result))}])}))

                (catch #?(:clj Exception :cljs js/Error) e
                  {:error {:code -32603
                           :message (str "Internal error: "
                                         #?(:clj (.getMessage e)
                                            :cljs (.-message e)))}})))))

        {:error {:code -32602 :message (str "Invalid resource URI: " uri)}}))))

(defn handle-resources-subscribe
  "Handle resources/subscribe request."
  [params _context]
  (let [uri (:uri params)]
    (if (nil? uri)
      {:error {:code -32602 :message "Invalid params: missing resource URI"}}
      (let [sub-id (subscribe-to-resource uri)]
        {}))))  ; Success - empty result

(defn handle-resources-unsubscribe
  "Handle resources/unsubscribe request."
  [params context]
  (let [uri (:uri params)
        sub-id (get-in context [:metadata :subscription-id])]  ; Apps track this
    (if (nil? uri)
      {:error {:code -32602 :message "Invalid params: missing resource URI"}}
      (do
        (unsubscribe-from-resource uri sub-id)
        {}))))  ; Success - empty result

(defn handle-elicitation-create
  "Handle elicitation/create request (server→client user input).

  Per MCP 2025-06-18 spec:
  - Server initiates request for user input
  - Client presents form/UI to user
  - Client responds with accept/decline/cancel

  This is typically called FROM a tool handler via the elicit! DSL helper.

  Params:
    :message - Message to present to user
    :requestedSchema - JSON Schema for input (primitives only)

  Returns:
    {:elicitationId <id>} - ID for tracking the elicitation

  Note: The actual response comes later via client's separate call."
  [params context]
  (let [message (:message params)
        schema (:requestedSchema params)]
    (if (or (nil? message) (nil? schema))
      {:error {:code -32602 :message "Invalid params: message and requestedSchema required"}}
      (let [elicit-id (create-elicitation message schema)]
        {:elicitationId elicit-id}))))

(defn handle-elicitation-submit
  "Handle client's response to elicitation (client→server).

  Params:
    :elicitationId - ID of the elicitation
    :action - \"accept\", \"decline\", or \"cancel\"
    :content - Form data (if accepted)

  Returns empty result on success."
  [params context]
  (let [elicit-id (:elicitationId params)
        action (keyword (:action params))
        content (:content params)]
    (if (nil? elicit-id)
      {:error {:code -32602 :message "Invalid params: elicitationId required"}}
      (do
        (elicit-response! elicit-id action content)
        {}))))

(defn handle-elicitation-cancel
  "Handle elicitation cancellation (client→server).

  Params:
    :elicitationId - ID of the elicitation to cancel

  Returns empty result on success."
  [params context]
  (let [elicit-id (:elicitationId params)]
    (if (nil? elicit-id)
      {:error {:code -32602 :message "Invalid params: elicitationId required"}}
      (do
        (cancel-elicitation elicit-id)
        {}))))

(defn handle-completion-complete
  "Handle completion/complete request for argument autocomplete.

  Per MCP 2025-06-18 spec:
  - Client requests completions for a partial argument value
  - Server returns suggested completions based on context

  Params:
    :ref - Reference map with :type and :name (tool/prompt/resource)
    :argument - Map with :name and :value (partial input)
    :context - Map with :arguments (previously entered values)

  Returns:
    {:completion {:values [...] :total N :hasMore boolean}}"
  [params context]
  (let [ref (:ref params)
        arg-name (get-in params [:argument :name])
        arg-value (get-in params [:argument :value] "")
        prev-args (get-in params [:context :arguments] {})

        ;; Get the referenced port
        port-type (:type ref)
        port-name (:name ref)
        port-id (keyword port-name)]

    (if (or (nil? ref) (nil? arg-name))
      {:error {:code -32602 :message "Invalid params: ref and argument.name required"}}

      (let [registry (:port-registry context)
            port (core/get-port registry port-id)]
        (if (nil? port)
          {:error {:code -32602 :message (str "Unknown port: " port-name)}}

          ;; Get completion function from port metadata
          (let [port-schema (core/port-schema port)
                completion-fn (get-in port-schema [:metadata :completions (keyword arg-name)])]
            (if completion-fn
              (try
                ;; Call completion function with partial value and context
                (let [values (completion-fn arg-value prev-args)
                      ;; Ensure values is a vector of strings
                      value-strs (mapv str values)]
                  {:completion {:values value-strs
                                :total (count value-strs)
                                :hasMore false}})
                (catch #?(:clj Exception :cljs js/Error) e
                  {:error {:code -32603
                           :message (str "Completion error: "
                                         #?(:clj (.getMessage e)
                                            :cljs (.-message e)))}}))
              ;; No completion function defined
              {:completion {:values []
                            :total 0
                            :hasMore false}})))))))

(defn handle-logging-set-level
  "Handle logging/setLevel request (MCP 2025-06-18).

  Allows client to set minimum log level for the session.
  Only messages at or above this level will be sent.

  Params:
    :level - Log level string (\"debug\", \"info\", \"warning\", \"error\")

  Returns empty result on success."
  [params context]
  (let [level-str (:level params)
        level (when level-str (keyword level-str))
        session-id (or (get-in context [:session :id]) :default)]
    (if (and level (contains? log-level-order level))
      (do
        (set-session-log-level! session-id level)
        {})
      {:error {:code -32602
               :message (str "Invalid params: level must be one of debug, info, warning, error")}})))

(defn handle-ping
  "Handle ping request per MCP 2025-06-18 spec.

  Per spec: Receiver MUST respond promptly with an empty response.
  This allows either party to confirm the connection remains active."
  [_params _context _server-info]
  {})

;; ============================================================================
;; MCP Protocol Adapter Implementation
;; ============================================================================

(defrecord McpAdapter [server-info method-handlers* adapter-opts]
  core/ProtocolAdapter

  (protocol-id [_]
    :mcp)

  (protocol-version [_]
    "2025-06-18")

  (protocol-capabilities [_ port-registry]
    (let [ports (core/list-ports port-registry)
          has-prompts? (some #(get-in % [:metadata :prompt]) ports)
          has-resources? (some #(get-in % [:metadata :resource]) ports)
          refactoring-enabled? (:refactoring-enabled? adapter-opts)
          subscriptions-enabled? (get adapter-opts :enable-subscriptions? true)]
      (cond-> {:tools {}
               :prompts (when has-prompts? {:listChanged false})
               :resources (when has-resources?
                           {:subscribe subscriptions-enabled?
                            :listChanged false})
               :roots {:listChanged false}  ; Roots always available per MCP 2025-06-18
               :sampling {}  ; Sampling always available per MCP 2025-06-18
               :elicitation {}  ; Elicitation always available per MCP 2025-06-18
               :completion {}  ; Completion always available per MCP 2025-06-18
               :logging {}}  ; Logging always available per MCP 2025-06-18
        refactoring-enabled?
        (assoc :refactoring {:enabled true}))))

  (protocol-dispatch [this method params context]
    (let [handlers @method-handlers*
          handler (get handlers method)
          ;; Enrich context with adapter options
          enriched-context (merge context adapter-opts)]
      (if handler
        (try
          ;; Validate request ID if present
          (when-let [request-id (:id (:request enriched-context))]
            (when-not (validate-request-id request-id)
              (throw (ex-info "Duplicate request ID"
                              {:code -32600
                               :message "Invalid Request: duplicate request ID"}))))

          ;; Call handler with enriched context
          (handler params enriched-context)

          (catch #?(:clj Exception :cljs js/Error) e
            {:error {:code -32603
                     :message (str "Internal error: "
                                   #?(:clj (.getMessage e)
                                      :cljs (.-message e)))}}))

        ;; Unknown method
        {:error {:code -32601 :message (str "Method not found: " method)}}))))

;; ============================================================================
;; Public API
;; ============================================================================

(defn create-mcp-adapter
  "Create an MCP protocol adapter with hybrid security model.

  Options:
  - :server-info - Map with :name and :version (default: defport-mcp-server v0.1.0)
  - :custom-handlers - Map of method name -> handler fn (overrides defaults)
  - :enable-refactoring - Boolean to enable dangerous tools (default: check DEFPORT_ENABLE_REFACTORING env var)
  - :tool-filter - Custom filter fn (fn [ports] -> filtered-ports) to override default filtering
  - :performance - Performance configuration map with:
    - :batch-processing - Batch processing options:
      - :enabled - Enable concurrent batch processing (default: false)
      - :strategy - :sequential | :pmap | :futures | :core-async (default: :sequential)
      - :max-concurrency - Max parallel operations (default: 10, for :core-async)
      - :timeout-ms - Timeout per item or overall (default: 30000)

  Hybrid Security Model:
  - By default, tools with :dangerous metadata are filtered from tools/list
  - Set :enable-refactoring true or DEFPORT_ENABLE_REFACTORING=true to include dangerous tools
  - Provide custom :tool-filter for application-specific filtering logic
  - Applications mark dangerous tools via {:metadata {:dangerous true}}

  Performance Configuration:
  - Default: Sequential batch processing (backward compatible)
  - Opt-in: Enable concurrent processing for 5-10x speedup on batch operations
  - See defport.util.batch for strategy details

  Returns McpAdapter instance implementing ProtocolAdapter protocol.

  Examples:
    ;; Default (safe mode - dangerous tools filtered)
    (def adapter (create-mcp-adapter))

    ;; Enable refactoring via options
    (def adapter (create-mcp-adapter {:enable-refactoring true}))

    ;; Custom tool filter
    (def adapter (create-mcp-adapter
                   {:tool-filter (fn [tools]
                                   (if (user-has-permission? :refactor)
                                     tools
                                     (remove dangerous? tools)))}))

    ;; Enable concurrent batch processing
    (def adapter (create-mcp-adapter
                   {:performance {:batch-processing {:enabled true
                                                     :strategy :pmap}}}))

    ;; With controlled concurrency
    (def adapter (create-mcp-adapter
                   {:performance {:batch-processing {:enabled true
                                                     :strategy :core-async
                                                     :max-concurrency 10}}}))

    ;; Environment variable control (set DEFPORT_ENABLE_REFACTORING=true)
    (def adapter (create-mcp-adapter))"
  ([]
   (create-mcp-adapter nil))

  ([opts]
   (let [server-info (or (:server-info opts)
                        {:name "defport-mcp-server" :version "0.1.0"})
         custom-handlers (:custom-handlers opts {})

         ;; Hybrid approach: check option first, then env var, default false
         refactoring-enabled? (if (contains? opts :enable-refactoring)
                               (:enable-refactoring opts)
                               refactoring-enabled?)

         ;; Resource subscriptions enabled by default
         subscriptions-enabled? (get opts :enable-subscriptions? true)

         ;; Performance options (default to sequential)
         performance (merge {:batch-processing {:enabled false
                                                :strategy :sequential
                                                :max-concurrency 10
                                                :timeout-ms 30000}}
                           (:performance opts))

         ;; Build adapter options for context enrichment
         adapter-opts {:refactoring-enabled? refactoring-enabled?
                      :tool-filter (:tool-filter opts)
                      :enable-subscriptions? subscriptions-enabled?
                      :performance performance}

         ;; Default method handlers
         default-handlers {"initialize" #(handle-initialize %1 %2 server-info)
                          "tools/list" handle-tools-list
                          "tools/call" handle-tools-call
                          "tools/call/cancel" handle-tools-call-cancel
                          "prompts/list" handle-prompts-list
                          "prompts/get" handle-prompts-get
                          "resources/list" handle-resources-list
                          "resources/read" handle-resources-read
                          "resources/subscribe" handle-resources-subscribe
                          "resources/unsubscribe" handle-resources-unsubscribe
                          "resources/templates/list" (fn [_params _context] {:resourceTemplates []})  ; MCP Inspector extension
                          "roots/list" handle-roots-list
                          "elicitation/create" handle-elicitation-create
                          "elicitation/submit" handle-elicitation-submit
                          "elicitation/cancel" handle-elicitation-cancel
                          "completion/complete" handle-completion-complete
                          "logging/setLevel" handle-logging-set-level
                          "ping" #(handle-ping %1 %2 server-info)}

         ;; Merge custom handlers
         method-handlers (merge default-handlers custom-handlers)]

     (->McpAdapter server-info (atom method-handlers) adapter-opts))))

(defn register-custom-handler!
  "Register or override a custom method handler.

  adapter - McpAdapter instance
  method - Method name string (e.g., \"custom/analyze\")
  handler - Handler fn with signature: (fn [params context] -> result)

  Example:
    (register-custom-handler! adapter \"custom/analyze\"
      (fn [params context]
        {:result \"analyzed\"}))"
  [adapter method handler]
  (swap! (:method-handlers* adapter) assoc method handler))

;; ============================================================================
;; Performance Configuration Accessors
;; ============================================================================

(defn get-batch-strategy
  "Get the batch processing strategy from adapter.

  Args:
    adapter - McpAdapter instance

  Returns:
    Keyword - :sequential | :pmap | :futures | :core-async"
  [adapter]
  (get-in (:adapter-opts adapter) [:performance :batch-processing :strategy] :sequential))

(defn get-batch-opts
  "Get batch processing options from adapter.

  Args:
    adapter - McpAdapter instance

  Returns:
    Map with batch options ready to pass to batch/process-batch"
  [adapter]
  (batch/get-batch-opts (:performance (:adapter-opts adapter))))

(defn batch-enabled?
  "Check if batch processing is enabled in adapter.

  Args:
    adapter - McpAdapter instance

  Returns:
    Boolean - true if batch processing is explicitly enabled"
  [adapter]
  (get-in (:adapter-opts adapter) [:performance :batch-processing :enabled] false))
