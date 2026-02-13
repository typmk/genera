(ns clojure.http
  "HTTP client.")

;; Portable HTTP - maps to platform HTTP libraries

(defn request
  "Performs an HTTP request. Returns a map with:
   :status - HTTP status code
   :headers - response headers map
   :body - response body (string or bytes)

   Options:
   :method - :get :post :put :delete :patch :head :options (default :get)
   :headers - request headers map
   :body - request body (string, bytes, or input stream)
   :query-params - map of query parameters
   :form-params - map of form parameters (application/x-www-form-urlencoded)
   :content-type - content type for body
   :accept - accept header
   :as - :string (default), :bytes, :stream
   :timeout - timeout in milliseconds
   :follow-redirects - if true (default), follow redirects
   :basic-auth - [user password] for basic authentication"
  [url & [opts]]
  (throw (UnsupportedOperationException. "clojure.http/request not yet implemented")))

(defn get
  "Performs a GET request."
  [url & [opts]]
  (request url (assoc opts :method :get)))

(defn post
  "Performs a POST request."
  [url & [opts]]
  (request url (assoc opts :method :post)))

(defn put
  "Performs a PUT request."
  [url & [opts]]
  (request url (assoc opts :method :put)))

(defn delete
  "Performs a DELETE request."
  [url & [opts]]
  (request url (assoc opts :method :delete)))

(defn patch
  "Performs a PATCH request."
  [url & [opts]]
  (request url (assoc opts :method :patch)))

(defn head
  "Performs a HEAD request."
  [url & [opts]]
  (request url (assoc opts :method :head)))

(defn options
  "Performs an OPTIONS request."
  [url & [opts]]
  (request url (assoc opts :method :options)))

;; Utilities

(defn url-encode
  "URL-encodes a string."
  [s]
  (throw (UnsupportedOperationException. "clojure.http/url-encode not yet implemented")))

(defn url-decode
  "URL-decodes a string."
  [s]
  (throw (UnsupportedOperationException. "clojure.http/url-decode not yet implemented")))

(defn form-encode
  "Encodes a map as application/x-www-form-urlencoded."
  [params]
  (throw (UnsupportedOperationException. "clojure.http/form-encode not yet implemented")))

(defn form-decode
  "Decodes application/x-www-form-urlencoded data to a map."
  [s]
  (throw (UnsupportedOperationException. "clojure.http/form-decode not yet implemented")))
