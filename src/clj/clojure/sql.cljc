(ns clojure.sql
  "Portable SQL/database operations.

   This namespace provides platform-independent database operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.sql namespaces.

   Maps to:
   - JVM: java.sql.* (JDBC)
   - PHP: PDO
   - JS: better-sqlite3, pg, mysql2, etc.")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol IConnection
  "A database connection."
  (-close [conn] "Close the connection")
  (-closed? [conn] "Returns true if connection is closed")
  (-commit [conn] "Commit current transaction")
  (-rollback [conn] "Rollback current transaction")
  (-auto-commit? [conn] "Returns true if auto-commit is enabled")
  (-set-auto-commit [conn auto] "Set auto-commit mode"))

(defprotocol IStatement
  "A prepared statement."
  (-execute! [stmt] [stmt params] "Execute the statement")
  (-query [stmt] [stmt params] "Execute query and return results")
  (-close-statement [stmt] "Close the statement"))

(defprotocol IResultSet
  "A result set from a query."
  (-next-row [rs] "Move to next row, returns true if there is one")
  (-get-value [rs col] "Get value at column (by name or index)")
  (-get-columns [rs] "Get column names")
  (-row-map [rs] "Get current row as a map")
  (-close-result-set [rs] "Close the result set"))

(defprotocol SQLEngine
  "SQL operations engine.
   Backends must provide an implementation of this protocol."

  ;; Connection
  (-connect [e url] [e url opts]
    "Connect to database. opts may include :user :password etc.")
  (-connection-from-datasource [e datasource]
    "Get connection from a datasource/pool")

  ;; Statements
  (-prepare [e conn sql]
    "Prepare a statement")
  (-prepare-call [e conn sql]
    "Prepare a callable statement (stored procedure)")

  ;; Direct execution
  (-execute-one! [e conn sql] [e conn sql params]
    "Execute SQL, return affected row count")
  (-query-one [e conn sql] [e conn sql params]
    "Execute query, return seq of row maps")

  ;; Transactions
  (-with-transaction [e conn opts f]
    "Execute f within a transaction. opts: :isolation :read-only")

  ;; Metadata
  (-get-metadata [e conn]
    "Get database metadata"))

;;; ====================================================================
;;; Dynamic vars
;;; ====================================================================

(def ^:dynamic *sql-engine* nil)
(def ^:dynamic *connection* nil)

;;; ====================================================================
;;; Connection
;;; ====================================================================

(defn connect
  "Connects to a database.
   url is a connection string (JDBC URL, DSN, etc.)
   opts may include :user :password and driver-specific options."
  ([url]
   (-connect *sql-engine* url))
  ([url opts]
   (-connect *sql-engine* url opts)))

(defn close
  "Closes a connection, statement, or result set."
  [closeable]
  (cond
    (satisfies? IConnection closeable) (-close closeable)
    (satisfies? IStatement closeable) (-close-statement closeable)
    (satisfies? IResultSet closeable) (-close-result-set closeable)))

(defn closed?
  "Returns true if the connection is closed."
  [conn]
  (-closed? conn))

;;; ====================================================================
;;; Execution
;;; ====================================================================

(defn execute!
  "Executes SQL statement. Returns number of affected rows.
   params is a vector of parameter values for prepared statement."
  ([conn sql]
   (-execute-one! *sql-engine* conn sql))
  ([conn sql params]
   (-execute-one! *sql-engine* conn sql params)))

(defn query
  "Executes a query and returns a lazy seq of row maps.
   params is a vector of parameter values for prepared statement."
  ([conn sql]
   (-query-one *sql-engine* conn sql))
  ([conn sql params]
   (-query-one *sql-engine* conn sql params)))

;;; ====================================================================
;;; Prepared Statements
;;; ====================================================================

(defn prepare
  "Creates a prepared statement."
  [conn sql]
  (-prepare *sql-engine* conn sql))

(defn execute-statement!
  "Executes a prepared statement."
  ([stmt]
   (-execute! stmt))
  ([stmt params]
   (-execute! stmt params)))

(defn query-statement
  "Executes a prepared statement as a query."
  ([stmt]
   (-query stmt))
  ([stmt params]
   (-query stmt params)))

;;; ====================================================================
;;; Result Sets
;;; ====================================================================

(defn result-set-seq
  "Returns a lazy seq of row maps from a result set."
  [rs]
  (lazy-seq
   (when (-next-row rs)
     (cons (-row-map rs) (result-set-seq rs)))))

(defn columns
  "Returns the column names from a result set."
  [rs]
  (-get-columns rs))

;;; ====================================================================
;;; Transactions
;;; ====================================================================

(defmacro with-transaction
  "Executes body within a transaction.
   binding is [conn-sym conn-expr] or [conn-sym conn-expr opts].
   If body completes normally, commits. If exception, rolls back."
  [binding & body]
  (let [conn-sym (first binding)
        conn-expr (second binding)
        opts (if (> (count binding) 2) (nth binding 2) {})]
    `(-with-transaction *sql-engine* ~conn-expr ~opts
                        (fn [~conn-sym] ~@body))))

(defn commit
  "Commits the current transaction."
  [conn]
  (-commit conn))

(defn rollback
  "Rolls back the current transaction."
  [conn]
  (-rollback conn))

;;; ====================================================================
;;; Convenience
;;; ====================================================================

(defn insert!
  "Inserts a row into table. row is a map of column->value."
  [conn table row]
  (let [cols (keys row)
        vals (vals row)
        placeholders (repeat (count cols) "?")
        sql (str "INSERT INTO " (name table)
                 " (" (clojure.string/join ", " (map name cols)) ")"
                 " VALUES (" (clojure.string/join ", " placeholders) ")")]
    (execute! conn sql (vec vals))))

(defn update!
  "Updates rows in table. set-map is column->value, where-clause is SQL."
  [conn table set-map where-clause & params]
  (let [cols (keys set-map)
        vals (vals set-map)
        set-sql (clojure.string/join ", " (map #(str (name %) " = ?") cols))
        sql (str "UPDATE " (name table) " SET " set-sql " WHERE " where-clause)]
    (execute! conn sql (vec (concat vals params)))))

(defn delete!
  "Deletes rows from table matching where-clause."
  [conn table where-clause & params]
  (let [sql (str "DELETE FROM " (name table) " WHERE " where-clause)]
    (execute! conn sql (vec params))))

(defn find-by-id
  "Finds a single row by primary key."
  [conn table id & {:keys [pk] :or {pk :id}}]
  (first (query conn
                (str "SELECT * FROM " (name table) " WHERE " (name pk) " = ?")
                [id])))
