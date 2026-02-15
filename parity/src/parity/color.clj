(ns parity.color)

(def ^:dynamic *color* true)

(defn ansi [code s] (if *color* (str "\033[" code "m" s "\033[0m") s))
(defn bold    [s] (ansi "1" s))
(defn dim     [s] (ansi "2" s))
(defn green   [s] (ansi "32" s))
(defn yellow  [s] (ansi "33" s))
(defn cyan    [s] (ansi "36" s))
(defn red     [s] (ansi "31" s))
(defn white   [s] (ansi "37" s))

(defn heading [s]
  (let [line (apply str (repeat 72 "─"))]
    (str "\n" (bold (cyan s)) "\n" (dim line))))

(defn sub-heading [s]
  (bold (white s)))
