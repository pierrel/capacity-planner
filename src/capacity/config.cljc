(ns capacity.config)

(defn read [file-or-filename]
  (with-open [r (clojure.java.io/reader file-or-filename)]
    (clojure.edn/read (java.io.PushbackReader. r))))
