(ns test
  (:require  [clojure.test :as t]))

(defn test-file? [file]
  (and (.isFile file)
       (re-matches #".*\.clj[^.]*$" (.getPath file))))

(defn load-return-failures [files]
  (doseq [file files]
    (try
      (load-file (.getPath file))
      (catch Exception e (println "Coult not load file" (.getPath file) "," e)))))

(defn -main [& args]
  (let [dir-name "test"
        directory (clojure.java.io/file dir-name)
        files (filter test-file?
                      (file-seq directory))]
    (load-return-failures files)
    (System/exit (if (t/successful? (t/run-all-tests))
                    0
                    1))))

