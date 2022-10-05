(defn update-current [result config]
  (assoc-in result ["metaData" "current"] (config "default")))

(defn update-engines [result config]
  (let [ff-engines (result "engines")
        engines (config "engines")]
    (assoc result "engines"
           (reduce-kv
            (fn [ff-engines name {keyword "keyword" url "url"}]
              (conj (filterv (fn [{n "_name" [k] "_definedAliases"}]
                               (not (or (= n name) (= k keyword))))
                             ff-engines)
                    {"_name" name "_definedAliases" [keyword] "_metaData" {}
                     "_urls" [{"params" [] "rels" [] "template" url}]}))
            ff-engines engines))))

(let [result (cheshire.core/parse-stream (java.io.BufferedReader. *in*))
      config (cheshire.core/parse-stream (clojure.java.io/reader
                                          (first *command-line-args*)))]
  (cheshire.core/generate-stream
   (-> result
       (update-current config)
       (update-engines config))
   (java.io.BufferedWriter. *out*))
  nil)
