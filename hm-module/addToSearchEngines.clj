(defn update-engines [result engines]
  (let [ff-engines (result "engines")]
    (assoc result "engines"
           (reduce-kv
            (fn [ff-engines name {keyword "keyword" url "url"}]
              (conj (filterv (fn [{n "_name" [k] "_definedAliases"}]
                               (not (or (= n name) (= k keyword))))
                             ff-engines)
                    {"_name" name "_definedAliases" [keyword] "_metaData" {}
                     "_urls" [{"params" [] "rels" [] "template" url}]}))
            ff-engines engines))))

(let [result (json/parse-stream (java.io.BufferedReader. *in*))
      engines (json/parse-stream (io/reader (first *command-line-args*)))]
  (json/generate-stream (update-engines result engines)
                        (java.io.BufferedWriter. *out*)))
