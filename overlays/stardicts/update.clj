#! /usr/bin/env -S nix shell nixpkgs#babashka -c bb -f

(def threads 6)

(defn partition-to [n s]
  (loop [i n s (partition-all n s) acc []]
    (if (zero? i)
      acc
      (let [acc (conj acc (filterv #(not (nil? %)) (mapv first s)))]
        (recur (dec i) (map rest s) acc)))))

(defn npmap [n f s]
  (apply concat (pmap #(doall (map f %)) (partition-to n s))))

(defn url->hash [url]
  (-> (shell/sh "nix" "store" "prefetch-file" "--json" url)
      :out
      json/parse-string
      (get "hash")))

(defn url-object [url] {"url" url "hash" (url->hash url)})

(defn write-url-objects [input-file output-file]
  (let [urls (str/split-lines (slurp input-file))
        url-objects (npmap threads url-object urls)]
    (json/generate-stream url-objects (io/writer output-file) {:pretty true})))

(write-url-objects "./urls.txt" "./urls.json")
