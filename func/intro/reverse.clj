(let [lines (line-seq (java.io.BufferedReader. *in*))] (println (apply str (map #(str % "\n") ( (fn [lst]
     (loop [xs lst ys ()]
           (if (empty? xs) ys (recur (rest xs) (cons (first xs) ys)))))lines)))))
