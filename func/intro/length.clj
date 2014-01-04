(let [lines (line-seq (java.io.BufferedReader. *in*))] (println ( (fn[lst]
    (loop [n 0 xs lst]
          (if (empty? xs) n (recur (+ n 1) (rest xs)))))lines)))
