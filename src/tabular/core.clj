(ns tabular.core
  (:require [clojure.string :refer (trim join split split-lines)]))

(defn- split-lines-trimmed
  [s]
  (->> s
       split-lines
       (map trim)))

(defn- str-size
  [s]
  (cond
    (nil? s) 0
    (not (string? s)) (str-size (str s))
    (.contains s "\n") (apply max (map #(.length %) (split-lines-trimmed s)))
    true (.length s)))

(defn- safe-max
  [& args]
  (apply max (remove nil? args)))


(defn- pad
  [n]
  (apply str (take n (repeat " "))))

;; Lots of stuff to do here
(defn tabularize
  "Given the keys and the maps prints out a table."
  [ks db &
   {:keys [headers indent header-underline],
    :or {header false,
         indent "    ",
         header-underline "-"}}]
  (with-out-str
   (let [sizes (reduce (fn [sizes row] ;; map from keys in the db to sizes
                         (reduce (fn [sizes key]
                                   (update sizes key safe-max (str-size (get row key))))
                                 sizes
                                 ks))
                       {}
                       db)
         line-in (fn [value line-number] ;; gets the line-number in value
                   (if (nil? value)
                     nil
                     (nth (split-lines-trimmed value) line-number nil)))
         print-with-padding (fn [[key column-line]]
                              (let [actual-size (get sizes key)
                                    size (if (nil? column-line)
                                           0
                                           (.length column-line))
                                    to-pad (- actual-size size)]
                                (if (nil? column-line)
                                  (print (str (pad to-pad) " "))
                                  (print (str column-line (pad to-pad) " ")))))]
     (when headers
       (do
         (print indent)
         (run! print-with-padding (zipmap ks ks))
         (println)
         (print indent)
         (run! print-with-padding
               (zipmap ks
                       (map (fn [key]
                              (apply str
                                     (take (count key)
                                           (repeat header-underline))))
                            ks)))
         (println)))
     (loop [line-in-row 0
            row-number 0] ;; points into the row in the db
       (when (< row-number (count db))
         (let [row (get db row-number)
               current-lines (map (fn [key] (line-in (get row key) line-in-row)) ks)]
           (if (every? nil? current-lines)
             (do
               ;(println)
               (recur 0 (inc row-number))) ; next row
             (do
               (print indent)
               (run! print-with-padding (zipmap ks current-lines))
               (println)
               (recur (inc line-in-row) row-number)))))))))
