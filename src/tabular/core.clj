(ns tabular.core
  (:require [clojure.string :refer (trim split-lines)]))


(defn- str-size
  [s split-lines-trimmed]
  (cond
    (nil? s) 0
    (not (string? s)) (str-size (str s) split-lines-trimmed)
    (.contains s "\n") (apply max (map #(.length %) (split-lines-trimmed s)))
    :else (.length s)))

(defn- safe-max
  [& args]
  (apply max (remove nil? args)))

(defn- pad
  "Right pad..."
  [n]
  (apply str (take n (repeat " "))))

(defn- column-sizes
  "Calculates a map from keys in the db to the width of the column."
  [ks db split-lines-trimmed]
  (reduce (fn [sizes row]
            (reduce (fn [sizes key]
                      (update sizes key safe-max (str-size (get row key) split-lines-trimmed)))
                    sizes
                    ks))
          {}
          db))


;; Lots of stuff to do here
(defn tabularize
  "Given the keys and the maps prints out a table."
  [ks db &
   {:keys [headers indent header-underline row-spacer],
    :or {headers false,
         indent "    ",
         header-underline "-",
         row-spacer false}}]
  (with-out-str
   (let [split-lines-trimmed (fn [^String s]
                               (->> s
                                    split-lines
                                    (map trim)))
         split-lines-trimmed (memoize split-lines-trimmed)
         sizes (column-sizes ks db split-lines-trimmed)
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
       (let [ks-strs (map (fn [header]
                            (if (keyword? header)
                              (name header)
                              header))
                          ks)]

         (print indent)
         (run! print-with-padding (zipmap ks ks-strs))
         (println)
         (print indent)
         (run! print-with-padding
               (zipmap ks
                       (map (fn [key]
                              (apply str
                                     (take (count key)
                                           (repeat header-underline))))
                            ks-strs)))
         (println)))
     (loop [line-in-row 0
            row-number 0] ;; points into the row in the db
       (when (< row-number (count db))
         (let [row (get db row-number)
               current-lines (map (fn [key] (line-in (get row key) line-in-row)) ks)]
           (if (every? nil? current-lines)
             (do
               (when row-spacer
                 (println))
               (recur 0 (inc row-number))) ; go to next row
             (do
               (print indent)
               (run! print-with-padding (zipmap ks current-lines))
               (println)
               (recur (inc line-in-row) row-number)))))))))
