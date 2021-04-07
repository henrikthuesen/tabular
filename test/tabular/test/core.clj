(ns tabular.test.core
  (:use [clojure.test]
        [tabular.core]))

(deftest simple
  (is (= "    1 2 3 \n" (tabularize ["a" "b" "c"] [{"a" "1", "b" "2", "c" "3"}]))))
