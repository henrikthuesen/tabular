(ns tabular.test.core
  (:require [clojure.test :refer [deftest is]]
            [tabular.core :refer [tabularize]]))

(deftest simple
  (is (= "    1 2 3 \n" (tabularize ["a" "b" "c"] [{"a" "1", "b" "2", "c" "3"}])))
  (is (= "    1 2 3 \n" (tabularize [:a :b :c] [{:a "1", :b "2", :c "3"}]))))

(def complex-one
  [{"Id" "This is a long id", "Geo" "42", "Location" "Denmark"}
   {"Id" "small one", "Geo" "42", "Location" "Denmark"}
   {"Id" "small one\nLines\nand a long one here, that is",
    "Geo" "42",
    "Location" "Denmark\nSOme other country"}])

(deftest complex
  (is
   (=
    "    This is a long id            42 Denmark            \n    small one                    42 Denmark            \n    small one                    42 Denmark            \n    Lines                           SOme other country \n    and a long one here, that is                       \n"
    (tabularize ["Id" "Geo" "Location"]
                complex-one))))

(deftest with-headers
  (is (= "    a b c \n    - - - \n    1 2 3 \n" (tabularize ["a" "b" "c"] [{"a" "1", "b" "2", "c" "3"}] :headers true)))
  (is (= "    a b c \n    - - - \n    1 2 3 \n" (tabularize [:a :b :c] [{:a "1", :b "2", :c "3"}] :headers true)))
)
