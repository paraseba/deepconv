(ns deepconv.test.core
  (:use [deepconv.core] :reload)
  (:use [clojure.test]))

(deftest simple-conversions
  (testing "conversion-for-type"
    (let [int-conv (conversion-for-type java.lang.Integer (partial + 1))
          double-conv (conversion-for-type java.lang.Double (partial + 2.0))
          converter (make-converter int-conv double-conv)]
      (is (= (+ 1 3) (converter 3)))
      (is (= (+ 2.0 5.0) (converter 5.0))))
           ))
