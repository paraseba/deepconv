(ns deepconv.test.core
  (:use [deepconv.core] :reload)
  (:use [clojure.test]))

(deftest simple-conversions
  (let [int-conv (conversion-for-type java.lang.Integer (partial + 1))
        double-conv (conversion-for-type java.lang.Double (partial + 2.0))]

    (testing "conversion-for-type"
      (let [converter (make-converter int-conv double-conv)]
        (is (= (+ 1 3) (converter 3)))
        (is (= (+ 2.0 5.0) (converter 5.0)))))

    (testing "vector-conversion"
      (let [converter (make-converter int-conv double-conv (vector-conversion))]
        (is (= (+ 1 3) (converter 3)))
        (is (= [(+ 1 3) (+ 2.0 5.0)] (converter [3 5.0])))))

    (testing "list-conversion"
      (let [converter (make-converter int-conv double-conv (list-conversion))]
        (is (= (+ 1 3) (converter 3)))
        (is (= (list (+ 1 3) (+ 2.0 5.0)) (converter (list 3 5.0))))))

    (testing "map-conversion"
      (let [converter (make-converter int-conv double-conv (map-conversion))]
        (is (= (+ 1 3) (converter 3)))
        (is (= {(+ 1 3) (+ 2.0 5.0)} (converter {3 5.0})))))))

(deftest mixed-conversions
  (let [int-conv (conversion-for-type java.lang.Integer (partial + 1))
        double-conv (conversion-for-type java.lang.Double (partial + 2.0))
        conversions [int-conv double-conv (vector-conversion)
                     (list-conversion) (map-conversion)]
        converter (apply make-converter conversions)
        four (+ 1 3)
        seven (+ 2.0 5.0)]
    (is (= four (converter 3)))
    (is (= seven (converter 5.0)))
    (is (= [four seven] (converter [3 5.0])))
    (is (= (list four seven) (converter (list 3 5.0))))
    (is (= {four seven} (converter {3 5.0})))
    (is (= {four seven} (converter {3 5.0})))
    (is (= [four [seven] [[(list four)]] [{[] (list seven)}]]
           (converter [3 [5.0] [[(list 3)]] [{[] (list 5.0)}]])))
    (is (= {[four seven] {four seven}, four seven}
           (converter {[3 5.0] {3 5.0}, 3 5.0})))))
