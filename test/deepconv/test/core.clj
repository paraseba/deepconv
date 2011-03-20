(ns deepconv.test.core
  (:use [deepconv.core] :reload)
  (:use [clojure.test]))

(deftest simple-conversions
  (let [int-conv (conversion-for-type java.lang.Integer (partial + 1))
        double-conv (conversion-for-type java.lang.Double (partial + 2.0))]

    (testing "conversion-for-type"
      (let [convert (make-converter int-conv double-conv)]
        (is (= (+ 1 3) (convert 3)))
        (is (= (+ 2.0 5.0) (convert 5.0)))))

    (testing "vector-conversion"
      (let [convert (make-converter int-conv double-conv vector-conversion)]
        (is (= (+ 1 3) (convert 3)))
        (is (= [(+ 1 3) (+ 2.0 5.0)] (convert [3 5.0])))))

    (testing "list-conversion"
      (let [convert (make-converter int-conv double-conv list-conversion)]
        (is (= (+ 1 3) (convert 3)))
        (is (= (list (+ 1 3) (+ 2.0 5.0)) (convert (list 3 5.0))))))

    (testing "map-conversion"
      (let [convert (make-converter int-conv double-conv map-conversion)]
        (is (= (+ 1 3) (convert 3)))
        (is (= {(+ 1 3) (+ 2.0 5.0)} (convert {3 5.0})))))))

(deftest mixed-conversions
  (let [int-conv (conversion-for-type java.lang.Integer (partial + 1))
        double-conv (conversion-for-type java.lang.Double (partial + 2.0))
        conversions [int-conv double-conv
                     vector-conversion list-conversion map-conversion]
        convert (apply make-converter conversions)
        four (+ 1 3)
        seven (+ 2.0 5.0)]
    (is (= four (convert 3)))
    (is (= seven (convert 5.0)))
    (is (= [four seven] (convert [3 5.0])))
    (is (= (list four seven) (convert (list 3 5.0))))
    (is (= {four seven} (convert {3 5.0})))
    (is (= {four seven} (convert {3 5.0})))
    (is (= [four [seven] [[(list four)]] [{[] (list seven)}]]
           (convert [3 [5.0] [[(list 3)]] [{[] (list 5.0)}]])))
    (is (= {[four seven] {four seven}, four seven}
           (convert {[3 5.0] {3 5.0}, 3 5.0})))))
