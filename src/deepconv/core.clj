(ns deepconv.core)

(deftype Conversion [can-convert? convert])
(defn can-convert? [conversion value] ((.can-convert? conversion) value))
(defn convert [conversion value] ((.convert conversion) value))

(defn make-converter [& conversions]
  (fn [value]
    (if-let [conversion (some #(and (can-convert? % value) %) conversions)]
      (convert conversion value)
      value)))

(defn conversion-for-type [convert-type f]
  (Conversion. #(= convert-type (type %)) f))
