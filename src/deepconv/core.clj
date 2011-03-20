(ns deepconv.core)

(deftype Conversion [can-convert? convert])
(defn can-convert? [conversion value] ((.can-convert? conversion) value))
(defn convert [conversion value others] ((.convert conversion) value others))

(defn make-converter [& conversions]
  (fn conv [value]
    (if-let [conversion (some #(and (can-convert? % value) %) conversions)]
      (convert conversion value conv)
      value)))

(defn conversion-for-type [convert-type f]
  (Conversion. #(= convert-type (type %))
               (fn [v o]
                 (f v))))

(defn vector-conversion []
  (Conversion. vector?
               (fn [v o]
                 (vec (map o v)))))

(defn list-conversion []
  (Conversion. list?
               (fn [v o]
                 (list* (map o v)))))

(defn map-conversion []
  (Conversion. map?
               (fn [v o]
                 (reduce
                   (fn [res [k v]]
                     (assoc res (o k) (o v)))
                   {}
                   v))))
