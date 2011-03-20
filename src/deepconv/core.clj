(ns deepconv.core
  "Generic conversion of values. Provides conversions based on type, and
  nested conversions for lists, vectors & maps")

; Implement a generic conversion.
; can-convert? is a function that takes a value and returns true if it
; can be handled by this conversion
; convert implements the actual conversion, it receives the value to
; convert and a function representing other converters to handle nested values
(deftype Conversion [can-convert? convert])

(defn- can-convert?
  "Calls can-convert? member of a conversion"
  [conversion value]
  ((.can-convert? conversion) value))

(defn- convert
  "Calls convert member of a conversion"
  [conversion value others]
  ((.convert conversion) value others))

(defn make-converter
  "Create a converter function given a series of conversions. The function
  returned takes a single argument to be converted"
  [& conversions]
  (fn conv [value]
    (if-let [conversion (some #(and (can-convert? % value) %) conversions)]
      (convert conversion value conv)
      value)))

(defn conversion-for-type
  "Creates a conversion based on type iguality.
  convert-type is the type handled and f is a function that takes a value
  of type convert-type and returns the converted value"
  [convert-type f]
  (Conversion. #(= convert-type (type %))
               (fn [v o]
                 (f v))))

(def ^{:doc "Returns a conversion for vectors. All elements are converted by
            other conversions, a vector is returned"}
  vector-conversion
  (Conversion. vector?
               (fn [v o]
                 (vec (map o v)))))

(def ^{:doc "Returns a conversion for lists. All elements are converted by
            other conversions, a list is returned"}
  list-conversion
  (Conversion. list?
               (fn [v o]
                 (list* (map o v)))))

(def ^{:doc "Returns a conversion for maps. All elements are converted by
            other conversions, a map is returned"}
  map-conversion
  (Conversion. map?
               (fn [v o]
                 (reduce
                   (fn [res [k v]]
                     (assoc res (o k) (o v)))
                   {}
                   v))))
