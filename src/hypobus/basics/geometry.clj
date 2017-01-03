(ns hypobus.basics.geometry
  (:require [frechet-dist.core :as frechet]
            [hypobus.utils.tool :as tool]))

;; TODO: most of these declarations should be dynamic at some point
(def MAX-DISTRUST 1.0)
(def MIN-WEIGHT (/ 1 (* 100 100))); 100 meters radious as deviation
(def RADIOUS 6372800); radious of the Earth in meters

(def MIN-DIST 30); meters
(def MAX-DIST 100); meters
(def MAX-GAP  300); meters

;; TODO: Do I really need the type hints here even after I put them
;;      on the record ?
(defn- haversine-radians
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in meters
  by default."
  [^double lat-1 ^double lon-1 ^double lat-2 ^double lon-2]
  (let [h  (+ (Math/pow (Math/sin (/ (- lat-2 lat-1) 2)) 2)
              (* (Math/pow (Math/sin (/ (- lon-2 lon-1) 2)) 2)
                 (Math/cos lat-2)
                 (Math/cos lat-1)))]
    (* RADIOUS 2 (Math/asin (Math/sqrt h)))))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in DEGREES. The distance is computed in meters
  by default. This function takes two hash-maps as argument with keys :lat :lon"
  [p1 p2]
  (haversine-radians (Math/toRadians (:lat p1)) (Math/toRadians (:lon p1))
                     (Math/toRadians (:lat p2)) (Math/toRadians (:lon p2))))

; (haversine [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106
; distance between paris and san francisco
; (* (haversine [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defprotocol GeoDistance
  (distance [object-1 object-2]
            [object-1 object-2 dist-fn]
            "computes the distance between two geometric objects (points, curves)"))

(defrecord HypoPoint [^double lat
                      ^double lon
                      ^double weight
                      ^double distrust])

(extend-protocol GeoDistance
  clojure.lang.PersistentArrayMap                           ;; point as hash-map
  (distance ([point-1 point-2]   (haversine point-1 point-2))
            ([point-1 point-2 f] (f point-1 point-2)))
  HypoPoint                                         ;; point as HypoPoint record
  (distance ([point-1 point-2]   (haversine point-1 point-2))
            ([point-1 point-2 f] (f point-1 point-2)))
  clojure.lang.PersistentVector                        ;; point as lat lon tuple
  (distance ([[lat lon] [lat2 lon2]]
             (haversine-radians (Math/toRadians lat) (Math/toRadians lon)
                                (Math/toRadians lat2) (Math/toRadians lon2)))
            ([point-1 point-2 f]
             (f point-1 point-2)))
  clojure.lang.Sequential                         ;; curve as sequence of points
  (distance ([coll coll2]   (frechet/partial-distance coll coll2 haversine))
            ([coll coll2 f] (f coll coll2))))

(defn gaps
  "returns a reducing function to use with partition-by such that a curve can
  be partitioned into several onees if the distance between two consecutive
  points is greater than max-gap. f is the function used to calculate the
  distance between two points in the curve. f defaults to the haversine and
  max-gap defaults to hypobus.basics.geometry/MAX-GAP"
  ([]
   (gaps MAX-GAP haversine))
  ([max-gap]
   (gaps max-gap haversine))
  ([max-gap f]
   (let [prior (volatile! 1)]
     (fn [value]
       (if (> max-gap (f @prior value))
        (do (vreset! prior value) nil)
        (vreset! prior value))))))
;; example
;; (partition-by (gaps 2 #(Math/abs (- %1 %2))) [2 3 4 7 6 1])

(defn- interpolate
  "returns n interpolated points between mi (inclusive) and mj (exclusive)
  where mi and mj are both hash-maps with the same shape"
  [mi mj n]
  (let [delta (map / (map - (vals mj) (vals mi))
                     (repeat n))  ; size of each interval
        pdelta (zipmap (keys mi) delta)]
    (for [i (range n)]
      (merge-with + mi (tool/update-vals pdelta #(* % i))))))
;; example
;; (interpolate {:a 0 :b 0} {:a 5 :b 5} 3)

(defn tidy
  "tidy up a curve such that the distance between two points is not smaller
  than min-dist and not greater than max-dist. f is the function used to
  calculate the distance between two points in the curve. min-dist and max-dist
  default to hypobus.basics.geometry/MIN-DIST and hypobus.basics.geometry/MAX-DIST
  respectively"
  ([f hypocurve]
   (tidy MIN-DIST MAX-DIST f hypocurve))
  ([min-dist max-dist f hypocurve]
   (let [pij-dist (map f hypocurve (rest hypocurve))
         judge    (fn [index dist]
                    (cond
                      (> dist max-dist) (interpolate (nth hypocurve index)
                                                     (nth hypocurve (inc index))
                                                     (Math/ceil (/ dist max-dist)))
                      (< dist min-dist) nil ; prepare for filtering
                      :else (list (nth hypocurve index)))) ; OK point between the limits
         sampler  (comp (map-indexed judge) (remove nil?) (mapcat identity))
         new-coll (into [] sampler pij-dist)]
     (conj new-coll (last hypocurve)))))
