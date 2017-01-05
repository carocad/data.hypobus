(ns hypobus.basic-test
  (:require [clojure.test :as test]
            [frechet-dist.core :as frechet]
            [frechet-dist.protocols :as frepos]
            [frechet-dist.sampler :as sampler]
            [hypobus.basics.geometry :as geo]
            [hypobus.conjectures.core :as hypo]
            [hypobus.simulation.faker :as faker]))

;******************************* TEST DATA *******************************;
;; data manually created based on a mapbox route created manually
(def connatra-trace [[-75.578556,6.254853],[-75.586194,6.258479],[-75.584177,6.262275],[-75.586495,6.265091],[-75.587568,6.270167],[-75.5904,6.272129],[-75.590271,6.272727],[-75.591173,6.272513],[-75.592546,6.27038],[-75.59452,6.27294],[-75.593919,6.273537],[-75.593147,6.273324],[-75.594906,6.27085],[-75.59658,6.264238]])

(def max-dist 40); meters
(def num-subcurves 30) ; amount of subcurves
(def min-span 0.3); minimum percentage of the original curve
(def normal-noise 0.0001); degrees. Standard deviation of the location
(def burst-noise 0.01); degrees. standard deviation of the burst noise
(def num-vandals 3)

(def coordinates (sampler/refine connatra-trace max-dist frepos/distance))
(def hypocurve (mapv (fn [[lon lat]] (geo/->HypoPoint lon lat geo/MIN-WEIGHT geo/MAX-DISTRUST))
                    coordinates))
(def subcurves (faker/subcurves hypocurve num-subcurves min-span))
(def noisy-curves (map #(faker/add-noise normal-noise %) subcurves))
(def spam-curves  (faker/bandal-curves num-vandals coordinates))
(def burst-curves (faker/add-burst burst-noise (last noisy-curves)))
(def traces (shuffle (concat (butlast noisy-curves) spam-curves (vector burst-curves))))

(def foo (hypo/conjectures traces))
(sort-by first (map #(vector (hypo/avg-distrust %) %) foo))

 ;; TODO: make a real test using the test data
(require '[proto-repl-charts.charts])
(require '[proto-repl-charts.table])

(proto-repl-charts.charts/custom-chart
  "coordinates2"
  {:data {:xs {"lat" "lon"} ;; y x axis
          :columns [(cons "lat" (map second coordinates))
                    (cons "lon" (map first coordinates))]
          :type "scatter"}})

(proto-repl-charts.charts/custom-chart
  "hypos"
  {:data {:xs {"hypo-lat" "hypo-lon"
               "lat" "lon"};; y x axis
          :columns [(cons "hypo-lat" (map :lat (nth foo 0)))
                    (cons "hypo-lon" (map :lon (nth foo 0)))
                    (cons "lat" (map second coordinates))
                    (cons "lon" (map first coordinates))]
          :type "scatter"}})


(test/deftest hypothize
  (let [connatra-trace [[-75.578556,6.254853],[-75.586194,6.258479],[-75.584177,6.262275],[-75.586495,6.265091],[-75.587568,6.270167],[-75.5904,6.272129],[-75.590271,6.272727],[-75.591173,6.272513],[-75.592546,6.27038],[-75.59452,6.27294],[-75.593919,6.273537],[-75.593147,6.273324],[-75.594906,6.27085],[-75.59658,6.264238]]
        max-dist      40; meters
        num-subcurves 10 ; amount of subcurves
        min-span      0.3; minimum percentage of the original curve
        normal-noise  0.0001; degrees. Standard deviation of the location
        burst-noise   0.01; degrees. standard deviation of the burst noise
        num-vandals   1

        coordinates   (sampler/refine connatra-trace max-dist frepos/distance)
        subcurves     (faker/subcurves coordinates num-subcurves min-span)
        geo-subcurves (map (fn [s] (map #(zipmap [:lon :lat] %) s))
                           subcurves)

        noisy-curves  (map #(faker/add-noise normal-noise %) geo-subcurves)
        spam-curves   (faker/bandal-curves num-vandals coordinates)
        burst-curves  (faker/add-burst burst-noise (last noisy-curves))
        traces        (shuffle (concat (butlast noisy-curves)
                                       spam-curves
                                       (vector burst-curves)))]
   (test/is (not (nil? traces))))
 (test/is (= 4 (+ 2 2)))
 (test/is (= 7 (+ 3 4))))

;;(test/run-tests 'hypobus.basic-test)
