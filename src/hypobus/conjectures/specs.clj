(ns hypobus.conjectures.specs
  (:require [clojure.spec :as s]
            [hypobus.conjectures.core]))

(s/def ::dist  (s/and number? pos?))
(s/def ::index (s/and integer? (comp not neg?)))
(s/def ::similar? boolean?) ;; TODO: remove this from the original function

(s/def ::lat (s/and number? #(<= -90 % 90)))
(s/def ::lon (s/and number? #(<= -180 % 180)))
(s/def ::weight (s/and number? pos?))
(s/def ::distrust (s/and number? #(<= 0 % 1)))

(s/def ::geo-point  (s/keys :req-un [::lat ::lon]))
(s/def ::hypo-point (s/keys :req-un [::lat ::lon ::weight ::distrust]))

(s/def ::geo-curve  (s/coll-of ::geo-point  :kind sequential? :min-count 2))
(s/def ::hypo-curve (s/coll-of ::hypo-point :kind sequential? :min-count 2))

(s/def ::hypothesis (s/nilable (s/coll-of ::hypo-curve)))
;;TODO: ::couple doesnt take into account the order of the indexes
(s/def ::couple (s/coll-of (s/coll-of ::index :kind sequential? :count 2)))
(s/def ::full-coupling (s/coll-of (s/coll-of (s/nilable ::index)
                                             :kind sequential?
                                             :count 2)))

(s/fdef hypobus.conjectures.core/hypothize
  :args (s/cat :hypos ::hypothesis :trace ::hypo-curve)
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.core/conjectures
  :args (s/alt :traces ::hypothesis
               :trace-sets (s/cat :trace-1 ::hypothesis
                                  :trace-2 ::hypothesis))
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.core/recombine
  :args (s/cat :hypos (s/nilable ::hypothesis))
  :ret (s/nilable ::hypothesis))

(s/fdef hypobus.conjectures.route/similarity
  :args (s/cat :P ::hypo-curve
               :Q ::hypo-curve)
  :ret (s/keys :req-un [::dist ::couple ::similar?]))

(s/fdef hypobus.conjectures.route/full-coupling
  :args (s/cat :P ::hypo-curve
               :Q ::hypo-curve
               :coupling ::couple)
  :ret ::couple)

(s/fdef hypobus.conjectures.route/fuse
  :args (s/alt :points (s/cat :p1 ::hypo-point :p2 ::hypo-point)
               :curves (s/cat :P ::hypo-curve :Q ::hypo-curve :coupling ::couple))
  :ret ::hypo-curve)

(s/fdef hypobus.basics.geometry/haversine
  :args (s/cat :p1 ::geo-point :p2 ::geo-point)
  :ret ::dist)
