(ns opengl.objects
  [:require
   [opengl.core :as core]
   [opengl.geom :as geom]
   [opengl.util :as util]
   [opengl.b3d :as b3d]
   [opengl.route :as route]]
  (:gen-class))

(defn- create-track-transforms-in-block-with-direction
  [block direction]
  (let [[dx dy] direction
        track-yaw (Math/atan2 dx dy)]
    (assoc block
      :direction [dx dy]
      :track-transform
      (geom/transform-create track-yaw 0.0 0.0)
      :ground-transform
      (geom/transform-create track-yaw 0.0 0.0))))

(defn- create-initial-transforms-in-block [block]
  (merge
   {:track-transform geom/identity-transform
    :direction [0.0 1.0]
    :position [0.0 0.0 0.0]
    :curve 0.0
    :turn 0.0
    :height 0.0
    :pitch 0.0}
   block))

(defn- update-position-in-block [prev-block block block-length]
  (if prev-block
    (let [[x y z] (:position prev-block)
         [dx dz] (:direction prev-block)
         curve (or (:curve block) (:curve prev-block) 0.0)
         turn (or (:turn block) (:turn prev-block) 0.0)
         pitch (or (:pitch block) (:pitch prev-block) 0.0)
         height (or (:height block) (:height prev-block) 0.0)]
      (assoc block
        :position [(+ (* dx block-length) x) y (+ (* dz block-length) z)]
        :direction (:direction prev-block)
        :turn turn
        :curve curve
        :pitch pitch
        :height height))
    (create-initial-transforms-in-block block)))

(defn- create-track-transforms-in-block [prev-block block]
  (let [direction (:direction block)
        curves (:curve block)
        radius (:radius curves)
        cant (:cant curves)
        block-length (- (:end-ref block) (:start-ref block))]
    (if (or (nil? radius) (= radius 0.0))
      (create-track-transforms-in-block-with-direction
        block direction)
      (let [b (/ block-length (Math/abs radius))
            c (Math/sqrt (* 2.0 radius radius (- 1.0 (Math/cos b))))
            a (* -1.0 0.5 (Math/signum radius) b)]
        (create-track-transforms-in-block-with-direction
          block
          (geom/rotate-vector-2d
           direction (Math/cos a) (Math/sin a)))))))

(defn- create-transforms-for-player-rail [block]
  {:planar 0.0
   :updown 0.0
   :position (:position block)
   :direction (:direction block)
   :rail-transform (:track-transform block)})

(defn- create-transforms-for-rail
  [block next-block rail]
  (let [[start-x start-y] (:start rail)
        [end-x end-y] (:end rail)

        direction (:direction block)
        position (:position block)
        curve (:curve block)
        height (or (:height block) 0.0)
        block-length (- (:end-ref block) (:start-ref block))

        end-x (or end-x start-x)
        end-y (or end-y start-y)

        [dx dy] direction
        [pxo pyo pzo] position
        [px py pz] (geom/vector-add position [(* dy start-x) start-y (* -1.0 dx start-x)])

        px2 (+ (* dx block-length) pxo)
        py2 (+ height pyo)
        pz2 (+ (* dy block-length) pzo)

        ;; direction has already been rotated
        ;; direction2 (geom/rotate-vector-2d direction
        ;;                                   (Math/cos (- curve))
        ;;                                   (Math/sin (- curve)))
        direction2 direction

        next-turn (or (:turn next-block) (:turn block))

        direction2 (geom/rotate-vector-2d direction2
                                          (Math/cos next-turn)
                                          (Math/sin next-turn))

        next-curve (float
                    (or (:radius (:curve next-block))
                        (:radius (:curve block))
                        0.0))
        next-pitch (or (:pitch next-block) (:pitch block))

        slope2 (/ block-length (Math/sqrt (+ 1.0 (* next-pitch next-pitch))))
        height2 (* slope2 block-length)

        b2 (if (= 0.0 next-curve) 0.0
               (/ slope2 (Math/abs next-curve)))

        block-interval2 (Math/sqrt
                         (* 2.0 next-curve next-curve (- 1.0 (Math/cos b2))))

        a2 (* 0.5 (Math/signum next-curve) b2)
        [dx2 dy2] (geom/rotate-vector-2d direction2
                                         (Math/cos (- a2)) (Math/sin (- a2)))

        offset2 [(* dy2 end-x) end-y (* -1.0 dx2 end-x)]
        position2 (geom/vector-add offset2 [px2 py2 pz2])

        [pdx pdy pdz] (geom/vector-normalize (geom/vector-sub position2 [px py pz]))

        [norm-x norm-z] (geom/vector-normalize [pdx pdz])

        rail-trans-z [pdx pdy pdz]
        rail-trans-x [norm-z 0.0 (* -1.0 norm-x)]
        rail-trans-y (geom/vector-cross rail-trans-z rail-trans-x)

        rail-transform (geom/transform-create-from-vectors
                        rail-trans-x
                        rail-trans-y
                        rail-trans-z)

        delta-x (- end-x start-x)
        delta-y (- end-y start-y)

        planar (Math/atan (/ dx block-length))
        updown (Math/atan (/ dy block-length))]
    {:planar planar
     :updown updown
     :rail-transform rail-transform
     :position [px py pz]
     :direction direction}))

(defn- next-block [context block]
  (nth (:blocks context)
       (int (/ (:end-ref block) (:block-size context)))
       nil))

(defn- create-rail-transforms-in-block [block next-block]
  (assoc
      block :rails
      (let [rails (:rails block)]
        (into
         {}
         (map (fn [[railidx rail]]
                [railidx
                 (merge
                  rail
                  (if (or (= railidx 0) (nil? next-block))
                    (create-transforms-for-player-rail block)
                    (create-transforms-for-rail block next-block rail)
                    ))])
              (:rails block))))))

(defn- create-geometries-for-blocks-in-context [context]
  (let [block-length (:block-size context)]
    (assoc context :blocks
           (reduce
            (fn [blocks block]
              (conj blocks
                    (create-rail-transforms-in-block
                     (create-track-transforms-in-block
                      (last blocks)
                      (update-position-in-block (last blocks) block block-length))
                     (next-block context block))))
            [] (:blocks context)))))
