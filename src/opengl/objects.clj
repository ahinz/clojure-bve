(ns opengl.objects
  [:require
   [opengl.core :as core]
   [opengl.geom :as geom]
   [opengl.util :as util]
   [opengl.b3d :as b3d]
   [opengl.models :as m]
   [opengl.route :as route]]
  (:gen-class))


(defn- apply-translate-to-mesh
  [mesh [tx ty tz]
   normal-transform1
   normal-transform2]
  (m/create-mesh
   (map (fn [face]
          (let [verts
                (map (fn [vert]
                       (let [[x y z] (geom/rotate-with-transform
                                       (geom/rotate-with-transform
                                         (:coordinate vert) normal-transform1)
                                       normal-transform2)
                             [nx ny nz] (:normal vert)
                             [nx' ny' nz'] (geom/rotate-with-transform
                                             (geom/rotate-with-transform
                                               [nx ny nz] normal-transform1)
                                             normal-transform2)]
                         (let [v (m/create-vertex
                                  [(+ tx x)
                                   (+ ty y)
                                   (+ tz z)]
                                  (:texture-coordinate vert)
                                  [nx' ny' nz'])]
                           v)))
                     (:verts face))]
            (m/update-face face verts)))
        (:faces mesh))))

(defn- create-transformed-object
  [prototype pos base-transform aux-transform track-position]
  (map (fn [mesh]
         (apply-translate-to-mesh mesh pos aux-transform base-transform))
       prototype))


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
    (if (or (nil? radius) (= (float radius) 0.0))
      (create-track-transforms-in-block-with-direction
        block direction)
      (let [b (/ block-length (Math/abs (float radius)))
            c (Math/sqrt (* 2.0 (float radius) (float radius) (- 1.0 (Math/cos b))))
            a (* -1.0 0.5 (Math/signum (float radius)) b)]
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

        ;;TODO remove me
        height 0.0

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
                    (do
                      (create-transforms-for-rail block next-block rail))
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
                     (route/next-block context block))))
            [] (:blocks context)))))

(defn- get-x [tx] (nth tx 0))
(defn- get-y [tx] (nth tx 1))
(defn- get-z [tx] (nth tx 2))

(defn- rotate-free-obj [rail-transform [px py pz] [dx dy dz]]
  [(+ px
      (* dx (:x (get-x rail-transform)))
      (* dy (:x (get-y rail-transform)))
      (* dz (:x (get-z rail-transform))))
   (+ py
      (* dx (:y (get-x rail-transform)))
      (* dy (:y (get-y rail-transform)))
      (* dz (:y (get-z rail-transform))))
   (+ pz
      (* dx (:z (get-x rail-transform)))
      (* dy (:z (get-y rail-transform)))
      (* dz (:z (get-z rail-transform))))])

(defn- filter-not-nil [f] (filter identity f))

; this is copied from https://github.com/sladen/openbve/blob/master/openBVE/OpenBve/OldParsers/CsvRwRouteParser.cs#L4634
(defn- transform-form-object [prototype neardist fardist]
  (when prototype
    (let [mesh (first prototype)
          faces (:faces mesh)
          face (first faces)
          verts (:verts face)

          x-sub (fn [v s]
                  (when v
                    (let [[x y z] (:coordinate v)]
                      (m/update-vertex v [(- s x) y z]))))

          [_ _ x2 x3 _ _ x6 x7] verts
          [x2 x3 x6 x7] [(x-sub x2 neardist)
                         (x-sub x3 fardist)
                         (x-sub x6 neardist)
                         (x-sub x7 fardist)]

          update-vector [x3 x2 nil nil x7 x6 nil nil]

          tx-vector (map (fn [orig-v new-v]
                           (or new-v orig-v))
                         verts update-vector)

          new-face (m/update-face face tx-vector)
          new-mesh (m/create-mesh [new-face])]
      [new-mesh])))

(defn create-form-object [context block form]
  (if (nil? form)
    []
    (let [symbol-table (:symbol-table context)

          ;; position (:position block)
          ;; direction (:direction block)
          ;; [dir-x dir-y] direction

          roof-idx (:roof-idx form)
          form-idx (:form-idx form)

          rail1 (get (:rails block) (:rail1 form))
          rail2 (get (:rails block) (:rail2 form))

          rail-transform1 (:rail-transform rail1)
          rail-transform2 (:rail-transform rail2)

          [dx1 dy1] (:start rail1)
          [dx2 dy2] (:start rail2)

          [dx1e _] (:end rail1)
          [dx2e _] (:end rail2)

          dx1e (or dx1e dx1)
          dx2e (or dx2e dx2)
          delta-start (- dx2 dx1)
          delta-end (- dx2e dx1e)

          p1 (:position rail1)
          p2 (:position rail2)

          prototype-form-l (get symbol-table (str "forml" form-idx))
          prototype-form-cl (transform-form-object
                             (get symbol-table (str "formcl" form-idx))
                             delta-start delta-end)

          prototype-roof-l (get symbol-table (str "roofl" roof-idx))
          prototype-roof-cl (transform-form-object
                             (get symbol-table (str "roofcl" roof-idx))
                             delta-start delta-end)

          prototype-sec-rail-form-r (get symbol-table (str "formr" form-idx))
          prototype-sec-rail-roof-r (get symbol-table (str "roofr" roof-idx))

          prototypes-1 [prototype-form-l prototype-form-cl
                        prototype-roof-l prototype-roof-cl]

          prototypes-2 [prototype-sec-rail-form-r
                        prototype-sec-rail-roof-r]

          renderer #(create-transformed-object
                     %1 %2 %3 geom/identity-transform
                     0) ; track position, un-used?
          ]
      (concat
       (map #(renderer % p1 rail-transform1) (filter-not-nil prototypes-1))
       (map #(renderer % p2 rail-transform2) (filter-not-nil prototypes-2)))
      ))
  )

(defn- get-rail-aligned-objects-in-rail [context block rail]
  (let [[dx dy] (:start rail)
        [x y z] (:position block)
        x (+ dx x)
        y (+ dy y)
        pos [x y z]
        rail-transform (:rail-transform rail)
        track-pos (:start-ref block)]
    (concat
     ;; Rails
     (create-transformed-object
      (:prototype rail) pos
      rail-transform geom/identity-transform
      track-pos)

     ;; Freeobjs
     (map
      (fn [freeobj]
        (let [x (:x freeobj)
              y (:y freeobj)
              z (:z freeobj)
              position (rotate-free-obj rail-transform
                                        pos
                                        [x y z])]
          (create-transformed-object
           (:prototype freeobj)
           position
           rail-transform
           (geom/transform-create (:yaw freeobj)
                                  (:pitch freeobj)
                                  (:roll freeobj))
           track-pos)))
      (:freeobjs rail))

     ;; Walls
     (map
      #(create-transformed-object
        (second %) pos
        rail-transform geom/identity-transform
        track-pos) (:walls rail)))))

(defn- flatten1 [l] (apply concat l))
(defn- get-rail-aligned-objects-in-block [context block]
  (flatten1
   (map
    #(get-rail-aligned-objects-in-rail context block (second %))
    (:rails block))))

(defn get-drawable-objects-in-block [context block]
  (concat
   (get-rail-aligned-objects-in-block context block)
   (create-form-object context block (:form block))))

(defn get-drawable-objects-in-context [context]
  (reduce (fn [objs block]
            (concat
             objs
             (get-drawable-objects-in-block context block)))
          []
          (:blocks context)))

(def context1 (route/parse-route-file "Flushing/test.csv"))
(def context
  (create-geometries-for-blocks-in-context context1))

(def context (assoc context :blocks (take 40 (:blocks context))))
(def objs (get-drawable-objects-in-context context))

(def dummy 1)
