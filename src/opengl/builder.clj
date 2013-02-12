(ns opengl.builder
  [:require
   [opengl.core :as core]
   [opengl.geom :as geom]
   [opengl.models :as m]
   [opengl.route :as route]]
  (:gen-class))

(def block-length 25)

(defn- partition-into-blocks [nodes block-length]
  (reduce (fn [blocks node]
            (if (:track-ref node)
              (let [track-ref-str (:track-ref node)
                    track-ref (read-string track-ref-str)
                    block-id (int (/ track-ref block-length))
                    nodes-in-block (or (get blocks block-id) [])]
                (assoc blocks block-id (conj nodes-in-block node))
              ))
            ) {} nodes))

(defn build-block-vector [nodes block-length]
  (let [block-map (partition-into-blocks nodes block-length)
        max-track-ref (apply max (keys block-map))]
    (into
     []
     (map
      (fn [block-id]
        (let [nodes-in-block (or (get block-map block-id) [])]
          (m/create-block
           block-id
           (* block-id block-length)
           (* (+ block-id 1) block-length)
           nodes-in-block)))
      (range max-track-ref)))))

(defn apply-translate-to-mesh
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

(defn create-transformed-object
  [prototype pos base-transform aux-transform track-position]
  (map (fn [mesh]
         (apply-translate-to-mesh mesh pos aux-transform base-transform))
       prototype))

(defn- get-x [tx]
  (nth tx 0))

(defn- get-y [tx]
  (nth tx 1))

(defn- get-z [tx]
  (nth tx 2))

(defn- read-string-if-not-empty [s]
  (if (= (count (.trim s)) 0) nil
      (read-string s)))

(defn- split-body [node]
  (if (nil? node)
    []
    (map read-string-if-not-empty
         (.split
          (.trim (route/trim-trailing-comma (:body node))) ";"))))

(defn create-rail-objects
  [block rail track-pos]
  (let [position (:position rail)
        direction (:direction rail)
        rail-transform (:rail-transform rail)
        type (:type rail)]
    (create-transformed-object
     (:type rail)
     position
     rail-transform
     geom/identity-transform
     track-pos)))

(defn create-signal-objects
  [context block player-rail]
  (map
   (fn [signal]
     (let [rail-transform (:rail-transform player-rail)
           position (rotate-free-obj rail-transform
                                     (:position player-rail)
                                     (:position signal))]
       (create-transformed-object
        (second (first (:prototypes signal)))
        position
        rail-transform
        (geom/transform-create (:yaw signal)
                               (:pitch signal)
                               (:roll signal))
        (:track-pos signal))))
   (:signals block)))


(defn create-free-objects
  [freeobjs block rail track-pos]
  (apply
   concat
   (map (fn [freeobj]
          (let [rail-transform (:rail-transform rail)
                position (rotate-free-obj rail-transform
                                          (:position rail)
                                          (:position freeobj))]
            (create-transformed-object
             (:prototype freeobj)
             position
             rail-transform
             (geom/transform-create (:yaw freeobj)
                                    (:pitch freeobj)
                                    (:roll freeobj))
             track-pos))) freeobjs)))

(defn create-wall-objects
  [wall block railidx rail track-pos]
  (if wall
    (apply
     concat
     (let [position (:position rail)
           direction (:direction rail)
           rail-transform (:rail-transform rail)
           [x y] (:offset rail)]
       (map #(create-transformed-object
              %
              position
              rail-transform
              geom/identity-transform
              track-pos) (:prototypes wall))))
    []))

(defn- fatal-error [s]
  (throw (Exception. s)))

(defn- fatal-error-missing-rail [railidx node]
  (fatal-error (str "Could not find rail index " railidx " " node)))

(defn create-base-transforms [context]
  (let [[dx dy] (:direction context)
        track-yaw (Math/atan2 dx dy)
        track-pitch (Math/atan (:pitch context))

        ground-transform (geom/transform-create track-yaw 0.0 0.0)
        track-transform (geom/transform-create track-yaw track-pitch 0.0)]
    (assoc context
      :ground-transform ground-transform
      :track-transform track-transform)))

(defn- update-rails [context rails]
  (assoc context :rails rails))

(defn- update-rails-and-block [context block rails]
  [(assoc context :rails rails)
   (assoc block :rails rails)])

(defn- prune-end-rails [context]
  (update-rails
   context
   (into {}
         (filter (fn [[k v]] (not (:end-rail v))) (:rails context)))))

(defn parse-block-with-wall-info
  [block sym-tbl]
  (reduce (fn [walls node]
            (let [[railidx dir index] (split-body node)
                  indicies (cond (= dir -1) ["walll"]
                                 (= dir 1)  ["wallr"]
                                 :else ["walll" "wallr"])]
              (assoc walls railidx
                     {:dir dir
                      :prototypes (map #(get sym-tbl (str % index)) indicies)})))
          {}
          (filter #(route/is-type % "wall") (:nodes-in-block block))))

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

(defn annotate-block-with-freeobj-info [context block sym-tbl]
  (let [rails (:rails block)
        rails (into {} (map (fn [[k v]] [k (assoc v :freeobjs [])]) rails))]
    (update-rails-and-block
     context
     block
     (reduce
      (fn [rails freeobj-node]
        (let [[railidx i dx dy yaw pitch roll] (split-body freeobj-node)
              yaw (or yaw 0.0)
              pitch (or pitch 0.0)
              roll (or roll 0.0)

              rail (get rails railidx)
              prototype (get sym-tbl (str "freeobj" i))

              track-pos (read-string (:track-ref freeobj-node))
              dz (- track-pos (:start-ref block))

              new-rail (assoc
                           rail :freeobjs
                           (conj (:freeobjs rail)
                                 {:prototype prototype
                                  :position [dx dy dz]
                                  :yaw yaw
                                  :pitch pitch
                                  :roll roll}))]
          (assoc rails railidx new-rail)))
      rails
      (filter #(route/is-type % "freeobj") (:nodes-in-block block))))))

(defn annotate-block-with-wall-info
  [context block sym-tbl]
  (let [walls (merge (:walls context)
                     (parse-block-with-wall-info block sym-tbl))
        rails (:rails block)
        rails (into {}
                    (map (fn [[rail-index rail]]
                           (let [wall-on-rail (get walls rail-index)]
                             (if wall-on-rail
                               [rail-index (assoc rail :walls wall-on-rail)]
                               [rail-index rail]))) rails))]
    [(assoc context :rails rails)
     (assoc block :rails rails)]))

(defn annotate-block-with-nexts-block-rail-info
  [context block sym-tbl]
  (let [new-rails
        (let [rails (:rails context)]
          (reduce (fn [rails node]
                    (cond
                     (route/is-type node "rail")
                     (let [[railidx x y railtype] (split-body node)
                           rail (get rails railidx)]
                       (if (nil? rail)
                         (fatal-error-missing-rail railidx node))
                       (assoc
                           rails railidx
                           (assoc rail :type
                                  (get sym-tbl (str "rail" railidx))
                                  :offset-end [x y])))

                     :else rails))
                  rails (:nodes-in-block (:next-block block))))]
    [(assoc context :rails new-rails)
     (assoc block :rails new-rails)]))

(def signal-translate
  {2 0
   -2 1
   3 2
   4 3
   -4 4
   5 5
   -5 6
   6 7})

(defn load-signal-object [signal_name]
  (core/b3d-parse-file (java.io.File. (str "compatibility/signals/" signal_name ".csv"))))

(defn create-signal-object [index [aspects names]]
  {index (into {} (map (fn [aspect name] [aspect (load-signal-object name)]) aspects names))})

(def signals
  (into {} (map-indexed
    create-signal-object
    [[[0 2] ["signal_2_0" "signal_2a_2"]]
     [[0 4] ["signal_2_0" "signal_2b_4"]]
     [[0 2 4] ["signal_3_0" "signal_3_2" "signal_3_4"]]
     [[0 1 2 4] ["signal_4_0" "signal_4a_1" "signal_4a_2" "signal_4a_4"]]
     [[0 2 3 4] ["signal_4_0" "signal_4b_2" "signal_4b_3" "signal_4b_4"]]
     [[0 1 2 3 4] ["signal_5_0" "signal_5a_1" "signal_5_2" "signal_5_3" "signal_5_4"]]
     [[0 2 3 4 5] ["signal_5_0" "signal_5_2" "signal_5_3" "signal_5_4" "signal_5b_5"]]
     [[0 1 2 3 4 5] ["signal_6_0" "signal_6_1" "signal_6_2" "signal_5_3" "signal_6_4" "signal_6_5"]]
     [[0 3 4] ["repeatingsignal_0" "repeatingsignal_3" "repeatingsignal_4"]]])))

(defn annotate-block-with-signals
  [context block sym-tbl]
  [context
   (assoc block :signals
          (map (fn [node]
                 (let [[aspects _ x y yaw pitch roll] (split-body node)
                       yaw (or yaw 0.0)
                       pitch (or pitch 0.0)
                       roll (or roll 0.0)
                       track-pos (float (read-string (:track-ref node)))
                       dz (- track-pos (:start-ref block))
                       ]
                   {:track-pos track-pos
                    :prototypes (get signals (get signal-translate aspects))
                    :position [x y dz]
                    ;; These funky constants are from the OpenBVE source
                    :yaw (* 0.0174532925199433 yaw)
                    :pitch (* 0.0174532925199433 pitch)
                    :roll (* 0.0174532925199433 roll)}))
               (filter #(or
                         (route/is-type % "signal")
                         (route/is-type % "sig")) (:nodes-in-block block))))])

(defn annotate-block-with-rail-info
  [context block sym-tbl]
  (let [new-rails
        (let [rails (:rails context)]
          (reduce (fn [rails node]
                    (cond
                     (route/is-type node "railstart")
                     (let [[railidx dx dy typ] (split-body node)]
                       (if (get rails railidx)
                         (fatal-error
                          (str "Cannot start rail " railidx " because it has already been started!")))
                       (assoc rails railidx
                              {:offset [dx dy]
                               :type (get sym-tbl (str "rail" railidx))
                               :cant 0.0
                               :freeobjs []
                               :curve 0.0}))

                     (route/is-type node "railtype")
                     (let [[railidx texture] (split-body node)
                           rail (get rails railidx)]
                       (if (nil? rail)
                         (fatal-error-missing-rail railidx node))
                       (assoc
                           rails railidx
                           (assoc rail :type
                                  (get sym-tbl (str "rail" railidx)))))

                     (route/is-type node "rail")
                     (let [[railidx x y railtype] (split-body node)
                           rail (get rails railidx)]
                       (if (nil? rail)
                         (fatal-error-missing-rail railidx node))
                       (assoc
                           rails railidx
                           (assoc rail :type
                               (get sym-tbl (str "rail" railidx))
                               :offset [x y])))

                     (route/is-type node "railend")
                     (let [[railidx x y] (split-body node)
                           rail (get rails railidx)
                           [sx sy] (:offset rail)
                           x (or x sx)
                           y (or y sy)]
                       (assoc rails railidx
                              (assoc rail
                                :offset-end [x y]
                                :end-rail true)))
                     :else
                     rails))
                  rails (:nodes-in-block block)))]
    [(assoc context :rails new-rails)
     (assoc block :rails new-rails)]))

(defn create-starting-context [symbol-table]
  {:symbol-table symbol-table
   :rails {0 {:offset [0.0 0.0]
              :freeobjs []}}
   :walls {}

   :pitch 0

   :position [0.0 0.0 0.0]
   :direction [0.0 1.0]
   :track-transform geom/identity-transform

   :block-length 25})

(defn update-position [context block]
  (let [player-rail (get (:rails block) 0)
        [x y z] (:position block)
        [dx dz] (:direction block)
        block-length (:block-length context)
        dxs (* block-length dx)
        dzs (* block-length dz)]
    (assoc context
      :position [(+ dxs x) y (+ dzs z)]
      :direction [dx dz])))

(defn- filter-not-nil [f] (filter identity f))

; this is copied from https://github.com/sladen/openbve/blob/master/openBVE/OpenBve/OldParsers/CsvRwRouteParser.cs#L4634
(defn transform-form-object [prototype neardist fardist]
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
  (let [symbol-table (:symbol-table context)

        ;; position (:position block)
        ;; direction (:direction block)
        ;; [dir-x dir-y] direction

        [r1 r2 roof-idx form-idx] (split-body form)

        rail1 (get (:rails block) r1)
        rail2 (get (:rails block) r2)

        rail-transform1 (:rail-transform rail1)
        rail-transform2 (:rail-transform rail2)

        [dx1 dy1] (:offset rail1)
        [dx2 dy2] (:offset rail2)

        [dx1e _] (:offset-end (get (:rails block) r1))
        [dx2e _] (:offset-end (get (:rails block) r1))
        dx1e (or dx1e dx1)
        dx2e (or dx2e dx2)
        delta-start (- dx2 dx1)
        delta-end (- dx2e dx1e)

        ;; [x y z] position

        ;; p1 [(+ (* dx1 dir-y) x) (+ (* dy1 dir-x) y) z]
        ;; p2 [(+ (* dx2 dir-y) x) (+ (* dy2 dir-x) y) z]

        p1 (:position rail1)
        p2 (:position rail1)

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
    )
  )

(defn create-form-objects [context block forms]
  (apply concat
       (map #(create-form-object context block %) forms)))

(defn- vector-add [v1 v2]
  (map + v1 v2))

(defn- vector-sub [v1 v2]
  (map - v1 v2))

(defn vector-normalize [v1]
  (let [t (Math/sqrt (apply + (map #(* % %) v1)))]
    (if (= t 0)
      v1
      (map #(/ % t) v1))))

(defn vector-cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn create-player-rail-transforms [track-transform position direction]
  {:planar 0.0
   :updown 0.0
   :position position
   :direction direction
   :rail-transform track-transform })

(defn create-transforms
  [rail block next-block direction position curve height block-length]
  (let [[start-x start-y] (:offset rail)
        [end-x end-y] (:offset-end rail)

        end-x (or end-x start-x)
        end-y (or end-y start-y)

        [dx dy] direction
        [pxo pyo pzo] position
        [px py pz] (vector-add position [(* dy start-x) start-y (* -1.0 dx start-x)])

        px2 (+ (* dx block-length) pxo)
        py2 (+ height pyo)
        pz2 (+ (* dy block-length) pzo)

        ;; direction has already been rotated
        ;; direction2 (geom/rotate-vector-2d direction
        ;;                                   (Math/cos (- curve))
        ;;                                   (Math/sin (- curve)))
        direction2 direction

        next-turn (:turn next-block)

        direction2 (geom/rotate-vector-2d direction2
                                          (Math/cos next-turn)
                                          (Math/sin next-turn))

        next-curve (:curve next-block)
        next-pitch (:pitch next-block)

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
        position2 (vector-add offset2 [px2 py2 pz2])

        [pdx pdy pdz] (vector-normalize (vector-sub position2 [px py pz]))

        [norm-x norm-z] (vector-normalize [pdx pdz])

        rail-trans-z [pdx pdy pdz]
        rail-trans-x [norm-z 0.0 (* -1.0 norm-x)]
        rail-trans-y (vector-cross rail-trans-z rail-trans-x)

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

(defn create-objects-for-block-and-rail [context block [railidx rail]]
  (concat
   (create-rail-objects block rail (:start-ref block))
   (create-wall-objects (:walls rail) block railidx rail (:start-ref block))
   (create-free-objects (:freeobjs rail) block rail (:start-ref block))))

(defn create-objects-for-block2
  [context block]
  [context
   (concat
    [(apply
      concat
      (map #(create-objects-for-block-and-rail context block %) (:rails block)))]
    (create-form-objects
     context block
     (filter #(route/is-type % "form") (:nodes-in-block block)))
    (create-signal-objects
     context block (get (:rails block) 0)))])

(defn create-transform-for-block-and-rail [context block [railidx rail]]
  (let [direction (:direction block)
        [dx dy] direction
        height (:height block)
        [rail-start-x rail-start-y] (:offset rail)
        offset [(* dy rail-start-x) height (* -1 dx rail-start-x)]

        orig-position (:position block)

        position (vector-add orig-position offset)
        next-block (:next-block block)

        world-transforms (if (or (= railidx 0) (not next-block))
                           (create-player-rail-transforms
                            (:track-transform block)
                            position
                            direction)
                           (create-transforms
                            rail block next-block direction orig-position
                            (:curve block) height block-length))]
    world-transforms))

(defn create-transform-for-block [context block]
  (let [rails (:rails block)]
    (update-rails
     block
     (into {} (map (fn [[railidx rail]]
                     [railidx
                      (merge
                       rail
                       (create-transform-for-block-and-rail
                        context block [railidx rail]))])
                   rails)))))

(defn update-block-directions [block curve direction]
  (let [[dx dy] direction
        track-yaw (Math/atan2 dx dy)]
    (assoc block
      :direction [dx dy]
      :track-transform
      (geom/transform-create track-yaw 0.0 0.0)
      :ground-transform
      (geom/transform-create track-yaw 0.0 0.0)
      :curve curve)))

(defn update-transforms-for-curves [context block]
  (let [curve (first (filter #(route/is-type % "curve") (:nodes-in-block block)))
        [rad cant] (map float (split-body curve))
        cant (or cant 0.0)]
    (if (and curve (not (= 0.0 rad)))
      (let [b (/ block-length (Math/abs rad))
            c (Math/sqrt (* 2.0 rad rad (- 1.0 (Math/cos b))))
            a (* -1.0 0.5 (Math/signum rad) b)]
        (update-block-directions
         block
         rad
         (geom/rotate-vector-2d (:direction block) (Math/cos a) (Math/sin a))))
      (update-block-directions
       block
       0.0
       (:direction block)))))

(defn parse-block-information [context prev-block block symbol-table]
  (let
      [block (assoc block
               :position (:position context)
               :direction (:direction context))
       [context block] (annotate-block-with-rail-info context block symbol-table)
       [context block] (annotate-block-with-nexts-block-rail-info
                         context block symbol-table)
       [context block] (annotate-block-with-wall-info context block symbol-table)
       [context block] (annotate-block-with-freeobj-info context block symbol-table)
       [context block] (annotate-block-with-signals context block symbol-table)

       block (update-transforms-for-curves context block)
       block (create-transform-for-block context block)

       context (assoc context :rails (:rails block))
       context (prune-end-rails context)

       context (update-position context block)]
    [context block]))

(defn provide-forward-references [blocks]
  (map (fn [block next-block]
         (assoc block
           :next-block next-block))
       blocks (concat (rest blocks) [nil])))

(def r
  (route/resolve-symbol-table
   (route/parse-route-file "Flushing/test.csv")))
(def s (:symbol-table r))
(def bv (build-block-vector (:nodes r) 25))
(def ablock (nth bv 0))
(def bblock (nth bv 1))
(def cblock (nth bv 2))
(def dblock (nth bv 3))

(def blocks (take 10 bv))
(def ablocks (provide-forward-references blocks))
(def ablocks (second
              (reduce (fn [[context blocks] block]
                        (let [[context block]
                              (parse-block-information context (last blocks) block s)]
                          [context (conj blocks block)]))
                      [(create-starting-context s) []] ablocks)))
(let [[context obj]
      (reduce (fn [[context objs] block]
                (let [[context new-objs] (create-objects-for-block2 context block)]
                  [context
                   (concat objs new-objs)]))
              [(create-starting-context s) []]
              ablocks)]
  (def objs obj))
