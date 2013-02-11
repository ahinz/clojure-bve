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
                       (let [[x y z] (:coordinate vert)
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


(defn create-free-object [rails symbol-table node position starting-distance rail-transformation]
  (let [rt rail-transformation
        body (.trim (route/trim-trailing-comma (:body node)))
        [railidx i dx dy yaw pitch roll] (map read-string (.split body ";"))
        [rx ry] (:offset (get rails railidx))
        [px py pz] position
        yaw (or yaw 0.0)
        pitch (or pitch 0.0)
        roll (or roll 0.0)

        prototype (get symbol-table (str "freeobj" i))

        track-pos (read-string (:track-ref node))
        dz (- track-pos starting-distance)

        posx (+ rx px
                (* dx (:x (get-x rt)))
                (* dy (:x (get-y rt)))
                (* dz (:x (get-z rt))))

        posy (+ ry py
                (* dx (:y (get-x rt)))
                (* dy (:y (get-y rt)))
                (* dz (:y (get-z rt))))

        posz (+ pz
                (* dx (:z (get-x rt)))
                (* dy (:z (get-y rt)))
                (* dz (:z (get-z rt))))]
    (create-transformed-object
     prototype
     [posx posy posz]
     rail-transformation
     (geom/transform-create yaw pitch roll)
     track-pos)))

(defn create-rail-object
  [prototype rail-transform position direction track-pos]
  (create-transformed-object
   prototype
   position
   rail-transform
   geom/identity-transform
   track-pos))

(defn create-rail-objects
  [context track-pos]
  (let [position (:position context)
        direction (:direction context)
        rail-transform (:track-transform context)
        rails (:rails context)]
    (map (fn [rail]
        (let [type (:type rail)
              [x y] (:offset rail)
              [px py pz] position
              trans-position [(+ px x) (+ py y) pz]]
          (create-transformed-object
           (:type rail)
           trans-position
           rail-transform
           geom/identity-transform
           track-pos)))
      (vals rails))))

(defn create-wall-objects
  [walls rails context track-pos]
  (let [position (:position context)
        direction (:direction context)
        rail-transform (:track-transform context)]
    (map (fn [wall]
           (let [type (:type wall)
                 rail-id (:rail wall)
                 rail (get rails rail-id)
                 [x y] (:offset rail)
                 [px py pz] position]
             (create-transformed-object
              type
              [(+ x px) (+ y py) pz]
              rail-transform
              geom/identity-transform
              track-pos)))
         walls)))

(defn fatal-error [s]
  (throw (Exception. s)))

(defn split-body [node]
  (map read-string
       (.split
        (.trim (route/trim-trailing-comma (:body node))) ";")))

(defn fatal-error-missing-rail [railidx node]
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

(defn update-rail-objects-for-block
  [context block sym-tbl]
  (assoc
      context :rails
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
                             :type (get sym-tbl (str "rail" railidx))}))

                   (route/is-type node "railtype")
                   (let [[railidx texture] (split-body node)
                         rail (get rails railidx)]
                     (if (nil? rail)
                       (fatal-error-missing-rail railidx node))
                     (assoc
                         rails railidx
                         (assoc rail :type
                                (get sym-tbl (str "rail" railidx)))))

                   (route/is-type node "railend")
                   (fatal-error "not sure how to handle a rail end yet")
                   :else
                   rails))
                rails (:nodes-in-block block)))))

(defn update-context-objects-for-block
  [context block sym-tbl]
  (-> context
      (update-rail-objects-for-block block sym-tbl)))

(defn create-starting-context [symbol-table]
  {:symbol-table symbol-table
   :rails {0 {:offset [0.0 0.0]}}
   :walls {}

   :pitch 0

   :position [0.0 0.0 0.0]
   :direction [0.0 1.0]

   :block-length 25})

(defn update-position [context]
  (let [[x y z] (:position context)
        [dx dz] (:direction context)
        block-length (:block-length context)
        dx (* block-length dx)
        dz (* block-length dz)]
    (assoc context :position
           [(+ dx x) y (+ dz z)])))

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

(defn create-form-object [context form]
  (let [symbol-table (:symbol-table context)

        position (:position context)
        direction (:direction context)
        [dir-x dir-y] direction

        rail-transform (:track-transform context)

        [r1 r2 roof-idx form-idx] (split-body form)

        [dx1 dy1] (:offset (get (:rails context) r1))
        [dx2 dy2] (:offset (get (:rails context) r2))

        [dx1e _] (:offset-end (get (:rails context) r1))
        [dx2e _] (:offset-end (get (:rails context) r1))
        dx1e (or dx1e dx1)
        dx2e (or dx2e dx2)
        delta-start (- dx2 dx1)
        delta-end (- dx2e dx1e)

        [x y z] position

        p1 [(+ (* dx1 dir-y) x) (+ (* dy1 dir-x) y) z]
        p2 [(+ (* dx2 dir-y) x) (+ (* dy2 dir-x) y) z]

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
                   %1 %2 rail-transform geom/identity-transform
                   0) ; track position, un-used?
        ]
    (concat
     (map #(renderer % p1) (filter-not-nil prototypes-1))
     (map #(renderer % p2) (filter-not-nil prototypes-2)))
    )
  )

(defn create-form-objects [context forms]
  (apply concat
       (map #(create-form-object context %) forms)))

(defn create-objects-for-block
  [context block]
  (let [context (create-base-transforms context)
        symbol-table (:symbol-table context)
        position (:position context)
        direction (:direction context)

        context
        (update-context-objects-for-block
         context block symbol-table)

        starting-distance (:start-ref block)
        end-distance (:stop-ref block)
        nodes (sort-by
               #(read-string (:track-ref %))
               (:nodes-in-block block))]
    (let
        [objs
         (map
          (fn [node]
            (cond
             (route/is-type node "freeobj")
             (create-free-object
              (:rails context)
              symbol-table node
              position starting-distance
              geom/identity-transform ;; needs to be rail transform
              ))) nodes)
         formobjs (create-form-objects
                   context (filter #(route/is-type % "form") nodes))

         railobjs (create-rail-objects context starting-distance)
         wallobjs (create-wall-objects
                   (:walls context)
                   (:rails context)
                   context
                   starting-distance)]
      [(update-position context)
       (concat formobjs wallobjs objs railobjs)
       ]
      )))

(def r
  (route/resolve-symbol-table
   (route/parse-route-file "Flushing/test.csv")))
(def s (:symbol-table r))
(def bv (build-block-vector (:nodes r) 25))
(def ablock (nth bv 0))
(def bblock (nth bv 1))
(def cblock (nth bv 2))
(def dblock (nth bv 3))

(def blocks (take 7 bv))
(let [[context obj]
      (reduce (fn [[context objs] block]
                (let [[context new-objs] (create-objects-for-block context block)]
                  [context
                   (concat objs new-objs)]))
              [(create-starting-context s) []]
              blocks)]
  (def objs obj))
;; (def ctxt (create-starting-context s))
;; (def objs-and-ctxt (create-objects-for-block ctxt ablock))
;; (def objs (filter identity (second objs-and-ctxt)))
;; (def ctxt (first objs-and-ctxt))
;; (def objs-and-ctxt (create-objects-for-block ctxt bblock))
;; (def objs (concat objs (filter identity (second objs-and-ctxt))))
;; (def ctxt (first objs-and-ctxt))
;; (def objs-and-ctxt (create-objects-for-block ctxt cblock))
;; (def objs (concat objs (filter identity (second objs-and-ctxt))))
;; (def ctxt (first objs-and-ctxt))
;; (def objs-and-ctxt (create-objects-for-block ctxt dblock))
;; (def objs (concat objs (filter identity (second objs-and-ctxt))))
;; (def ctxt (first objs-and-ctxt))
