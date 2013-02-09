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
                                  (:texture-coords vert)
                                  [nx' ny' nz'])]
                           v)))
                     (:verts face))]
            (m/create-face verts (:material face))))
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
  [rails rail-transform position direction track-pos]
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
       (vals rails)))

(defn create-wall-objects
  [walls rails rail-transform position direction track-pos]
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
       walls))

(defn fatal-error [s]
  (throw (Exception. s)))

(defn split-body [node]
  (map read-string
       (.split
        (.trim (route/trim-trailing-comma (:body node))) ";")))

(defn fatal-error-missing-rail [railidx node]
  (fatal-error (str "Could not find rail index " railidx " " node)))

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
  {:rails {0 {:offset [0.0 0.0]}}
   :rail-transform geom/identity-transform
   :position [0.0 0.0 0.0]
   :direction [0.0 0.0 0.0]
   :symbol-table symbol-table})

(defn create-objects-for-block
  [context block]
  (let [symbol-table (:symbol-table context)
        rail-transform (:rail-transform context)
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
              position starting-distance rail-transform))) nodes)
         railobjs (create-rail-objects
                   (:rails context)
                   rail-transform position direction
                   starting-distance)
         wallobjs (create-wall-objects
                   (:walls context)
                   (:rails context)
                   rail-transform position direction
                   starting-distance)]
      [context (concat wallobjs objs railobjs)]
      )))

(def r
  (route/resolve-symbol-table
   (route/parse-route-file "Flushing/test.csv")))
(def s (:symbol-table r))
(def bv (build-block-vector (:nodes r) 25))
(def ablock (nth bv 0))
(def iblock (nth bv 3))
(def ctxt
  {:rails
   {0 {:offset [0.0 0.0]
       :type (get s "rail9")}
    1 {:offset [-12.0 0.0]
       :type (get s "rail8")}}
   :walls
   [{:type (get s "walll5")
     :rail 0}
    {:type (get s "wallr5")
     :rail 1}]})

(def ctxt (create-starting-context s))
(def objs-and-ctxt (create-objects-for-block ctxt ablock))
(def objs (filter identity (second objs-and-ctxt)))
(def ctxt (first objs-and-ctxt))
