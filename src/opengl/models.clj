(ns opengl.models
  [:require
   [opengl.geom :as geom]]
  (:gen-class))

(set! *warn-on-reflection* true)

;;;;;; Rendering Models

(defrecord Vertex
    [coordinate texture-coordinate normal])

(defn create-vertex
  ([coordinate texture-coordinate]
     (create-vertex coordinate texture-coordinate [0 0 0]))
  ([coordinate texture-coordinate normal]
     (Vertex. coordinate texture-coordinate normal)))

(defn update-vertex
  [vertex coordinate]
  (create-vertex coordinate (:texture-coordinate vertex) (:normal vertex)))

(defrecord Mesh
    [faces bounding-sphere])

(defn- vert-add
  ([] [0.0 0.0 0.0])
  ([v1 v2]
     (map
      +
      (or (:coordinate v1) v1)
      (or (:coordinate v2) v2))))

(defn- vert-scala-div [[x y z] d]
  [(/ x (float d)) (/ y (float d)) (/ z (float d))])

(defn- sqr [x] (* x x))

(defn- vert-distance-sqr [v1 v2]
  (let [[x1 y1 z1] (or (:coordinate v1) v1)
        [x2 y2 z2] (or (:coordinate v2) v2)]
    (+ (sqr (- x2 x1))
       (sqr (- y2 y1))
       (sqr (- z2 z1)))))

(defrecord Sphere
    [^doubles center ^double radius])

(defn- create-bounding-sphere-for-faces [faces]
  (let [verts (apply concat (map #(:verts %) faces))
        center (vert-scala-div
                (reduce vert-add verts)
                (count verts))]
    (Sphere.
     (double-array center)
     (if (= (count verts) 0)
       0.0
       (Math/sqrt
        (apply max (map #(vert-distance-sqr center %) verts)))))))

(defn create-mesh [faces]
  (Mesh. faces (create-bounding-sphere-for-faces faces)))

(defrecord Face
    [verts material two-sided])

(defn create-face [verts material two-sided]
  (Face. verts material two-sided))

(defn update-face [face verts]
  (Face. verts (:material face) (:two-sided face)))

(defrecord ColorSet
    [color transparent emissive])

(defrecord TextureSet
    [daytime nighttime night-percentage])

(defrecord Texture
    [bmp-file])

(defrecord Material
    [color-set texture-set blend-mode])

(defn create-material [colors textures blend-mode]
  (Material. colors textures blend-mode))

(defn create-texture [t]
  (Texture. t))

(defn create-texture-set
  ([texture] (create-texture-set texture nil 0))
  ([day-texture night-texture pct-night-in-blend]
     (TextureSet. day-texture night-texture pct-night-in-blend)))

(defn create-color-set
  ([color] (create-color-set color nil nil))
  ([color transp] (create-color-set color transp nil))
  ([color transp emiss] (ColorSet. color transp emiss)))

;;;;;;; Data models

(defrecord Block
    [id start-ref stop-ref nodes-in-block])

(defn create-block [id start-ref stop-ref nodes-in-block]
  {:id id :start-ref start-ref :stop-ref stop-ref
   :nodes-in-block nodes-in-block
   :curve 0.0
   :pitch 0.0
   :height 0.0
   :turn 0.0
   :offset [0.0 0.0]
   :track-transform geom/identity-transform
   :position [0.0 0.0 0.0]
   :direction [0.0 1.0]
   :block-length 25
   })
