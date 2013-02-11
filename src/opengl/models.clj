(ns opengl.models
  (:gen-class))

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
    [faces])

(defn create-mesh [faces]
  (Mesh. faces))

(defrecord Face
    [verts material])

(defn create-face [verts material]
  (Face. verts material))

(defn update-face [face verts]
  (Face. verts (:material face)))

(defrecord ColorSet
    [color transparent emissive])

(defrecord TextureSet
    [daytime nighttime night-percentage])

(defrecord Texture
    [bmp-file])

(defrecord Material
    [color-set texture-set])

(defn create-material [colors textures]
  (Material. colors textures))

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
  (Block. id start-ref stop-ref nodes-in-block))
