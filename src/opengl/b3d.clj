(ns opengl.b3d
  [:require
   [opengl.models :as m]
   [opengl.util :as util]]
  (:gen-class))

(set! *warn-on-reflection* true)

(def errors
  {:vertex-count "A Vertex may only have three arguments"
   :number "Could not parse number"
   :unknown-vertex "Vertex #%s not found"
   :face-in-not-in-builder "Faces must be defined in a mesh builder object"
   :texture-in-not-in-builder "Textures must be defined in a mesh builder object"
   :unknown-line "Couldn't parse command"})

(defn- add-error [context error-key & rest]
  (assoc context :errors
         (conj
          (or (:errors context) [])
          {:line (:line context)
           :linenum (:linenum context)
           :error (apply format (concat [(get errors error-key)] rest))})))

(defrecord Face [verts transp two-sided])
(defn- create-face [verts transp two-sided] (Face. verts transp two-sided))

(defn- is-in-builder? [context] (not (nil? (:active context))))

(defn- build-face-objects [faces mesh]
  (let [texture-coords (:texture-coords mesh)
        two-sided (:two-sided mesh)
        daytime-texture (:texture-path mesh)
        nighttime-texture nil ;; Ignored for now
        blend (:blend mesh)
        color (:color mesh)
        color-set (m/create-color-set color (:transp mesh))
        texture-set (m/create-texture-set daytime-texture nighttime-texture 0.0)
        material (m/create-material color-set texture-set blend)]
    (apply vector
          (map (fn [face] ; Use vectors for faster random access
                 (m/create-face
                  (apply
                   vector
                   (map #(m/create-vertex (apply vector %) (get texture-coords %))
                        (:verts face)))
                  material
                  (:two-sided face))) faces))))

(defn- build-mesh [mesh]
  (m/create-mesh
   (build-face-objects
    (:faces mesh) mesh)))

(defn- save-active-mesh [context]
  (let [mesh (:active context)]
    (if mesh
      (assoc context
        :meshes (cons (build-mesh mesh) (:meshes context))
        :active {})
      context)))

(defn- push-mesh-builder [context]
  (assoc (save-active-mesh context) :active {:verts [] :faces [] :texture-coords {}}))

(defn- parse-prefix-line [^String line]
  (second (re-matches #"[a-zA-Z0-9]+\s+(.*)$" line)))

(defn- assoc-active [context key val]
  (let [mesh (:active context)
        updated-mesh (assoc mesh key val)]
    (assoc context :active updated-mesh)))

(defn- set-transp-color [context color]
  (assoc-active context :transp color))

(defn- set-texture-path [context path]
  (if (is-in-builder? context)
    (assoc-active context :texture-path (str (:path context) java.io.File/separator  path))
    (add-error context :texture-in-not-in-builder)))

(defn- set-blend-mode [context parts]
  (let [[^String mode ^String glow-half ^String glow-mode] parts]
    (assoc-active context :blend
                      {:mode (.toLowerCase mode)
                       :glow (if (and glow-half glow-mode)
                               {:mode (.toLowerCase glow-mode)
                                :half (read-string glow-half)})})))

;; TODO- Explicitly reject nighttime textures for now
;; These are used for typing first and second to string
(defn- parse-load [^String line]
  (-> line (.substring 4) (.split ",") first
      util/trim util/replace-windows-path-chars))

(defn- parse-load2 [^String line]
  (-> line (.split ",") second
      util/trim util/replace-windows-path-chars))

(defn- active-vertices [context]
  (:verts (:active context)))

(defn- push-vertex [context vertex]
  (cond
   (= (count vertex) 3)
   (assoc-active context :verts (conj (active-vertices context) vertex))

   (nil? vertex) ; number format error
   (assoc-active (add-error context :number)
                 :verts (conj (active-vertices context) [0.0 0.0 0.0]))

   :else ; length
   (assoc-active (add-error context :vertex-count)
                 :verts (conj (active-vertices context) [0.0 0.0 0.0]))))

(defn- push-texture-coordinate [context [v x y]]
  (let [tcoords (:texture-coords (:active context))
        verts (active-vertices context)]
    (cond
     (nil? v)
     (add-error context :number)

     (>= (int v) (count verts))
     (add-error context :unknown-vertex (int v))

     :else
     (assoc-active context :texture-coords
                   (assoc tcoords (nth verts (int v)) [x y])))))

(defn- resolve-vertices [context face-verts]
  (let [verts (active-vertices context)]
    (cond
     (nil? face-verts)
     [context nil]

     (>= (apply max face-verts) (count verts))
     [(add-error context :unknown-vertex (apply max face-verts)) nil]

     :else
     [context (map #(nth verts %) face-verts)])))

(defn- push-face [context face-verts two-sided?]
  (if (not (is-in-builder? context))
    (add-error context :face-in-not-in-builder)
    (let
        [[context new-face-verts] (resolve-vertices context face-verts)
         mesh (:active context)
         faces (:faces mesh)]
      (cond
       (nil? face-verts) (add-error context :number)
       (nil? new-face-verts) context  ; error added in resolve function

       :else
       (assoc-active
        context
        :faces (conj (:faces mesh)
                     (create-face new-face-verts
                                  (:transp (:active context)) two-sided?)))))))

(defn- set-active-color [context color]
  (let [mesh (:active context)
        mesh-with-color (assoc mesh :color color)]
    (assoc context :active mesh-with-color)))

(defn- handle-line [context ^String line]
  (let [context (assoc context
                  :line line
                  :linenum (+ 1 (or (:linenum context) 0)))
        line (util/strip-comment line)]
    (cond
     (or
      (= 0 (count line))
      (= \; (first line)))
     context

     ;; Mesh
     (or (util/starts-with line "[MeshBuilder]")
         (util/starts-with line "createmeshbuilder"))
     (push-mesh-builder context)

     (util/starts-with line "AddVertex")
     (push-vertex context (util/wrap-nil
                           (util/c-split-f line)))

     (util/starts-with line "Vertex")
     (push-vertex context (util/wrap-nil
                           (util/split-f (parse-prefix-line line))))

     (util/starts-with line "Face2")
     (push-face context (util/wrap-nil
                         (util/split-i (parse-prefix-line line))) true)

     (util/starts-with line "Face")
     (push-face context (util/wrap-nil
                         (util/split-i (parse-prefix-line line))) false)

     (util/starts-with line "AddFace")
     (push-face context (util/wrap-nil
                         (util/c-split line)) false)

     (util/starts-with line "Color")
     (set-active-color context (util/wrap-nil
                                (apply vector (util/split-i
                                  (parse-prefix-line line)))))

     (util/starts-with line "Transparent")
     (set-transp-color context (parse-prefix-line line))

     ;; Texture
     (= "[Texture]" line)
     context

     (util/starts-with line "LoadTexture")
     (set-texture-path context (parse-load2 line))

     (util/starts-with line "Load")
     (set-texture-path context (parse-load line))

     (util/starts-with line "SetTextureCoordinates")
     (push-texture-coordinate context (util/wrap-nil
                                       (util/c-split-f line)))

     (util/starts-with line "Coordinates")
     (push-texture-coordinate context (util/wrap-nil
                                       (util/split-f (parse-prefix-line line))))

     ;; TODO: Implement
     (util/starts-with line "SetEmissiveColor")
     context

     ;; TODO: Implement
     (util/starts-with line "SetBlendMode")
     (set-blend-mode context (rest (util/split line ",")))

     :else
     (add-error context :unknown-line))))

(defn parse-string
  ([^String s] (parse-string s {}))
  ([^String s context]
     (push-mesh-builder
      (reduce handle-line context (map #(util/trim %) (util/split s "\n"))))))

(defn parse-file [^java.io.File f]
  (parse-string (slurp f) {:path (.getParent f)}))

(defn parse-file-from-string [^String s]
  (try (parse-file (java.io.File. s))
       (catch Exception e {:errors (format "File not found %s" s)})))
