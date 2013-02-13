(ns opengl.b3d
  [:require
   [opengl.models :as m]
   [opengl.util :as util]]
  (:gen-class))

(set! *warn-on-reflection* true)

(defrecord Face [verts transp two-sided])
(defrecord Mesh [verts faces color texture blend texture-coords])

(defn- build-mesh [mesh]
  (Mesh. (:verts mesh) (:faces mesh) (:color mesh)
         (:texture-path mesh) (:blend mesh) (:texture-coords mesh)))

(defn- save-active-mesh [context]
  (let [mesh (:active context)
        meshes (:meshes context)]
    (if mesh
      (assoc context
        :meshes (cons (build-mesh mesh) meshes)
        :active {})
      context)))

(defn- push-mesh-builder [context]
  (assoc (save-active-mesh context) :active {:verts [] :faces [] :texture-coords {}}))

(defn- set-error [context err]
  (assoc context :error err))

(defn- parse-prefix-line [^String line ^String prefix]
  (map read-string
       (.split
        (.substring line (count prefix))
        ",")))

(defn- assoc-active [context key val]
  (let [mesh (:active context)
        updated-mesh (assoc mesh key val)]
    (assoc context :active updated-mesh)))

(defn- set-transp-color [context color]
  (assoc-active context :transp color))

(defn- set-texture-path [context path]
  (assoc-active context :texture-path (str (:path context) java.io.File/separator  path)))

(defn- set-blend-mode [context parts]
  (let [[^String mode ^String glow-half ^String glow-mode] parts]
    (assoc-active context :blend
                      {:mode (.toLowerCase mode)
                       :glow (if (and glow-half glow-mode)
                               {:mode (.toLowerCase glow-mode)
                                :half (read-string glow-half)})})))

;; TODO- Explicitly reject nighttime textures for now
;; These are used for typing first and second to string
(defn- trim [^String s] (.trim s))
(defn- replace-windows-path-chars [^String s]
  (.replaceAll s "\\\\" "/"))

(defn- parse-load [^String line]
  (-> line (.substring 4) (.split ",") first trim replace-windows-path-chars))

(defn- parse-load2 [^String line]
  (-> line (.split ",") second trim replace-windows-path-chars))


(defn- push-vertex [context vertex]
  (let [verts (:verts (:active context))]
    (assoc-active context :verts (conj verts vertex))))

(defn- parse-vertex-line [line]
  (parse-prefix-line line "Vertex"))

(defn- parse-face-line [line]
  (parse-prefix-line line "Face"))

(defn- parse-face-2-line [line]
  (parse-prefix-line line "Face2"))

(defn- get-vertex [context n]
  (let [verts (:verts (:active context))
        len (count verts)]
    (if (>= n len)
      (do
        (println "WARNING: (Line " (:linenum context) "): " (:line context) " ; Invalid vertex: " n)
        nil)
      (nth verts n))))

(defn- push-texture-coordinate [context [v x y]]
  (let [tcoords (:texture-coords (:active context))]
    (assoc-active context :texture-coords (assoc tcoords v [x y]))))

(defn- push-face [context face-verts two-sided?]
  (let [
        face (Face. face-verts (:transp (:active context)) two-sided?)
        mesh (:active context)
        faces (:faces mesh)]
    (assoc-active context :faces (conj (:faces mesh) face))))

(defn- parse-color-line [line]
  (parse-prefix-line line "Color"))

(defn- parse-transp-line [line]
  (parse-prefix-line line "Transparent"))

(defn- set-active-color [context color]
  (let [mesh (:active context)
        mesh-with-color (assoc mesh :color color)]
    (assoc context :active mesh-with-color)))

(defn- c-split [^String s]
  (map read-string (rest (.split s ","))))

(defn- handle-line [^String line linenum context]
  (let [context (assoc context :line line :linenum (+ 1 linenum))]
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
     (push-vertex context (c-split line))

     (util/starts-with line "Vertex")
     (push-vertex context (parse-vertex-line line))

     (util/starts-with line "Face")
     (push-face context (parse-face-line line) false)

     (util/starts-with line "Face2")
     (push-face context (parse-face-2-line line) true)

     (util/starts-with line "AddFace")
     (push-face context (c-split line) false)

     (util/starts-with line "Color")
     (set-active-color context (parse-color-line line))

     (util/starts-with line "Transparent")
     (set-transp-color context (parse-transp-line line))

     ;; Texture
     (= "[Texture]" line)
     context

     (util/starts-with line "LoadTexture")
     (set-texture-path context (parse-load2 line))

     (util/starts-with line "Load")
     (set-texture-path context (parse-load line))

     (util/starts-with line "SetTextureCoordinates")
     (push-texture-coordinate context (c-split line))

     (util/starts-with line "Coordinates")
     (push-texture-coordinate context (parse-prefix-line line "Coordinates"))

     ;; TODO: Implement
     (util/starts-with line "SetEmissiveColor")
     context

     ;; TODO: Implement
     (util/starts-with line "SetBlendMode")
     (set-blend-mode context (rest (.split line ",")))

     :else (set-error context (str "Invalid line: " line " (line " linenum ")"))
     )))

(defn- strip-comment [^String str]
  (let [idx (.lastIndexOf str ";")]
    (.trim
     (if (> idx 0)
       (.substring str 0 idx)
       str))))

(defn- handle-lines [^java.io.BufferedReader reader context linenum]
  (let [line (.readLine reader)
        end-of-stream? (= line nil)
        error (:error context)]
    (cond
     error {:error error :context context}
     end-of-stream? (save-active-mesh context)
     :else (recur reader (handle-line (strip-comment line) linenum context) (+ 1 linenum)))))

(defn- create-mesh [ast-mesh]
  (let [color (:color ast-mesh)
        texture (:texture ast-mesh)
        blend-mode (or (:blend ast-mesh) {:mode nil :glow nil})
        colors (m/create-color-set color nil nil) ;;TODO support transparent and emissive color
        textures (m/create-texture-set texture nil 0.0) ;;TODO support blend and nightitme textures
        material (m/create-material colors textures blend-mode)
        tcoords (:texture-coords ast-mesh)
        verts (:verts ast-mesh)
        faces (:faces ast-mesh)
        updated-verts (map-indexed
                       (fn [idx ver] (m/create-vertex ver (get tcoords idx))) verts)]
    (m/create-mesh
     (map (fn [face]
            (m/create-face
             (map #(nth updated-verts %) (:verts face))
             material
             (:two-sided face)))
          faces))))


(defn parse-file [^java.io.File file]
  (let [file-name (.getName file)
        reader (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. file)))
        result (handle-lines reader { :path (.getParent file) } 0)]
    (if (:error result)
      {:error (:error result)}
      (map create-mesh (:meshes result))
    )))
