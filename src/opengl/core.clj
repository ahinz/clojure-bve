(ns opengl.core
  (:gen-class))

(defn get-byte-buffer [^String file]
  (let [channel (.getChannel (java.io.RandomAccessFile. file "r"))]
    (.order
     (.map channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 (.size channel))
     java.nio.ByteOrder/LITTLE_ENDIAN)))

(defrecord V3f [^float x ^float y ^float z])
(defn V3f-make [x y z] (V3f. x y z))

(defrecord Color3f [^float r ^float g ^float b])
(defn Color3f-make [r g b] (Color3f. r g b))

(defrecord Mesh [faces color])

(defmethod clojure.core/print-method Mesh [x writer]
  (.write writer (str "#Mesh(" (count (:faces x)) " face" (if (not= 1 (count (:faces x))) "s") ")")))

(defn b3d-build-mesh [mesh]
  (Mesh. (:faces mesh) (:color mesh)))

(defn b3d-save-active-mesh [context]
  (let [mesh (:active context)
        meshes (:meshes context)]
    (if mesh
      (assoc context
        :meshes (cons (b3d-build-mesh mesh) meshes)
        :active {})
      context)))

(defn b3d-push-mesh-builder [context]
  (assoc (b3d-save-active-mesh context) :active {:verts [] :faces []}))

(defn b3d-set-error [context err]
  (assoc context :error err))

(defn b3d-parse-prefix-line [line prefix]
  (map read-string
       (.split
        (.substring line (count prefix))
        ",")))

(defn b3d-assoc-active [context key val]
  (let [mesh (:active context)
        updated-mesh (assoc mesh key val)]
    (assoc context :active updated-mesh)))

(defn b3d-push-vertex [context vertex]
  (let [verts (:verts (:active context))]
    (b3d-assoc-active context :verts (conj verts vertex))))

(defn b3d-parse-vertex-line [line]
  (apply V3f-make (b3d-parse-prefix-line line "Vertex")))

(defn b3d-parse-face-line [line]
  (b3d-parse-prefix-line line "Face"))

(defn b3d-get-vertex [context n]
  (let [verts (:verts (:active context))
        len (count verts)]
    (if (>= n len)
      (do
        (println "WARNING: (Line " (:linenum context) ") \"" (:line context) "\" Invalid vertex: " n " Verticies: " verts)
        nil)
      (nth verts n))))

(defn b3d-push-face [context face-verts]
  (let [face (doall (filter (comp nil?) (map #(b3d-get-vertex context %) face-verts)))
        mesh (:active context)
        faces (:faces mesh)]
    (b3d-assoc-active context :faces (conj (:faces mesh) face))))

(defn b3d-parse-color-line [line]
  (apply Color3f-make (b3d-parse-prefix-line line "Color")))

(defn b3d-set-active-color [context color]
  (let [mesh (:active context)
        mesh-with-color (assoc mesh :color color)]
    (assoc context :active mesh-with-color)))

(defn handle-line [line linenum context]
  (let [context (assoc context :line line :linenum (+ 1 linenum))]
    (cond
     (or
      (= 0 (count line))
      (= \; (first line)))
     context

     (= "[MeshBuilder]" line)
     (b3d-push-mesh-builder context)

     (.startsWith line "Vertex") (b3d-push-vertex context (b3d-parse-vertex-line line))
     (.startsWith line "Face") (b3d-push-face context (b3d-parse-face-line line))
     (.startsWith line "Color") (b3d-set-active-color context (b3d-parse-color-line line))

     :else (b3d-set-error context (str "Invalid line: " line))
     )))

(defn handle-lines [reader context linenum]
  (let [line (.readLine reader)
        end-of-stream? (= line nil)
        error (:error context)]
    (cond
     error {:error error :context context}
     end-of-stream? (b3d-save-active-mesh context)
     :else (recur reader (handle-line (.trim line) linenum context) (+ 1 linenum)))))

(defn b3d-parse-file [^String file]
  (let [reader (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. file)))]
    (handle-lines reader {} 0)))
