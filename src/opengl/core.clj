(ns opengl.core
  (:gen-class))

(defrecord V3f [^float x ^float y ^float z])
(defn V3f-make [x y z] (V3f. x y z))

(defrecord Color4f [^float r ^float g ^float b ^float a])
(defn Color4f-make
  ([r g b] (Color4f. r g b 255))
  ([r g b a] (Color4f. r g b a)))

(defrecord Face [verts transp])
(defrecord Mesh [faces color texture texture-coords])

(defrecord TextureCoord [vertex x y])

(defmethod clojure.core/print-method Mesh [x writer]
  (.write writer (str "#Mesh(" (count (:faces x)) " face" (if (not= 1 (count (:faces x))) "s")
                      " Textures: " (:texture x) " (" (count (:texture-coords x)) "))")))

(defn b3d-build-mesh [mesh]
  (Mesh. (:faces mesh) (:color mesh) (:texture-path mesh) (:texture-coords mesh)))

(defn b3d-save-active-mesh [context]
  (let [mesh (:active context)
        meshes (:meshes context)]
    (if mesh
      (assoc context
        :meshes (cons (b3d-build-mesh mesh) meshes)
        :active {})
      context)))

(defn b3d-push-mesh-builder [context]
  (assoc (b3d-save-active-mesh context) :active {:verts [] :faces [] :texture-coords {}}))

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

(defn b3d-set-transp-color [context color]
  (b3d-assoc-active context :transp color))

(defn b3d-set-texture-path [context path]
  (b3d-assoc-active context :texture-path (str (:path context) java.io.File/separator  path)))

;; TODO- Explicitly reject nighttime textures for now
(defn b3d-parse-load [line]
  (-> line (.substring 4) (.split ",") first .trim))

(defn b3d-push-vertex [context vertex]
  (let [verts (:verts (:active context))]
    (b3d-assoc-active context :verts (conj verts vertex))))

(defn b3d-parse-vertex-line [line]
  (apply V3f-make (b3d-parse-prefix-line line "Vertex")))

(defn b3d-parse-face-line [line]
  (b3d-parse-prefix-line line "Face"))

(defn b3d-parse-face-2-line [line]
  (b3d-parse-prefix-line line "Face2"))

(defn b3d-get-vertex [context n]
  (let [verts (:verts (:active context))
        len (count verts)]
    (if (>= n len)
      (do
        (println "WARNING: (Line " (:linenum context) "): " (:line context) " ; Invalid vertex: " n)
        nil)
      (nth verts n))))

(defn b3d-push-texture-coordinate [context [v x y]]
  (let [tcoords (:texture-coords (:active context))
        vertex (b3d-get-vertex context v)]
    (if vertex
      (b3d-assoc-active context :texture-coords (assoc tcoords vertex [x y]))
      (do
        (println "WARNING: (Line " (:linenum context) "): " (:line context) " ; Invalid Coordinate")
        context))))

(defn b3d-push-face [context face-verts two-sided?]
  (let [face-v (doall (filter (comp not nil?) (map #(b3d-get-vertex context %) face-verts)))
        face (Face. face-v (:transp (:active context)))
        mesh (:active context)
        faces (:faces mesh)]
    (b3d-assoc-active context :faces (conj (:faces mesh) face))))

(defn b3d-parse-color-line [line]
  (apply Color4f-make (b3d-parse-prefix-line line "Color")))

(defn b3d-parse-transp-line [line]
  (apply Color4f-make (b3d-parse-prefix-line line "Transparent")))

(defn b3d-set-active-color [context color]
  (let [mesh (:active context)
        mesh-with-color (assoc mesh :color color)]
    (assoc context :active mesh-with-color)))


(defn starts-with [thing prefix]
  (-> thing .toLowerCase (.startsWith (.toLowerCase prefix))))

(defn handle-line [line linenum context]
  (let [context (assoc context :line line :linenum (+ 1 linenum))]
    (cond
     (or
      (= 0 (count line))
      (= \; (first line)))
     context

     ;; Mesh
     (starts-with line "[MeshBuilder]")
     (b3d-push-mesh-builder context)

     (starts-with line "Vertex") (b3d-push-vertex context (b3d-parse-vertex-line line))
     (starts-with line "Face") (b3d-push-face context (b3d-parse-face-line line) true)
     (starts-with line "Face2") (b3d-push-face context (b3d-parse-face-2-line line) false)
     (starts-with line "Color") (b3d-set-active-color context (b3d-parse-color-line line))
     (starts-with line "Transparent") (b3d-set-transp-color context (b3d-parse-transp-line line))

     ;; Texture
     (= "[Texture]" line)
     context

     (starts-with line "Load") (b3d-set-texture-path context (b3d-parse-load line))
     (starts-with line "Coordinates") (b3d-push-texture-coordinate context (b3d-parse-prefix-line line "Coordinates"))

     :else (b3d-set-error context (str "Invalid line: " line))
     )))

(defn handle-line-safe [line linenum context])

(defn strip-comment [str]
  (let [idx (.lastIndexOf str ";")]
    (.trim
     (if (> idx 0)
       (.substring str 0 idx)
       str))))

(defn handle-lines [reader context linenum]
  (let [line (.readLine reader)
        end-of-stream? (= line nil)
        error (:error context)]
    (cond
     error {:error error :context context}
     end-of-stream? (b3d-save-active-mesh context)
     :else (recur reader (handle-line (strip-comment line) linenum context) (+ 1 linenum)))))

(defn b3d-parse-file [^java.io.File file]
  (let [file-name (.getName file)
        reader (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. file)))
        result (handle-lines reader { :path (.getParent file) } 0)]
    (if (:error result)
      {:error (:error result)}
      (:meshes result)
    )))

(defn parse-object-folder [^String folder-path]
  (let [file (java.io.File. folder-path)]
    (reduce (fn [[success errors] obj-path]
              (if (.endsWith (.getName obj-path) ".b3d")
                (do
                  (println "Parsing " obj-path)
                  (let [result (b3d-parse-file obj-path)]
                   (if (:error result)
                     (do
                       (println result)
                       [success (conj errors result)])
                     [(merge success result) errors])))
                [success errors])) [{} []] (.listFiles file))))

(defn get-byte-buffer [^String file]
  (let [channel (.getChannel (java.io.RandomAccessFile. file "r"))]
    (.order
     (.map channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 (.size channel))
     java.nio.ByteOrder/LITTLE_ENDIAN)))

(defn bmp-read-header [buffer context]
  (assoc context
    :id (str ( char (.get buffer)) (char (.get buffer)))
    :size (.getInt buffer)
    :reserved (.getInt buffer)
    :start-offset (.getInt buffer)))

(defn bmp-push-error [context error]
  (assoc context :error error))

(defn bmp-validate-header [context]
  (if (not= (:id context) "BM")
    (bmp-push-error context (str "Expected BM id, got " (:id context)))
    context))

(defn bmp-v [fnc astr]
  (fn [ctxt]
    (if (fnc ctxt)
      ctxt
      (bmp-push-error ctxt astr))))

(defn bmp-validate-dib [context]
  (if (not (:error context))
    (reduce (fn [ctx afn] (afn ctx)) context
            [(bmp-v #(= 1 (:color-planes %)) "Only one color plane allowed")
             (bmp-v #(= 40 (:dib-size %)) "Expected DIB header size of 40")
             (bmp-v #(= 0 (:compression %)) "Expected no compression types supported")])
    context))

(defn bmp-read-dib [buffer context]
  (if (not (:error context))
    (assoc context
     :dib-size (.getInt buffer)
     :width (.getInt buffer)
     :height (.getInt buffer)
     :color-planes (.getShort buffer)
     :color-depth (.getShort buffer)
     :compression (.getInt buffer)
     :image-size (.getInt buffer)
     :horiz-res (.getInt buffer)
     :vert-res (.getInt buffer)
     :ncolors (.getInt buffer)
     :nimport (.getInt buffer))
    context))

(defn bmp-read-color-table-color [buffer]
  [(.get buffer) (.get buffer) (.get buffer) (.get buffer)])

(defn bmp-append-color-to-color-table [context color]
  (assoc context :color-table (conj (:color-table context) color)))

(defn bmp-read-color-table [buffer context]
  (reduce (fn [ctxt _]
            (bmp-append-color-to-color-table ctxt (bmp-read-color-table-color buffer)))
          context
          (range (:ncolors context))))

(defn bmp-save-file-ref [file context]
  (assoc context :file file))

(defn bmp-meta-read-file [^String file]
  (let [buffer (get-byte-buffer file)]
    (->> {:color-table []}
         (bmp-read-header buffer)
         (bmp-validate-header)
         (bmp-read-dib buffer)
         (bmp-validate-dib)
         (bmp-read-color-table buffer)
         (bmp-save-file-ref file))))

(defn bmp-buffer-at-data [metadata]
  (let [buffer (get-byte-buffer (:file metadata))]
    (.position buffer (:start-offset metadata))))

(defn color3f-as-int [[c1 c2 c3 c4]]
  (bit-or
   (bit-shift-left c1 24)
   (bit-shift-left c2 16)
   (bit-shift-left c3 8)
   c4))

(defn bmp-data-into-array [metadata]
  (let [color-table (:color-table metadata)
        buffer (bmp-buffer-at-data metadata)
        arraysize (* (:height metadata) (:width metadata) 4)
        array (byte-array arraysize)]
    (doseq [i (range (/ arraysize 4))]
      (let [[b1 b2 b3 b4] (nth color-table (.get buffer))
            x (* i 4)]
        (aset-byte array x b1)
        (aset-byte array (+ x 1) b1)
        (aset-byte array (+ x 2) b2)
        (aset-byte array (+ x 3) b3)))
    array))

(defn bmp-data-into-buffer [metadata]
  (java.nio.ByteBuffer/wrap (bmp-data-into-array metadata)))
