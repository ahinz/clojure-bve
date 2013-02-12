(ns opengl.core
  [:require
   [opengl.models :as m]]
  (:gen-class))

(set! *warn-on-reflection* true)

(defrecord Face [verts transp two-sided])
(defrecord Mesh [verts faces color texture blend texture-coords])

(defn validate-mesh [ast-mesh]
  ; validate texture coord refs here
  true)

(defn create-mesh [ast-mesh]
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

(defrecord TextureCoord [vertex x y])

(defn b3d-build-mesh [mesh]
  (Mesh. (:verts mesh) (:faces mesh) (:color mesh)
         (:texture-path mesh) (:blend mesh) (:texture-coords mesh)))

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

(defn b3d-parse-prefix-line [^String line ^String prefix]
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

(defn b3d-set-blend-mode [context parts]
  (let [[^String mode ^String glow-half ^String glow-mode] parts]
    (b3d-assoc-active context :blend
                      {:mode (.toLowerCase mode)
                       :glow (if (and glow-half glow-mode)
                               {:mode (.toLowerCase glow-mode)
                                :half (read-string glow-half)})})))

;; TODO- Explicitly reject nighttime textures for now
;; These are used for typing first and second to string
(defn- trim [^String s] (.trim s))
(defn- replace-windows-path-chars [^String s]
  (.replaceAll s "\\\\" "/"))

(defn b3d-parse-load [^String line]
  (-> line (.substring 4) (.split ",") first trim replace-windows-path-chars))

(defn b3d-parse-load2 [^String line]
  (-> line (.split ",") second trim replace-windows-path-chars))


(defn b3d-push-vertex [context vertex]
  (let [verts (:verts (:active context))]
    (b3d-assoc-active context :verts (conj verts vertex))))

(defn b3d-parse-vertex-line [line]
  (b3d-parse-prefix-line line "Vertex"))

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
  (let [tcoords (:texture-coords (:active context))]
    (b3d-assoc-active context :texture-coords (assoc tcoords v [x y]))))

(defn b3d-push-face [context face-verts two-sided?]
  (let [
        face (Face. face-verts (:transp (:active context)) two-sided?)
        mesh (:active context)
        faces (:faces mesh)]
    (b3d-assoc-active context :faces (conj (:faces mesh) face))))

(defn b3d-parse-color-line [line]
  (b3d-parse-prefix-line line "Color"))

(defn b3d-parse-transp-line [line]
  (b3d-parse-prefix-line line "Transparent"))

(defn b3d-set-active-color [context color]
  (let [mesh (:active context)
        mesh-with-color (assoc mesh :color color)]
    (assoc context :active mesh-with-color)))


(defn strip-non-ascii [^String string]
  (.replaceAll string "[^\\x00-\\x7F]" ""))

(defn- lower-case [^String l] (.toLowerCase l))
(defn- str-starts-with [^String s1 ^String s2] (.startsWith s1 s2))

(defn starts-with [^String thing ^String prefix]
  (-> thing strip-non-ascii lower-case (str-starts-with (lower-case prefix))))

(defn- c-split [^String s]
  (map read-string (rest (.split s ","))))

(defn handle-line [^String line linenum context]
  (let [context (assoc context :line line :linenum (+ 1 linenum))]
    (cond
     (or
      (= 0 (count line))
      (= \; (first line)))
     context

     ;; Mesh
     (or (starts-with line "[MeshBuilder]")
         (starts-with line "createmeshbuilder"))
     (b3d-push-mesh-builder context)

     (starts-with line "AddVertex")
     (b3d-push-vertex context (c-split line))

     (starts-with line "Vertex")
     (b3d-push-vertex context (b3d-parse-vertex-line line))

     (starts-with line "Face")
     (b3d-push-face context (b3d-parse-face-line line) false)

     (starts-with line "Face2")
     (b3d-push-face context (b3d-parse-face-2-line line) true)

     (starts-with line "AddFace")
     (b3d-push-face context (c-split line) false)

     (starts-with line "Color") (b3d-set-active-color context (b3d-parse-color-line line))
     (starts-with line "Transparent")
     (b3d-set-transp-color context (b3d-parse-transp-line line))

     ;; Texture
     (= "[Texture]" line)
     context

     (starts-with line "LoadTexture")
     (b3d-set-texture-path context (b3d-parse-load2 line))

     (starts-with line "Load")
     (b3d-set-texture-path context (b3d-parse-load line))

     (starts-with line "SetTextureCoordinates")
     (b3d-push-texture-coordinate context (c-split line))

     (starts-with line "Coordinates")
     (b3d-push-texture-coordinate context (b3d-parse-prefix-line line "Coordinates"))

     ;; TODO: Implement
     (starts-with line "SetEmissiveColor")
     context

     ;; TODO: Implement
     (starts-with line "SetBlendMode")
     (b3d-set-blend-mode context (rest (.split line ",")))

     :else (b3d-set-error context (str "Invalid line: " line " (line " linenum ")"))
     )))

(defn handle-line-safe [line linenum context])

(defn strip-comment [^String str]
  (let [idx (.lastIndexOf str ";")]
    (.trim
     (if (> idx 0)
       (.substring str 0 idx)
       str))))

(defn handle-lines [^java.io.BufferedReader reader context linenum]
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
      (map create-mesh (:meshes result))
    )))

(defn parse-object-folder [^String folder-path]
  (let [file (java.io.File. folder-path)]
    (reduce (fn [[success errors] ^java.io.File obj-path]
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

(defn bmp-read-header [^java.nio.ByteBuffer buffer context]
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

(defn bmp-update-ncolors [context]
  (if (= 0 (:ncolors context))
    (assoc context :ncolors (int (Math/pow 2 (:color-depth context))))
    context))

(defn bmp-read-dib [^java.nio.ByteBuffer buffer context]
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

(defn bmp-read-color-table-color [^java.nio.ByteBuffer buffer]
  [(.get buffer) (.get buffer) (.get buffer) (.get buffer)])

(defn bmp-append-color-to-color-table [context color]
  (assoc context :color-table (conj (:color-table context) color)))

; if the data starts at 54 bytes
; 14 byte BMP header + 40 byte dib header
; then *don't* read a color table
(defn bmp-read-color-table [buffer context]
  (if (= 54 (:start-offset context))
    context
    (reduce (fn [ctxt _]
              (bmp-append-color-to-color-table ctxt (bmp-read-color-table-color buffer)))
            context
            (range (:ncolors context)))))

(defn bmp-save-file-ref [file context]
  (assoc context :file file))

(defn bmp-meta-read-file [^String file]
  (let [buffer (get-byte-buffer file)]
    (->> {:color-table []}
         (bmp-read-header buffer)
         (bmp-validate-header)
         (bmp-read-dib buffer)
         (bmp-update-ncolors)
         (bmp-validate-dib)
         (bmp-read-color-table buffer)
         (bmp-save-file-ref file))))

(defn bmp-buffer-at-data [metadata]
  (let [^java.nio.ByteBuffer buffer (get-byte-buffer (:file metadata))]
    (.position buffer (:start-offset metadata))))

(defn color3f-as-int [[c1 c2 c3 c4]]
  (bit-or
   (bit-shift-left c1 24)
   (bit-shift-left c2 16)
   (bit-shift-left c3 8)
   c4))

(defn bmp-data-into-array [metadata]
  (let [color-table (:color-table metadata)
        depth (:color-depth metadata)
        buffer ^java.nio.ByteBuffer (bmp-buffer-at-data metadata)
        arraysize (* (:height metadata) (:width metadata) 4)
        array (byte-array arraysize)]
    (cond
     (= depth 24)
     (doseq [i (range (/ arraysize 4))]
       (let [[b1 b2 b3] (map (fn [_] (.get buffer)) (range 3))
             x (* i 4)]
         (aset-byte array x b1)
         (aset-byte array (+ x 1) b2)
         (aset-byte array (+ x 2) b3)
         (aset-byte array (+ x 3) -127)))

     (= depth 8)
     (doseq [i (range (/ arraysize 4))]
       (let [[b1 b2 b3 b4] (nth color-table (bit-and (short (.get buffer)) 0xff))
             x (* i 4)]
         (aset-byte array x b1)
         (aset-byte array (+ x 1) b2)
         (aset-byte array (+ x 2) b3)
         (aset-byte array (+ x 3) b4)))

     (= depth 4)
     (doseq [i (range (/ arraysize 8))]
       (let [byte (bit-and (short (.get buffer)) 0xff)
             color-idx-1 (bit-and byte 0xf)
             color-idx-2 (bit-and (bit-shift-right byte 4) 0xf)
             [b1 b2 b3 b4] (nth color-table color-idx-1)
             [b5 b6 b7 b8] (nth color-table color-idx-2)
             x (* i 8)]
         (aset-byte array x b1)
         (aset-byte array (+ x 1) b1)
         (aset-byte array (+ x 2) b2)
         (aset-byte array (+ x 3) b3)
         (aset-byte array (+ x 4) b1)
         (aset-byte array (+ x 5) b1)
         (aset-byte array (+ x 6) b2)
         (aset-byte array (+ x 7) b3)))
     :else (throw (Exception. (str "Bad color depth " depth))))
    array))

(defn bmp-data-into-buffer [metadata]
  (java.nio.ByteBuffer/wrap (bmp-data-into-array metadata)))
