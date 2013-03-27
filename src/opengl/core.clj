(ns opengl.core
  [:require
   [opengl.models :as m]]
  (:gen-class))

(set! *warn-on-reflection* true)

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

(defn bmp-data-into-array [metadata transp-color]
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
         (aset-byte array (- arraysize 1 (+ x 3)) b1)
         (aset-byte array (- arraysize 1 (+ x 2)) b2)
         (aset-byte array (- arraysize 1 (+ x 1)) b3)
         (aset-byte array (- arraysize 1 (+ x 0)) -128 ;; (if (= transp-color [b3 b2 b1])
                                                    ;; 0
                                                    ;; -128)
                    )))

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

(defn bmp-data-into-buffer [metadata transp-color]
  (if transp-color (println "Transp" transp-color))
  (java.nio.ByteBuffer/wrap (bmp-data-into-array metadata transp-color)))
