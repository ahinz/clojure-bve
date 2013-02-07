(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.route :as route]]
  [:import
   (javax.swing JFrame)
   (javax.media.opengl GLCapabilities GLDrawableFactory GLProfile GLEventListener GL GL2 DebugGL2)
   (javax.media.opengl.awt GLCanvas)
   (javax.media.opengl.glu.gl2 GLUgl2)]
  (:gen-class))


(defn gl-create-textures [^GL2 gl n]
  (let [tids (int-array n)]
    (.glGenTextures gl n tids 0)
    (map (fn [t] {:gl-texture-id t}) tids)))

(defn gl-create-texture [^GL2 gl]
  (first (gl-create-textures gl 1)))

(defn gl-bind-texture-to-bmp [gl tex bmp]
  (let [tid (:gl-texture-id tex)
        width (:width bmp)
        height (:height bmp)
        buffer (core/bmp-data-into-buffer bmp)]
    (doto gl
      (.glBindTexture GL/GL_TEXTURE_2D tid)
      (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_S GL/GL_REPEAT)
      (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_T GL/GL_REPEAT)
      (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
      (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
      (.glTexImage2D GL/GL_TEXTURE_2D 0 GL/GL_RGBA
                    width height 0 GL/GL_BGRA
                    GL/GL_UNSIGNED_BYTE buffer))
    (assoc tex :bmp bmp :bound true)))

(defn gl-texture-get-or-load [gl textures file]
  (if (get textures file)
    [textures (get textures file)]
    (let [bmp (core/bmp-meta-read-file file)
          texture (gl-bind-texture-to-bmp gl (gl-create-texture gl) bmp)]
      [(assoc textures file texture) texture])))

(defn gl-render-vertex [gl vertex texture texture-coord]
  (println texture)
  (println texture-coord)
  (if (and texture texture-coord)
    (.glTexCoord2f gl (first texture-coord) (second texture-coord)))
  (println "Render Vertex" vertex)
  (.glVertex3f gl (:x vertex) (:y vertex) (:z vertex)))

(defn gl-enable-texture-if-needed [gl texture]
  (.glEnable gl GL/GL_TEXTURE_2D)
  (.glBindTexture gl GL/GL_TEXTURE_2D (:gl-texture-id texture)))

(defn gl-disable-texture-if-needed [gl texture]
  (.glDisable gl GL/GL_TEXTURE_2D))

(defn gl-render-mesh [gl mesh textures]
  (let [
        texture-map (:texture-coords mesh)
        texture-file (:texture mesh)
        [textures texture] (gl-texture-get-or-load gl textures texture-file)]
    (gl-enable-texture-if-needed gl texture)
    (doseq [face (:faces mesh)]
      (.glBegin gl GL2/GL_POLYGON)
      (.glColor3f gl 1.0 1.0 1.0)
      (doseq [vertex (:verts face)]
        (gl-render-vertex gl vertex texture (get texture-map vertex)))
      (.glEnd gl))
    (gl-disable-texture-if-needed gl texture)
    textures))

;(def q-mesh (first (core/b3d-parse-file (java.io.File. "Flushing/Speed30.b3d"))))
(def q-mesh (first (core/b3d-parse-file (java.io.File. "Flushing/bldg17.b3d"))))

(defn create-event-proxy [w h]
  (proxy [GLEventListener] []
    (display [drawable]
      (let [gl (.getGL drawable)
            glu (GLUgl2.)]
        ;(.gluOrtho2D glu 0.0 450.0 0.0 375.0)
        (.gluOrtho2D glu 0.0 30.0 30.0 0.0)
        (doto gl
          (.glClear GL/GL_COLOR_BUFFER_BIT)
          ;; (.glBegin GL2/GL_POLYGON)
          ;; (.glVertex2f 0.300 0.50)
          ;; (.glVertex2f 0.350 0.60)
          ;; (.glVertex2f 0.375 0.100)
          ;; (.glVertex2f 0.325 0.115)
          ;; (.glVertex2f 0.300 0.75)
          ;; (.glEnd)
          (gl-render-mesh q-mesh {})
          )))
    (displayChanged [drawable modeChanged deviceChanged] (println "DC"))
    (init [drawable]
      (.setGL drawable (DebugGL2. (.getGL drawable)))
      (let [gl (.getGL drawable)]
        (println "Init......")
        (doto gl
          (.glClearColor 1.0 1.0 1.0 1.0)
          (.glColor3f 0.0 0.0 0.0 )
          (.glPointSize 4.0)
          (.glViewport 0 0 w h)
          (.glLoadIdentity)
          )))
    (reshape [drawable x y width height] (println "RE"))))

(defn make-canvas []
  (let [canvas (GLCanvas.)
        frame (JFrame.)
        content-pane (.getContentPane frame)
        [w h] [500 500]]
    (do
      (.add content-pane canvas)
      (.addGLEventListener canvas (create-event-proxy w h))
      (doto frame
        (.setSize w h)
        (.setVisible true)))))
