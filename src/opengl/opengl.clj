(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.route :as route]]
  [:import
   (javax.swing JFrame)
   (javax.media.opengl GLCapabilities GLDrawableFactory GLProfile GLEventListener GL GL2 DebugGL2)
   (javax.media.opengl.awt GLCanvas)
   (javax.media.opengl.glu.gl2 GLUgl2)
   (com.jogamp.opengl.util FPSAnimator)]
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

(defn gl-texture-get-or-load [gl file]
  (let [texture (get @textures file)]
    (if texture
      texture
      (let [bmp (core/bmp-meta-read-file file)
            texture (gl-bind-texture-to-bmp gl (gl-create-texture gl) bmp)]
        (dosync
         (ref-set textures
                  (assoc @textures file texture)))
        texture))))

(defn gl-render-vertex [gl vertex texture texture-coord]
  (if (and texture texture-coord)
    (.glTexCoord2f gl (first texture-coord) (second texture-coord)))
;  (println "Render Vertex" vertex)
  (.glVertex3f gl (:x vertex) (:y vertex) (:z vertex)))

(defn gl-enable-texture-if-needed [gl texture]
  (.glEnable gl GL/GL_TEXTURE_2D)
  (.glBindTexture gl GL/GL_TEXTURE_2D (:gl-texture-id texture)))

(defn gl-disable-texture-if-needed [gl texture]
  (.glDisable gl GL/GL_TEXTURE_2D))

(defn gl-render-mesh [gl mesh]
  (let [
        texture-map (:texture-coords mesh)
        texture-file (:texture mesh)
        texture (gl-texture-get-or-load gl texture-file)]
    (gl-enable-texture-if-needed gl texture)
    (doseq [face (:faces mesh)]
      (.glBegin gl GL2/GL_POLYGON)
      (.glNormal3d gl 1 0 0)
      (.glColor3f gl 1.0 1.0 1.0)
      (doseq [vertex (:verts face)]
        (gl-render-vertex gl vertex texture (get texture-map vertex)))
      (.glEnd gl))
    (gl-disable-texture-if-needed gl texture)
    textures))

;(def q-mesh (first (core/b3d-parse-file (java.io.File. "Flushing/Speed30.b3d"))))
;(def q-mesh (first (core/b3d-parse-file (java.io.File. "Flushing/bldg17.b3d"))))
(def q-mesh (core/b3d-parse-file (java.io.File. "Flushing/SheaStadium.b3d")))

(defn gl-draw-axis [gl]
  (doto gl
    (.glBegin GL/GL_LINES)
    (.glColor3f 1.0 0.0 0.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d 40.0 0.0 0.0)

    (.glColor3f 0.0 1.0 0.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d 0.0 40.0 0.0)

    (.glColor3f 0.0 0.0 1.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d 0.0 0.0 40.0)
    (.glEnd)))

(def textures (ref {}))
(def gl-context
  (ref { :camera {:eye [100.0 30.0 130.0]
                  :center [0.0 0.0 0.0]}
    }))

(defn glu-look-at [glu ex ey ez cx cy cz ux uy uz]
  (.gluLookAt glu ex ey ez cx cy cz ux uy uz))

(defn create-event-proxy [w h]
  (proxy [GLEventListener] []
    (display [drawable]
      (let [gl (.getGL drawable)
            glu (GLUgl2.)
            context @gl-context
            camera (:camera context)]
        (println context)

        (.glClear gl (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))

        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) 1.0 10.0 200.0)
        (apply glu-look-at
               (concat [glu]
                       (:eye camera) (:center camera) [0.0 1.0 0.0]))

        (doseq [mesh q-mesh]
         (gl-render-mesh gl mesh))

        (gl-draw-axis gl)
        ))
    (displayChanged [drawable modeChanged deviceChanged] (println "DC"))
    (init [drawable]
      (.setGL drawable (DebugGL2. (.getGL drawable)))
      (let [gl (.getGL drawable)
            glu (GLUgl2.)
            aspect (float (/ w h))]

        (.glClearColor gl (Float. 1.0) 1.0 1.0 1.0)
        (.glClear gl (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))

        (.glViewport gl 0 0 w h)

        (.glMatrixMode gl javax.media.opengl.fixedfunc.GLMatrixFunc/GL_MODELVIEW)
        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) aspect 10.0 200.0)

        (.gluLookAt glu
                    100.0 30.0 130.0  ; eye x,y,z
                    0.0 0.0 0.0     ; center x,y,z
                    0.0 1.0 0.0)    ; up direction

        (.glEnable gl GL/GL_DEPTH_TEST)

        (.glCullFace gl GL/GL_BACK)
        (.glEnable gl GL/GL_CULL_FACE)

        ;(gl-draw-axis gl)
        ))
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
        (.setVisible true)))
    [canvas frame]))

(def canvas (first (make-canvas)))
(def anim (FPSAnimator. canvas 10))
(.start anim)

(defn set-looking-at [canvas ex ey ez]
  (dosync
   (ref-set
    gl-context
    (let [context @gl-context
          camera (:camera context)
          updated-camera (assoc camera :eye [ex ey ez])]
      (assoc context :camera updated-camera))))
  (println @gl-context))
