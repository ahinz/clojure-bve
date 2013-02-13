(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.route :as route]
   [opengl.models :as m]
   [opengl.geom :as geom]
   [opengl.builder :as builder]]
  [:import
   (javax.imageio ImageIO)
   (javax.swing JFrame)
   (javax.media.opengl GLCapabilities GLDrawableFactory GLProfile GLEventListener GL GL2 GL2GL3 DebugGL2 TraceGL2 GLAutoDrawable)
   (javax.media.opengl.awt GLCanvas)
   (javax.media.opengl.glu.gl2 GLUgl2)
   (com.jogamp.opengl.util FPSAnimator)
   (com.jogamp.opengl.util.gl2 GLUT)]
  (:gen-class))


(set! *warn-on-reflection* true)

(def textures (ref {}))

(defn gl-create-textures [^GL2 gl n]
  (if (> n 0)
    (let [tids (int-array n)]
      (.glGenTextures gl n tids 0)
      tids)
    []))

(defn gl-create-texture [^GL2 gl]
  (first (gl-create-textures gl 1)))

(defn gl-bind-texture-to-buffer
  [^GL2 gl ^Integer tid ^Integer width ^Integer height ^java.nio.ByteBuffer buffer]
  (doto gl
    (.glBindTexture GL/GL_TEXTURE_2D tid)
    (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_S GL/GL_REPEAT)
    (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_T GL/GL_REPEAT)
    (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
    (.glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
    (.glTexImage2D GL/GL_TEXTURE_2D 0 GL/GL_RGBA
                   width height 0 GL/GL_BGRA
                   GL/GL_UNSIGNED_BYTE buffer))
  tid)

;; TODO: Could probably do this for BMP as well
(defn gl-bind-texture-to-png [gl tid ^String png]
  (let [buffered-image (ImageIO/read (java.io.File. png))
        width (.getWidth buffered-image)
        height (.getHeight buffered-image)
        data (byte-array (* width height 4))]
    (doseq [x (range width)
            y (range height)]
      (let [color (.getRGB buffered-image x y)
            i (* 4 (+ (* y width) x))]
        ;; color is ARGB, but we're going to
        ;; load it as BGRA
        (aset-byte data (+ i 3) (unchecked-byte
                                 (bit-and 0xff (bit-shift-right color 24))))
        (aset-byte data (+ i 2) (unchecked-byte
                                 (bit-and 0xff (bit-shift-right color 16))))
        (aset-byte data (+ i 1) (unchecked-byte
                                 (bit-and 0xff (bit-shift-right color 8))))
        (aset-byte data    i    (unchecked-byte
                                 (bit-and 0xff color)))))
    (gl-bind-texture-to-buffer gl tid width height
                               (java.nio.ByteBuffer/wrap data))))

(defn gl-bind-texture-to-bmp [gl tid bmp]
  (let [meta (core/bmp-meta-read-file bmp)]
    (gl-bind-texture-to-buffer gl
                             tid
                             (:width meta)
                             (:height meta)
                             (core/bmp-data-into-buffer meta))))

(defn gl-bind-texture-to-file [gl tid ^String file]
  (println "Loading texture" file "in to gl texture slot" tid)
  (cond
   (.endsWith file ".bmp")
   (gl-bind-texture-to-bmp gl tid file)

   (.endsWith file ".png")
   (gl-bind-texture-to-png gl tid file)

   :else (throw (Exception. (str "Not sure how to load image: " file)))))

(defn gl-enable-texture-2d [^GL2 gl]
  (.glEnable gl GL/GL_TEXTURE_2D)
  ;(.glTexEnv gl GL2/GL_TEXTURE_ENV GL2/GL_TEXTURE_ENV_MODE GL2/GL_MODULATE)
  )

(defn gl-bind-current-texture [^GL2 gl texture]
  (let [tid (:gl-tid texture)]
    (if tid
      (.glBindTexture gl GL/GL_TEXTURE_2D tid))))

(defn gl-disable-texture [^GL2 gl]
  (.glDisable gl GL/GL_TEXTURE_2D))

(defn gl-load-single-texture [gl file]
  (gl-bind-texture-to-file gl
                            (gl-create-texture gl)
                            file))

(defn gl-mutable-load-and-save-single-texture [gl file]
  (let [t (gl-load-single-texture gl file)]
    (dosync
     (ref-set textures
              (assoc @textures file t))) t))


(defn mutable-create-or-get-texture-for-material [gl material]
  ; Right now we only support unblended daytime textures
  ; and use the primary color
  (let [colors (:color-set material)
        texture-set (:texture-set material)
        texture-file (:daytime texture-set)
        existing-texture (get @textures texture-file)]
    {:gl-tid (if (or (nil? texture-file) existing-texture)
               existing-texture
               (gl-mutable-load-and-save-single-texture gl texture-file))
     :color (:color colors)}))


(defn gl-render-vertex [^GL2 gl vertex]
  (let [tcoord (:texture-coordinate vertex)
        [x y z] (:coordinate vertex)
        [nx ny nz] (:normal vertex)]

    (.glNormal3d gl nx ny nz)

    (when tcoord
      (.glTexCoord2f gl (first tcoord) (second tcoord)))

    (.glVertex3f gl x y z)))

(defn gl-render-face [^GL2 gl face]
  (let [material (:material face)
        blend-mode (:blend-mode material)
        texture-info (mutable-create-or-get-texture-for-material gl material)
        color (:color (:color-set material))
        verts (:verts face)
        uses-glow-attn (:mode (:glow blend-mode))]

    (when (:gl-tid texture-info)
      (gl-enable-texture-2d gl)
      (gl-bind-current-texture gl texture-info))

    (if (:mode blend-mode)
      (if (= (:mode blend-mode) "additive")
        (.glBlendFunc gl GL/GL_SRC_ALPHA GL/GL_ONE)
        (.glBlendFunc gl GL/GL_ONE GL/GL_ZERO))
      (.glBlendFunc gl GL/GL_ONE GL/GL_ZERO))

    (if color
      (let [[r g b] color]
        (.glColor3f gl (/ r 255.0) (/ g 255.0) (/ b 255.0)))
      (.glColor3f gl 1.0 1.0 1.0))

    (if (:two-sided face)
      (.glPolygonMode gl GL/GL_FRONT_AND_BACK GL2GL3/GL_FILL)
      (.glPolygonMode gl GL/GL_FRONT GL2GL3/GL_FILL))

    ;(.glPolygonMode gl GL/GL_FRONT_AND_BACK GL2GL3/GL_LINE)
    ;(.glPolygonMode gl GL/GL_FRONT_AND_BACK GL2GL3/GL_FILL)
    ;(.glPolygonMode gl GL/GL_FRONT  GL2GL3/GL_FILL)
    ;(.glPolygonMode gl GL/GL_BACK  GL2GL3/GL_LINE)
    (.glDisable gl GL/GL_BLEND)

    (.glBegin gl GL2/GL_POLYGON)

    (if (not uses-glow-attn)
     (doseq [vert verts]
       (gl-render-vertex gl vert)))

    (.glEnd gl)

    (when (:gl-tid texture-info)
      (gl-disable-texture gl))))

(defn textures-in-vertex [v]
  (let [m (:material v)
        t (:texture-set m)
        d (:daytime t)
        n (:nighttime t)]
    (filter identity [d n])))

(defn gl-preload-textures [gl texture-files]
  (let [not-loaded-files (filter #(nil? (get @textures %)) (set texture-files))
        texture-ids (gl-create-textures gl (count not-loaded-files))]
    (dosync
     (doseq [[file tid] (map list not-loaded-files texture-ids)]
       (alter textures assoc file
              (gl-bind-texture-to-file gl tid file))
       not-loaded-files texture-ids))))

(defn gl-render-mesh [gl mesh]
  (doseq [face (:faces mesh)]
    (gl-preload-textures gl
     (filter identity
             (flatten
              (map textures-in-vertex face))))
    (gl-render-face gl face)))

(defn gl-draw-axis [^GL2 gl]
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
    (.glVertex3d 0.0 0.0 100.0)
    (.glEnd)))

(def gl-context
  (ref {:camera {:eye [20.0 20.0 -50.0]
                 :center [0.0 0.0 0.0]}
        :meshes (filter identity builder/objs)
    }))

(defn glu-look-at [^GLUgl2 glu
                   ^Double ex ^Double ey ^Double ez
                   ^Double cx ^Double cy ^Double cz
                   ^Double ux ^Double uy ^Double uz]
  (.gluLookAt glu ex ey ez cx cy cz ux uy uz))

(def last-time-stamps (ref (repeat 30 0)))

(defn- display-fps [^GL2 gl]
      (dosync
       (ref-set
        last-time-stamps
        (concat (rest @last-time-stamps) [(System/nanoTime)])))

      (let [glut (GLUT.)
            fps (* 1e9
                   (/ (count @last-time-stamps)
                      (- (last @last-time-stamps) (first @last-time-stamps))))]
        (.glPushMatrix gl)
        (.glLoadIdentity gl)
        (.glColor4f gl 0.0 0.0 1.0 1.0)
        (.glWindowPos2i gl 40 40)
        (.glutBitmapString
         glut
         GLUT/BITMAP_TIMES_ROMAN_24
         (format "FPS %2.2f" fps))
        (.glPopMatrix gl)))

(defn create-event-proxy [w h]
  (proxy [GLEventListener] []
    (display [^GLAutoDrawable drawable]
      (let [^GL2 gl (.getGL drawable)
            ^GLUgl2 glu (GLUgl2.)
            context @gl-context
            camera (:camera context)
            objs (:meshes context)]

        (.glClearColor gl (Float. 1.0) 1.0 1.0 1.0)
        (.glClear gl (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))

        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) 1.0 10.0 600.0)
        (apply glu-look-at
               (concat [glu]
                       (:eye camera) (:center camera) [0.0 1.0 0.0]))

        (.glScalef gl (float -1.0) 1.0 1.0)

        (doseq [meshes-from-b3d objs]
          (doseq [mesh meshes-from-b3d]
            (gl-render-mesh gl mesh)))

        (gl-draw-axis gl)
        (display-fps gl)))
    (displayChanged [drawable modeChanged deviceChanged] (println "DC"))
    (init [^GLAutoDrawable drawable]
      (.setGL drawable (DebugGL2. (.getGL drawable)))
                                        ;(.setGL drawable (TraceGL2. (.getGL drawable) System/out))
      (let [^GL2 gl (.getGL drawable)
            ^GLUgl2 glu (GLUgl2.)
            aspect (float (/ w h))]

        (.glViewport gl 0 0 w h)

        (.glMatrixMode gl javax.media.opengl.fixedfunc.GLMatrixFunc/GL_MODELVIEW)
        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) aspect 10.0 600.0)
        (.gluLookAt glu
                    100.0 30.0 130.0  ; eye x,y,z
                    0.0 0.0 0.0     ; center x,y,z
                    0.0 1.0 0.0)    ; up direction

        (.glClearDepth gl 1.0)
        (.glEnable gl GL/GL_BLEND)
        (.glEnable gl GL/GL_DEPTH_TEST)
        (.glDepthFunc gl GL/GL_LEQUAL)
                                        ;(.glDisable gl GL/GL_LIGHTING)

                                        ;(.glCullFace gl GL/GL_BACK)
                                        ;(.glEnable gl GL/GL_CULL_FACE)

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


(defn set-looking-at [canvas ex ey ez]
  (dosync
   (ref-set
    gl-context
    (let [context @gl-context
          camera (:camera context)
          updated-camera (assoc camera :eye [ex ey ez])]
      (assoc context :camera updated-camera))))
  '())

(def canvas (first (make-canvas)))
(def anim (FPSAnimator. canvas 10))
(.start anim)

(defn set-center [canvas x y z]
  (dosync
   (ref-set
    gl-context
    (let [context @gl-context
          camera (:camera context)
          updated-camera (assoc camera :center [x y z])]
      (assoc context :camera updated-camera))))
  '())

;(set-looking-at canvas 20.0 20.0 -50.0)
;(set-looking-at canvas 1.0 10.0 -50.0)
;(set-center canvas 0.0 0.0 10.0)
;(set-center canvas 0.0 0.0 120.0)
;(set-looking-at canvas 1.0 5.0 60.0)
(set-center canvas 0.0 0.0 400.0)
(set-looking-at canvas 1.0 5.0 10.0)
;(set-center canvas 0.0 0.0 400.0)
;(set-looking-at canvas 1.0 5.0 -10.0)
;(set-looking-at canvas 1.0 5.0 50.0)
(set-looking-at canvas 1.0 3.0 60.0)
;(set-looking-at canvas 1.0 3.0 10.0)
(set-center canvas 40.0 0.0 500.0)
(set-looking-at canvas 23.0 3.0 400.0)
