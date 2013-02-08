(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.route :as route]
   [opengl.models :as m]
   [opengl.geom :as geom]
   [opengl.builder :as builder]]
  [:import
   (javax.swing JFrame)
   (javax.media.opengl GLCapabilities GLDrawableFactory GLProfile GLEventListener GL GL2 GL2GL3 DebugGL2)
   (javax.media.opengl.awt GLCanvas)
   (javax.media.opengl.glu.gl2 GLUgl2)
   (com.jogamp.opengl.util FPSAnimator)]
  (:gen-class))

(def textures (ref {}))

;(def println-d println)
(def println-d (fn [& more]))

(defn gl-create-textures [^GL2 gl n]
  (if (> n 0)
    (let [tids (int-array n)]
      (.glGenTextures gl n tids 0)
      tids)
    []))

(defn gl-create-texture [^GL2 gl]
  (first (gl-create-textures gl 1)))

(defn gl-bind-texture-to-bmp [gl tid bmp]
  (println-d "Bound " tid " to " (:file bmp))
  (let [width (:width bmp)
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
    tid))

(defn gl-enable-texture-2d [gl]
  (println-d ".glEnable T2D")
  (.glEnable gl GL/GL_TEXTURE_2D)
  ;(.glTexEnv gl GL2/GL_TEXTURE_ENV GL2/GL_TEXTURE_ENV_MODE GL2/GL_MODULATE)
  )

(defn gl-bind-current-texture [gl texture]
  (let [tid (:gl-tid texture)]
    (if tid
      (println-d ".glBind" tid)
      (.glBindTexture gl GL/GL_TEXTURE_2D tid))))

(defn gl-disable-texture [gl]
  (println-d ".glDisable")
  (.glDisable gl GL/GL_TEXTURE_2D))

(defn gl-load-single-texture [gl file]
  (println-d "Load texture " file)
  (gl-bind-texture-to-bmp gl
                          (gl-create-texture gl)
                          (core/bmp-meta-read-file file)))

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


(defn gl-render-vertex [gl vertex]
  (let [tcoord (:texture-coordinate vertex)
        [x y z] (:coordinate vertex)]

    (.glNormal3d gl 1 0 0)

    (if tcoord
      (.glTexCoord2f gl (first tcoord) (second tcoord)))

    (.glVertex3f gl x y z)))

(defn gl-render-face [gl face]
  (let [material (:material face)
        texture-info (mutable-create-or-get-texture-for-material gl material)
        color (:color texture-info)
        verts (:verts face)]
    (when (:gl-tid texture-info)
      (gl-enable-texture-2d gl)
      (gl-bind-current-texture gl texture-info))

    (println-d ".glBegin")

    (if color
      (let [[r g b] color]
        (.glColor3f gl r g b)))

    (.glPolygonMode gl GL/GL_FRONT_AND_BACK GL2GL3/GL_LINE)
    ;(.glPolygonMode gl GL/GL_BACK  GL2GL3/GL_FILL)

    (.glBegin gl GL2/GL_POLYGON)


    (doseq [vert verts]
      (gl-render-vertex gl vert))

    (println-d ".glEnd")
    (.glEnd gl)

    (if texture-info
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
              (gl-bind-texture-to-bmp gl tid (core/bmp-meta-read-file file)))
       not-loaded-files texture-ids))))

(defn gl-render-mesh [gl mesh]
  (doseq [face mesh]
    (gl-preload-textures gl
     (filter identity
             (flatten
              (map textures-in-vertex face))))
    (gl-render-face gl face)))

;(def q-mesh (core/b3d-parse-file (java.io.File. "Flushing/Speed30.b3d")))
;(def q-mesh (core/b3d-parse-file (java.io.File. "Flushing/bldg17.b3d")))
;(def q-mesh (core/b3d-parse-file (java.io.File. "Flushing/SheaStadium.b3d")))
(def r
  (route/resolve-symbol-table
   (route/parse-route-file "Flushing/test.csv")))
(def s (:symbol-table r))
(def bv (builder/build-block-vector (:nodes r) 25))
(def iblock (nth bv 3))
(def ctxt
  {:rails
   {0 {:offset [0.0 0.0]
       :type (get s "rail9")}
    1 {:offset [-12.0 0.0]
       :type (get s "rail8")}}

   :walls
   [{:type (get s "walll5")
     :rail 0 }
    {:type (get s "wallr5")
     :rail 1}
    ]})

(def objs
  (apply concat (builder/create-objects-for-block ctxt iblock s (geom/transform-create 0 0 0) [0 0 0] [0 0 0])))


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

(def gl-context
  (ref {:camera {:eye [100.0 30.0 130.0]
                 :center [0.0 0.0 0.0]}
        :meshes (filter identity objs)
    }))

(defn glu-look-at [glu ex ey ez cx cy cz ux uy uz]
  (.gluLookAt glu ex ey ez cx cy cz ux uy uz))

(defn create-event-proxy [w h]
  (proxy [GLEventListener] []
    (display [drawable]
      (let [gl (.getGL drawable)
            glu (GLUgl2.)
            context @gl-context
            camera (:camera context)
            objs (:meshes context)]

        (.glClearColor gl (Float. 1.0) 1.0 1.0 1.0)
        (.glClear gl (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))
        ;; Clear texture cache
        (dosync
         (ref-set textures {}))

        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) 1.0 10.0 800.0)
        (apply glu-look-at
               (concat [glu]
                       (:eye camera) (:center camera) [0.0 1.0 0.0]))

        (doseq [mesh objs]
          (gl-render-mesh gl mesh))

        (gl-draw-axis gl)))
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
  '())

(set-looking-at canvas 20.0 20.0 -50.0)
