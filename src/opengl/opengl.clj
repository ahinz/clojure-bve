(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.objects :as objects]
   [opengl.route :as route]
   [opengl.models :as m]
   [opengl.geom :as geom]
   [opengl.builder :as builder]
   [opengl.simulation :as s]]
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

(def gl-context
  (ref {:last-time (System/nanoTime)
        :meshes (to-array (flatten (filter identity objects/objs)))
        :simulation-state (s/base-state)
    }))

(defn- time-elapsed-seconds []
  (let [cur-time (System/nanoTime)
        last-time (or (:last-time @gl-context) cur-time)
        diff (- cur-time last-time)]
    (dosync
     (ref-set
      gl-context
      (assoc @gl-context :last-time cur-time)))
    (/ diff 1.0e9)))

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

(defn- gl-set-blend-mode [^GL2 gl blend]
  (when (not= (:last-blend @gl-context) blend)
    (if blend
      (if (= blend "additive")
        (.glBlendFunc gl GL/GL_SRC_ALPHA GL/GL_ONE)
        (.glBlendFunc gl GL/GL_ONE GL/GL_ZERO))
      (.glBlendFunc gl GL/GL_ONE GL/GL_ZERO))
    (dosync
     (ref-set
      gl-context
      (assoc @gl-context :last-blend blend)))))

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

    (gl-set-blend-mode gl (:mode blend-mode))

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

(def display-lists
  (ref {}))

(defn gl-render-mesh [gl mesh]
  (doseq [face (:faces mesh)]
    (gl-preload-textures gl
     (filter identity
             (flatten
              (map textures-in-vertex face))))
    (gl-render-face gl face)))

(defn- create-plane [normal pt]
  (let [[nx ny nz] (geom/vector-normalize normal)]
    (double-array [nx ny nz (- (geom/vector-inner-product [nx ny nz] pt))])))

(def ang2rad (/ Math/PI 180.0))
;; From:
;; http://www.lighthouse3d.com/tutorials/view-frustum-culling/geometric-approach-implementation/
(defn- create-frustum-planes
  [angle ratio nearD farD p [dx dz] [ux uy uz]]
  (let [[px py pz] p
        Z (geom/vector-normalize [(- dx) 0.0 (- dz)])
        X (geom/vector-normalize
           (geom/vector-cross [ux uy uz] Z))
        Y (geom/vector-cross Z X)

        tang (Math/tan (* ang2rad angle 0.5))
        nh (* nearD tang)
        nw (* nh ratio)

        fh (* farD tang)
        fw (* fh ratio)

        ;; center of near and far planes
        nc (geom/vector-sub p (geom/vector-mult-scalar Z nearD))
        fc (geom/vector-sub p (geom/vector-mult-scalar Z farD))

        pt (geom/vector-add nc (geom/vector-mult-scalar Y nh))
        norm (geom/vector-cross
              (geom/vector-normalize
               (geom/vector-sub pt p)) X)
        top (create-plane norm pt)

        pt (geom/vector-sub nc (geom/vector-mult-scalar Y nh))
        norm (geom/vector-cross X
              (geom/vector-normalize
               (geom/vector-sub pt p)))
        bot (create-plane norm pt)

        pt (geom/vector-sub nc (geom/vector-mult-scalar X nw))
        norm (geom/vector-cross
              (geom/vector-normalize
               (geom/vector-sub pt p)) Y)
        left (create-plane norm pt)

        pt (geom/vector-add nc (geom/vector-mult-scalar X nw))
        norm (geom/vector-cross Y
              (geom/vector-normalize
               (geom/vector-sub pt p)))
        right (create-plane norm pt)

        far (create-plane Z fc)
        near (create-plane (geom/vector-sub [0.0 0.0 0.0] Z) nc)]
    (object-array [top bot left right near far])))

(defn- distance-from-plane2 [^doubles n ^double d ^doubles p]
  (+ d
     (+ (* (aget n 0) (aget p 0))
        (+ (* (aget n 1) (aget p 1))
           (+ (* (aget n 2) (aget p 2)))))))

(defn- distance-from-plane [[normal d] x]
  (+ d (geom/vector-inner-product normal x)))

(def OutsideOfSphere -1)
(def IntersectsSphere 0)
(def InsideOfSphere 1)

(defn- sphere-in-plane ^long [n d c radius]
  (let [^float dist (distance-from-plane2 n d c)]
    (cond
     (< dist (- radius))
     OutsideOfSphere

     (< (Math/abs dist) radius)
     IntersectsSphere

     :else
     InsideOfSphere)))

(defn- sphere-in-planes [^objects planes ^long offset ^doubles c ^double radius]
  (if (>= offset (alength planes))
    true
    (let [^doubles plane (aget planes offset)
          d (aget plane 3)
          contains (sphere-in-plane plane d c radius)]
      (cond
       (== contains IntersectsSphere)
       true

       (== contains InsideOfSphere)
       (recur planes (+ 1 offset) c radius)

       :else ;; OutsideOfSphere
       false))))

(defn gl-render-mesh-or-display-list [^GL2 gl ^Object mesh]
  (if-let [^Integer display-list (get @display-lists (.hashCode mesh))]
    (.glCallList gl display-list)
    (let [display-list (.glGenLists gl 1)]
      (.glNewList gl display-list GL2/GL_COMPILE)
      (gl-render-mesh gl mesh)
      (.glEndList gl)
      (.glCallList gl display-list)
      (dosync
       (ref-set
        display-lists
        (assoc @display-lists (.hashCode mesh) display-list))))))

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
            sim (s/update-simulation
                 objects/context
                 (:simulation-state @gl-context)
                 (time-elapsed-seconds))
            camera (:camera sim)
            objs (:meshes context)]

        (dosync
         (ref-set gl-context
                  (assoc @gl-context :simulation-state sim)))

        (.glClearColor gl (Float. 1.0) 1.0 1.0 1.0)
        (.glClear gl (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))

        (.glLoadIdentity gl)
        ;(.gluPerspective glu (Float. 25.0) 1.0 10.0 600.0)
        ;; (apply glu-look-at
        ;;        (concat [glu]
        ;;                [0.0 0.0 0.0] (:center camera) [0.0 1.0 0.0]))

        (.glScalef gl (float 1.0) 1.0 -1.0)

        (.glRotatef gl (- (:rotate camera)) 0.0 1.0 0.0)
        (let [[^float ex ey ez] (:eye camera)]
          (.glTranslatef gl (- ex) (- ey) (- ez)))

        (let [planes (create-frustum-planes
                      (:angle @gl-context)
                      (:aspect @gl-context)
                      (:near-dist @gl-context)
                      (:far-dist @gl-context)
                      (:eye camera)
                      (:dir camera)
                      [0.0 1.0 0.0])
              culled (reduce
                      (fn [culled mesh]
                        (let [^opengl.models.Sphere s (:bounding-sphere mesh)]
                          (if (sphere-in-planes planes 0 (.center s) (.radius s))
                           (do
                             (gl-render-mesh-or-display-list gl mesh)
                             culled)
                           (+ 1 culled))))
                      0 objs)]

          (println
           (format
            "Culled %s out of %s (%s)"
            culled (count objs) (* 100.0 (/ culled (count objs)))))
          )

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

        (.glMatrixMode gl javax.media.opengl.fixedfunc.GLMatrixFunc/GL_PROJECTION)
        (.glLoadIdentity gl)
        (.gluPerspective glu (Float. 25.0) aspect 10.0 300.0)

        (dosync (ref-set gl-context (assoc @gl-context
                                      :angle 25.0
                                      :aspect aspect
                                      :near-dist 10.0
                                      :far-dist 300.0)))

        (.glMatrixMode gl javax.media.opengl.fixedfunc.GLMatrixFunc/GL_MODELVIEW)
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


(defn set-looking-at [canvas x y z]
  (dosync
   (ref-set
    gl-context
    (let [context @gl-context
          sim-state (:simulation-state context)
          camera (:camera sim-state)
          updated-sim-state (assoc sim-state
                              :camera
                              (assoc camera
                                :eye [x y z]))]
      (assoc context :simulation-state updated-sim-state))))
  '())

;; (def canvas (first (make-canvas)))
;; (def anim (FPSAnimator. canvas 10))
;; (.start anim)

(defn set-center [canvas x y z]
  (dosync
   (ref-set
    gl-context
    (let [context @gl-context
          sim-state (:simulation-state context)
          camera (:camera sim-state)
          updated-sim-state (assoc sim-state
                              :camera
                              (assoc camera
                                :center [x y z]))]
      (assoc context :simulation-state updated-sim-state))))
  '())

(defn -main []
  (dosync (ref-set display-lists {}))
  (let [^GLCanvas canvas (first (make-canvas))
        anim (FPSAnimator. canvas 60)]
    (.start anim)
    (dosync (ref-set gl-context
                     (assoc @gl-context :simulation-state (s/base-state))))
    ;(set-center canvas 22.0 0.0 400)
    ;(set-looking-at canvas 15.0 3.0 340.0)
    ))
