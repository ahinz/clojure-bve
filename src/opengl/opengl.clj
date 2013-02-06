(ns opengl.opengl
  [:require
   [opengl.core :as core]
   [opengl.route :as route]]
  [:import
   (javax.swing JFrame)
   (javax.media.opengl GLCapabilities GLDrawableFactory GLProfile GLEventListener GL GL2)
   (javax.media.opengl.awt GLCanvas)
   (javax.media.opengl.glu.gl2 GLUgl2)]
  (:gen-class))

(defn create-event-proxy [w h]
  (proxy [GLEventListener] []
    (display [drawable]
      (let [gl (.getGL drawable)
            glu (GLUgl2.)]
        (.gluOrtho2D glu 0.0 450.0 0.0 375.0)
        (doto gl
          (.glClear GL/GL_COLOR_BUFFER_BIT)
          (.glBegin GL2/GL_POLYGON)
          (.glVertex2i 300 50)
          (.glVertex2i 350 60)
          (.glVertex2i 375 100)
          (.glVertex2i 325 115)
          (.glVertex2i 300 75)
          (.glEnd)
          )))
    (displayChanged [drawable modeChanged deviceChanged] (println "DC"))
    (init [drawable]
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
