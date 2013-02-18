(defproject opengl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot [opengl.core
        opengl.builder
        opengl.util
        opengl.geom
        opengl.models
        opengl.opengl
        opengl.route
        opengl.objects
        opengl.b3d]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojars.toxi/jogl "2.0.0-rc10"]
                 ])
