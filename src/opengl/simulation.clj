(ns opengl.simulation
  [:require
   [opengl.core :as core]
   [opengl.geom :as geom]
   [opengl.util :as util]
   [opengl.models :as m]
   [opengl.train :as train]
   [opengl.route :as route]]
  (:gen-class))

(set! *warn-on-reflection* true)

(defn base-state []
  {:camera {:eye [20.0 20.0 -50.0]
            :center [0.0 0.0 50.0]}
   :speed 16.2352 ; 2.2352 ; meters/sec
   :train (train/parse-train-file "R44/train.dat")
   :power-notch 0
   :brake-notch 0
   :track-pos 0.0})

(defn- camera-for-position [context track-pos]
  (let [block-index (int (/ track-pos (:block-size context)))
        block (nth (:blocks context) block-index)
        [dx dz] (:direction block)
        [x y z] (:position block)
        y (+ y 3.0)

        delta-z (- track-pos (:start-ref block))
        delta-zc (+ delta-z 10.0)

        [ex ey ez] [(+ (* dx delta-z) x) y (+ (* dz delta-z) z)]
        [cx cy cz] [(+ (* dx delta-zc) x) y (+ (* dz delta-zc) z)]
        ;; [ex ey ez] (:position block)
        ;; [cx cy cz] (:position block)
        ;; cz (+ cz 10.0)
        ;; cy (+ cy 3.0)
        ;; ey (+ ey 3.0)

]
;    (println "eye" ex ey ez "center" cx cy cz)
;    (println (:direction block) (:position block))
    {:eye [ex ey ez]
     :center [cx cy cz]
     :dir (:direction block)
     :rotate (/ (* 180.0 (Math/atan2 dx dz)) Math/PI)}))

(defn update-simulation [context state delta-time]
  (let [block-size (:block-size context)
        speed (:speed state)
        pos (+ (:track-pos state) (* speed delta-time))]

    (assoc state
      :camera (camera-for-position context pos)
      :track-pos pos)))
