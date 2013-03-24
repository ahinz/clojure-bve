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
   :train-viz (train/parse-panel-cfg-file "R44/" "panel.cfg")
   :power-notch 0
   :brake-notch 0
   :track-pos 0.0})
(def kmh-in-mps 0.277778)

(defn- next-speed [ctxt delta-time]
  (cond
   (> (get-in ctxt [:simulation-state :power-notch]) 0)
   (let [spd (get-in ctxt [:simulation-state :speed])
         accl-fns (get-in ctxt [:simulation-state :train :accl])
         accl-fn (nth accl-fns (- (get-in ctxt [:simulation-state :power-notch]) 1))
         accl (accl-fn spd)]
     (+ spd (* accl delta-time kmh-in-mps)))

   (> (get-in ctxt [:simulation-state :brake-notch]) 0)
   (let [spd (get-in ctxt [:simulation-state :speed])
         brake-notch (get-in ctxt [:simulation-state :brake-notch])
         decl (* brake-notch 0.1)]
     (max 0 (- spd (* decl delta-time))))

   :else
   (get-in ctxt [:simulation-state :speed])))

(defn incr-power-notch [ctxt]
  (assoc-in ctxt [:simulation-state :power-notch]
            (min (+ 1 (get-in ctxt [:simulation-state :power-notch]))
                 (get-in ctxt [:simulation-state :train :handle :power-notches]))))

(defn decr-power-notch [ctxt]
  (assoc-in ctxt [:simulation-state :power-notch]
            (max (- (get-in ctxt [:simulation-state :power-notch]) 1)
              0)))

(defn incr-brake-notch [ctxt]
  (assoc-in ctxt [:simulation-state :brake-notch]
            (min (+ 1 (get-in ctxt [:simulation-state :brake-notch]))
                 (get-in ctxt [:simulation-state :train :handle :brake-notches]))))

(defn decr-brake-notch [ctxt]
  (assoc-in ctxt [:simulation-state :brake-notch]
            (max (- (get-in ctxt [:simulation-state :brake-notch]) 1)
                 0)))

(defn incr-combined [ctxt]
  (if (= 0 (get-in ctxt [:simulation-state :brake-notch]))
    (incr-power-notch ctxt)
    (decr-brake-notch ctxt)))

(defn decr-combined [ctxt]
  (if (= 0 (get-in ctxt [:simulation-state :power-notch]))
    (incr-brake-notch ctxt)
    (decr-power-notch ctxt)))

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

(defn update-simulation [context sim-context delta-time]
  (let [block-size (:block-size context)
        speed (get-in sim-context [:simulation-state :speed])
        upd-speed (next-speed sim-context delta-time)
        pos (+ (get-in sim-context [:simulation-state :track-pos] ) (* speed delta-time))]

    (update-in sim-context [:simulation-state]
               #(assoc %
                  :speed upd-speed
                  :camera (camera-for-position context pos)
                  :track-pos pos))))
