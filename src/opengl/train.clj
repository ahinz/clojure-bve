(ns opengl.train
  [:require
   [opengl.core :as core]
   [opengl.util :as util]
   [opengl.b3d :as b3d]]
  (:gen-class))

(defn- tx-exponent [v2 e version]
  (if (= version "BVE1220000")
    (Math/min 4 (- 1  (/ (* v2 (Math/log e)) (Math/log (/ 9.0 4.0)))))
    e))

(defn- as-floats [lines]
  (map (comp float read-string) lines))

(defn- as-ints [lines]
  (map (comp int read-string) lines))

(defn- parse-device-section [lines]
  (let [[ats atc eb cs hb read pa dom dcm] (as-ints lines)
        doors {0 :combo 1 :auto 2 :manual}]
    {:ats (get {-1 [] 0 [:ats-sn] 1 [:ats-sn :ats-p]} ats [])
     :atc (get {0 nil 1 :atc-manual 2 :atc-auto} atc nil)
     :eb (= eb 1)
     :const-speed (= cs 1)
     :hold-brake (= hb 1)
     :readhesion-device read
     :pass-alarm (get {0 nil 1 :single 2 :loop} pa :nil)
     :door-open-mode (get doors dom :combo)
     :door-close-mode (get doors dcm :combo)}))

(defn- parse-car-section [lines]
  (let [[mcm nofc tcm ntc lc fcism wid hei com efa ufa]
        (as-floats lines)
        wid (or wid 2.6)
        hei (or hei 3.6)]
    {:motor-car-mass mcm
     :number-of-motor-cars (int (or nofc 0))
     :trailer-car-mass (or tcm 0.0)
     :number-of-trailer-cars (int (or ntc 0))
     :length-of-a-car lc
     :front-car-is-a-motor (= 0.0 fcism)
     :width-of-a-car wid
     :height-of-a-car hei
     :center-of-mass-height (or com 1.6)
     :exposed-frontal-area (or efa (* 0.6 wid hei))
     :unexposed-frontal-area (or ufa (* 0.2 wid hei))}))

(defn- parse-cab-section [lines]
  (let [[x y z driver-car] (as-floats lines)]
    {:offset [x y z]
     :driver-car (int (or driver-car 0))}))

(defn- parse-handle-section [handles]
  (let [[ht pn bn pnrs] (as-ints handles)]
    {:handle-type (if (= 0 ht) :separate :combined)
     :power-notches pn
     :brake-notches bn
     :power-notch-reduction-steps pnrs}))

(defn- parse-pressure-section [plines]
  (let [[bcsmp bcemp mrminp mrmaxp bpnp] (as-floats plines)]
    {:brake-cylinder-service-maximum-pressure (or bcsmp 480.0)
     :brake-cylinder-emergency-maximum-pressure (or bcemp 480.0)
     :main-reservoir-minimum-pressure (or mrminp 690.0)
     :main-reservoir-maximum-pressure (or mrmaxp 780.0)
     :brake-pipe-normal-pressure (or bpnp 490.0)}))

(defn- parse-brake-section [blines]
  (let [[bt bcs bcspd] (as-floats blines)]
    {:brake-type (nth [:elecro-straight-airbrake
                       :electro-pneumatic-air-brake
                       :air-brake-with-partial-release] (int bt))
     :brake-control-system (nth [:none
                                 :closing-electromagnetic-valve
                                 :delay-including-control] (int bcs))
     :brake-control-speed bcspd}))

(defn- parse-move-section [move-lines]
  (let [[jpu jpd jbu jbd bcu bcd] (as-floats move-lines)]
    {:jerk-power-up (or jpu 1000.0)
     :jerk-power-down (or jpd 1000.0)
     :jerk-brake-up (or jbu 1000.0)
     :jerk-brake-down (or jbd 1000.0)
     :brake-cylinder-up (or bcu 300.0)
     :brake-cylinder-down (or bcd 200.0)}))

(defn- parse-delay-section [delay-lines]
  (let [[dpu dpd dbu dbd] (as-floats delay-lines)]
    {:delay-power-up (or dpu 0.0)
     :delay-power-down (or dpd 0.0)
     :delay-break-up (or dbu 0.0)
     :delay-break-down (or dbd 0.0)}))

(defn- parse-perf-section [perf-lines]
  (let [[dcl cosf _ corr adc] (as-floats perf-lines)]
    {:deceleration (or dcl 1.0)
     :coefficient-of-static-friction (or cosf 0.35)
     :coefficient-of-rolling-resistance (or corr 0.0025)
     :aerodynamic-drag-coefficient (or adc 1.1)}))

(defn- create-accl-fn [accl-lines version]
  (map (fn [ln]
         (let [[a0 a1 v1 v2 e] (as-floats (.split ln ","))
               e (tx-exponent v2 e version)]
           (fn [v]
             (cond
              (< v v1) (+ a0 (* (/ v v1) (- a1 a0)))
              (< v v2) (* v1 (/ a1 v))
              :else (/ (* v1 a1 (Math/pow v2 (- e 1))) (Math/pow v e))))))
       accl-lines))

(defn- parse-sections
  "Given a list of strings that looks like:
   #Section Title 1
   data line 1
   data line 2
   data line 3
   #Section Title 2
   data line 4
   #Section Title 3
   data line 5
   data line 6

   Convert it into a hash like:
   { 'section title 1': ['data line 1', 'data line 2', 'data line 3'],
     'section title 2': ['data line 4'],
     'section title 3': ['data line 5', 'data line 6'] }"
  [lines sections]
  (if (> (count lines) 0)
    (let [section (.toLowerCase (.substring (first lines) 1))
          pred #(not (.startsWith % "#"))
          rest-of-lines (drop-while pred (rest lines))
          lines-in-section (take-while pred (rest lines))]
      (recur
       rest-of-lines
       (assoc sections section lines-in-section)))
    sections))

(defn parse-train-string [train file-name]
  (let [lines (.split train "\n")
        version (first lines)
        sections (assoc
                     (parse-sections (map #(.trim %) (rest lines)) {})
                   "VERSION" version)]
    {:accl (create-accl-fn (get sections "acceleration") version)
     :decl (parse-perf-section
            (or (get sections "performance")
                (get sections "deceleration")))
     :delay (parse-delay-section
             (get sections "delay"))
     :move (parse-move-section
            (get sections "move"))
     :brake (parse-brake-section
             (get sections "brake"))
     :pressure (parse-pressure-section
                (get sections "pressure"))
     :handle (parse-handle-section
              (get sections "handle"))
     :cab (parse-cab-section
           (or (get sections "cab")
               (get sections "cockpit")))
     :car (parse-car-section
           (get sections "car"))
     :device (parse-device-section
              (get sections "device"))}))

(defn parse-train-file [file-path]
  (parse-train-string (slurp file-path) file-path))
