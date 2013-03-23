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

(defn- parse-perf-section [perf-lines]
  (let [[dcl cosf _ corr adc] (map (comp float read-string) perf-lines)]
    {:deceleration (or dcl 1.0)
     :coefficient-of-static-friction (or cosf 0.35)
     :coefficient-of-rolling-resistance (or corr 0.0025)
     :aerodynamic-drag-coefficient (or adc 1.1)}))

(defn- create-accl-fn [accl-lines version]
  (map (fn [ln]
         (let [[a0 a1 v1 v2 e] (map (comp float read-string) (.split ln ","))
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
                (get sections "deceleration")))}))

(defn parse-train-file [file-path]
  (parse-train-string (slurp file-path) file-path))

(def q (parse-train-file "R44/train.dat"))
