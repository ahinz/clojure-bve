(ns opengl.geom
  (:gen-class))

(defrecord V3f [^float x ^float y ^float z])
(defn v3f [x y z] (V3f. x y z))
(defn v3f-as-vector [v] [(:x v) (:y v) (:z v)])

(def identity-transform
  [(v3f 1.0 0.0 0.0)
   (v3f 0.0 1.0 0.0)
   (v3f 0.0 0.0 1.0)])

(defn- sin-cos [^double a]
  [(Math/sin a) (Math/cos a)])

(defn- get-x [t] (nth t 0))
(defn- get-y [t] (nth t 1))
(defn- get-z [t] (nth t 2))

(defn rotate-with-transform [[x y z] t]
  [(+ (* (:x (get-x t)) x)
      (* (:x (get-y t)) y)
      (* (:x (get-z t)) z))

   (+ (* (:y (get-x t)) x)
      (* (:y (get-y t)) y)
      (* (:y (get-z t)) z))

   (+ (* (:z (get-x t)) x)
      (* (:z (get-y t)) y)
      (* (:z (get-z t)) z))])

(defn rotate
  [[px py pz] [dx dy dz] cosa sina]
  (let [t (/ 1.0 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
        oc (- 1.0 cosa)
        [dx dy dz] [(* dx t) (* dy t) (* dz t)]]
    [(+
      (* (+ cosa (* oc dx dx)) px)
      (* (- (* oc dx dy) (* sina dz)) py)
      (* (+ (* oc dx dz) (* sina dy)) pz))

     (+
      (* (+ cosa (* oc dy dy)) py)
      (* (+ (* oc dx dy) (* sina dz)) px)
      (* (- (* oc dy dz) (* sina dx)) pz))

     (+
      (* (+ cosa (* oc dz dz)) pz)
      (* (- (* oc dx dz) (* sina dy)) px)
      (* (+ (* oc dy dz) (* sina dx)) py))]))


(defn transform-create
  ([yaw pitch roll]
     (transform-create identity-transform yaw pitch roll))

  ([transform yaw pitch roll]
     (let [[sin-yaw cos-yaw] (sin-cos yaw)
           [sin-pitch cos-pitch] (sin-cos (- pitch))
           [sin-roll cos-roll] (sin-cos (- roll))

           [tx ty tz] transform
           [sx sy sz] (v3f-as-vector tx)
           [ux uy uz] (v3f-as-vector ty)
           [dx dy dz] (v3f-as-vector tz)

           [sx sy sz] (rotate [sx sy sz] [ux uy uz] cos-yaw sin-yaw)
           [dx dy dz] (rotate [dx dy dz] [ux uy uz] cos-yaw sin-yaw)
           [ux uy uz] (rotate [ux uy uz] [sx sy sz] cos-pitch sin-pitch)
           [dx dy dz] (rotate [dx dy dz] [sx sy sz] cos-pitch sin-pitch)
           [sx sy sz] (rotate [sx sy sz] [dx dy dz] cos-roll sin-roll)
           [ux uy uz] (rotate [ux uy uz] [dx dy dz] cos-roll sin-roll)]

       [(v3f sx sy sz)
        (v3f ux uy uz)
        (v3f dx dy dz)])))
