(ns opengl.objects-test
  [:require
   [opengl.util :as u]
   [opengl.geom :as geom]
   [opengl.models :as m]
   [opengl.route :as r]]
  (:use [clojure.test]
        [opengl.objects]))

(defn- get-track-geom-info [block]
  {:track-transform (:track-transform block)
   :position (:position block)
   :direction (:direction block)})

(defn- get-geom-info [block]
  {:track-transform (:track-transform block)
   :position (:position block)
   :direction (:direction block)
   :rails
   (into {}
         (map (fn [[railidx rail]]
                [railidx
                 {:rail-transform (:rail-transform rail)}])
              (:rails block)))})

(deftest geoms
  (testing "straight lines"
    (is
     (=
      (map get-geom-info
           (:blocks
            (#'opengl.objects/create-geometries-for-blocks-in-context
             (r/parse-route-string
              (u/text
               "with structure"
               ".rail(8) Rail8.b3d"
               ".rail(9) Rail9.b3d"
               ""
               "with track"
               "0,.railstart 1;-12.0;0.0;8,.railtype 0;9"
               "25,.rail 1;0.0;0.0;8"
               ) nil))))
      [{:track-transform geom/identity-transform
        :position [0.0 0.0 0.0]
        :direction [0.0 1.0]
        :rails {1 {:rail-transform geom/identity-transform}
                0 {:rail-transform geom/identity-transform}}}
       {:track-transform geom/identity-transform
        :position [0.0 0.0 25.0]
        :direction [0.0 1.0]
        :rails {1 {:rail-transform geom/identity-transform}
                0 {:rail-transform geom/identity-transform}}}])))
  (let [context
        (#'opengl.objects/create-geometries-for-blocks-in-context
         (r/parse-route-string
          (u/text
           "with structure"
           ".rail(8) Rail8.b3d"
           ".rail(9) Rail9.b3d"
           ""
           "with track"
           "0,.railstart 1;-12.0;0.0;8,.railtype 0;9"
           "25,.curve 800.0"
           "50,.curve 0.0"
           "75,.rail 1;0.0;0.0;8"
           ) nil))
        blocks (:blocks context)
        dir-after-curve (:direction (nth blocks 1))
        track-tx-after-curve (:track-transform (nth blocks 1))]
    (testing "no errors"
      (is
       (= (:errors context)
        '())))

    (testing "curved lines - track transforms"
      (is
       (=
        (map #(:track-transform %)
             (map get-geom-info
                  (:blocks context)))
        [geom/identity-transform
         track-tx-after-curve
         track-tx-after-curve
         track-tx-after-curve])))))
