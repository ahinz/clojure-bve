(ns opengl.route-test
  [:require
   [opengl.util :as u]
   [opengl.models :as m]]
  (:use [clojure.test]
        [opengl.route]))

(deftest initial-token-parse
  (testing "basic line parsing"
    (is
     (=
      (:nodes
       (#'opengl.route/parse-string
        (u/text
         "With Route"
         ".Comment New York City Transit Authority IRT 7 Line"
         ".Timetable Times Square Manhattan to Main Street Flushing Queens"
         ""
         ".DeveloperID 090957"
         "")))
      [{:type :with :body "route" :linenum 1 :line "With Route" :file nil}
       {:type "comment"
        :prefix nil
        :arg nil
        :suffix nil
        :body "New York City Transit Authority IRT 7 Line"
        :linenum 2
        :line ".Comment New York City Transit Authority IRT 7 Line"
        :file nil}
       {:type "timetable" :linenum 3
        :line ".Timetable Times Square Manhattan to Main Street Flushing Queens"
        :prefix nil
        :arg nil
        :suffix nil
        :body "Times Square Manhattan to Main Street Flushing Queens"
        :file nil}
       {:type "developerid"
        :line ".DeveloperID 090957" :linenum 5
        :prefix nil
        :arg nil
        :suffix nil
        :body "090957"
        :file nil}])))

  (testing "more basic line parsing"
    (is
     (=
      (:nodes
       (#'opengl.route/parse-string
        (u/text
         ".folder r62,"
         "route.runinterval 240"
         ".rail(2) 0,"
         "")))
      [{:type "folder"
        :line ".folder r62," :linenum 1
        :prefix nil
        :arg nil
        :suffix nil
        :body "r62"
        :file nil}
       {:type "runinterval"
        :line "route.runinterval 240" :linenum 2
        :prefix "route"
        :arg nil
        :suffix nil
        :body "240"
        :file nil}
       {:type "rail"
        :line ".rail(2) 0," :linenum 3
        :prefix nil
        :arg "2"
        :suffix nil
        :body "0"
        :file nil}])))

  (testing "suffix, don't strip comments"
    (is
     (=
      (:nodes
       (#'opengl.route/parse-string
        (u/text
         ""
         "with structure"
         ""
         ".rail(7).suffix Flushing\\oSlab.b3d;Half Tie type,"
         ".walll(1) Flushing\\TunnelL.b3d;Tunnel Wall and Ceiling (Left),"
         "")))
      [{:type :with
        :line "with structure" :linenum 2
        :body "structure"
        :file nil}
       {:type "rail"
        :line ".rail(7).suffix Flushing\\oSlab.b3d;Half Tie type,"
        :linenum 4
        :prefix nil
        :arg "7"
        :suffix "suffix"
        :body "Flushing\\oSlab.b3d;Half Tie type"
        :file nil}
       {:type "walll"
        :line ".walll(1) Flushing\\TunnelL.b3d;Tunnel Wall and Ceiling (Left),"
        :linenum 5
        :prefix nil
        :arg "1"
        :suffix nil
        :body "Flushing\\TunnelL.b3d;Tunnel Wall and Ceiling (Left)"
        :file nil}])))

  (testing "errors"
    (is
     (=
      (:errors
       (#'opengl.route/parse-string
        (u/text
         ""
         "whatwith structure"
         ""
         "invalid-line-is-bad.rail[7).suffix Flushing\\oSlab.b3d;Half Tie type,"
         "")))
      [{:linenum 2
        :line "whatwith structure"
        :error (get errors :bad-line)}
       {:linenum 4
        :line "invalid-line-is-bad.rail[7).suffix Flushing\\oSlab.b3d;Half Tie type,"
        :error (get errors :bad-line)}])))

  (testing "prefix lines"
    (let
        [line "125,.rail 1;-9.500000;0.000000;8,.freeobj 0;6;-1;0;0,.curve -800;0,"]
      (is
       (=
        (:nodes
         (#'opengl.route/parse-string
          line))
        [{:type :track-ref
          :line line :linenum 1
          :body "125"
          :file nil}
         {:type "rail"
          :line line
          :linenum 1
          :prefix nil
          :arg nil
          :suffix nil
          :body "1;-9.500000;0.000000;8"
          :file nil}
         {:type "freeobj"
          :line line
          :linenum 1
          :prefix nil
          :arg nil
          :suffix nil
          :body "0;6;-1;0;0"
          :file nil}
         {:type "curve"
          :line line
          :linenum 1
          :prefix nil
          :arg nil
          :suffix nil
          :body "-800;0"
          :file nil}])))))

(deftest track-ref-and-prefix-lines
  (testing "prefix"
    (is
     (=
      (:nodes
       (#'opengl.route/associate-with-blocks
         {:nodes
          [{:type :with :body "route" :linenum 1 :line "With Route" :file nil}
           {:type "comment"
            :prefix nil
            :arg nil
            :suffix nil
            :body "New York City Transit Authority IRT 7 Line"
            :linenum 2
            :line ".Comment New York City Transit Authority IRT 7 Line"
            :file nil}
           {:type "timetable" :linenum 3
            :line ".Timetable Times Square Manhattan to Main Street Flushing Queens"
            :prefix nil
            :arg nil
            :suffix nil
            :body "Times Square Manhattan to Main Street Flushing Queens"
            :file nil}
           {:type :with :body "whatever" :linenum 4 :line "With whatever" :file nil}
           {:type "developerid"
            :line ".DeveloperID 090957" :linenum 5
            :prefix nil
            :arg nil
            :suffix nil
            :body "090957"
            :file nil}]}))
      [{:type "comment"
        :prefix "route"
        :arg nil
        :suffix nil
        :body "New York City Transit Authority IRT 7 Line"
        :linenum 2
        :line ".Comment New York City Transit Authority IRT 7 Line"
        :file nil}
       {:type "timetable" :linenum 3
        :line ".Timetable Times Square Manhattan to Main Street Flushing Queens"
        :prefix "route"
        :arg nil
        :suffix nil
        :body "Times Square Manhattan to Main Street Flushing Queens"
        :file nil}
       {:type "developerid"
        :line ".DeveloperID 090957" :linenum 5
        :prefix "whatever"
        :arg nil
        :suffix nil
        :body "090957"
        :file nil}])))

  (testing "prefix lines and with blocks"
    (is
     (=
      (:nodes
       (#'opengl.route/associate-track-refs
        (#'opengl.route/associate-with-blocks
          {:nodes
           [{:type :with :body "route" :linenum 1 :line "With Route" :file nil}
            {:type :track-ref
             :line "" :linenum 1
             :body "125"
             :file nil}
            {:type "rail"
             :line ""
             :linenum 1
             :prefix "thing"
             :arg nil
             :suffix nil
             :body "1;-9.500000;0.000000;8"
             :file nil}
            {:type "freeobj"
             :line ""
             :linenum 1
             :prefix nil
             :arg nil
             :suffix nil
             :body "0;6;-1;0;0"
             :file nil}
            {:type "curve"
             :line ""
             :linenum 1
             :prefix nil
             :arg nil
             :suffix nil
             :body "-800;0"
             :file nil}]})))

      [{:type "rail"
        :line ""
        :linenum 1
        :track-ref 125
        :prefix "thing"
        :arg nil
        :suffix nil
        :body "1;-9.500000;0.000000;8"
        :file nil}
        {:type "freeobj"
         :line ""
         :linenum 1
         :track-ref 125
         :prefix "route"
         :arg nil
         :suffix nil
         :body "0;6;-1;0;0"
         :file nil}
        {:type "curve"
         :line ""
         :linenum 1
         :track-ref 125
         :prefix "route"
         :arg nil
         :suffix nil
         :body "-800;0"
         :file nil}]))))

(deftest parse-route-section
  (testing "route data is captured"
    (let [context
          (#'opengl.route/associate-track-refs
           (#'opengl.route/associate-with-blocks
             (#'opengl.route/parse-string
              (u/text
               "With Route"
               ".Comment New York City Transit Authority IRT 7 Line"
               ".Timetable Times Square Manhattan to Main Street Flushing Queens"
               ""
               ".DeveloperID 090957"
               ""))))
          parsed (#'opengl.route/parse-nodes-in-context context)]
      (is
       (=
        (:route parsed)
        {:comment "New York City Transit Authority IRT 7 Line"
         :timetable "Times Square Manhattan to Main Street Flushing Queens"
         :developerid "090957"}
        )))))

(deftest parse-structure-section
    (let [context
          (#'opengl.route/associate-track-refs
           (#'opengl.route/associate-with-blocks
             (#'opengl.route/parse-string
              (u/text
               ""
               "with structure"
               ""
               ".rail(7).suffix Flushing\\oSlab.b3d;Half Tie type,"
               ".walll(1) Flushing\\TunnelL.b3d;Tunnel Wall and Ceiling (Left),"
               "")
              {:debug-symbols true})))
          context (#'opengl.route/parse-nodes-in-context context)]
  (testing "structure elements are saved to symbol table"
    (is
     (= (symbol-from-context context "rail" 7) "Flushing/oSlab.b3d"))
    (is
     (= (symbol-from-context context "walll" 1) "Flushing/TunnelL.b3d")))))


(deftest parse-blocks
    (let [context
          (#'opengl.route/parse-nodes-in-blocks-in-context
           (#'opengl.route/parse-nodes-in-context
            (#'opengl.route/create-blocks-in-context
             (#'opengl.route/associate-track-refs
              (#'opengl.route/associate-with-blocks
                (#'opengl.route/parse-string
                 (u/text
                  ""
                  "with structure"
                  ""
                  ".rail(7).suffix Flushing\\oSlab.b3d;Half Tie type,"
                  ".rail(8) Rail8.b3d"
                  ".rail(9) Rail9.b3d"
                  ".walll(1) Flushing\\TunnelL.b3d;Tunnel Wall and Ceiling (Left),"
                  ".walll(5) Walll5.b3d"
                  ".wallr(5) Wallr5.b3d"
                  ".freeobj(1) Whatever.b3d"
                  ".freeobj(10) Whatever10.b3d"
                  ".freeobj(101) Whatever101.b3d"
                  ""
                  " with track"
                  " .height 0.3,"
                  " 0,.back 1,.height 0.3,.pitch 0,.ground 1,.railstart 1;-12.000000;0.000000;8,.railtype 1;8,.railtype 0;9,"
                  " 0,.sta Times Square;4.3230;4.3300;0;1;0;;;20;45;Flushing\\Square.wav,"
                  " 25,.form 0;1;0;0,"
                  " 50,.form 0;1;0;0,"
                  " 75,.form 0;1;1;0,.wall 0;1;5,.wall 1;-1;5,.stop 0,"
                  "")
                 {:debug-symbols true :block-size 25.0}))))))]
      (testing "No errors"
        (is (= (:errors context) '())))

      (testing "correctly annotate blocks"
        (is
         (= (:blocks context)
            [{:start-ref 0 :end-ref 25
              :height 0.3 :pitch 0 :ground 1
              :rails {0 {:start [0.0 0.0]
                         :end   [0.0 0.0]
                         :prototype "Rail9.b3d"}
                      1 {:start [-12.0 0.0]
                         :end   [-12.0 0.0]
                         :prototype "Rail8.b3d"}}}
             {:start-ref 25 :end-ref 50
              :rails {0 {:start [0.0 0.0]
                         :end   [0.0 0.0]
                         :prototype "Rail9.b3d"}
                      1 {:start [-12.0 0.0]
                         :end   [-12.0 0.0]
                         :prototype "Rail8.b3d"}}
              :form {:rail1 0 :rail2 1
                     :form-idx 0 :roof-idx 0}}
             {:start-ref 50 :end-ref 75
              :rails {0 {:start [0.0 0.0]
                         :end   [0.0 0.0]
                         :prototype "Rail9.b3d"}
                      1 {:start [-12.0 0.0]
                         :end   [-12.0 0.0]
                         :prototype "Rail8.b3d"}}
              :form {:rail1 0 :rail2 1
                     :form-idx 0 :roof-idx 0}}
             {:start-ref 75 :end-ref 100
              :rails {0 {:start [0.0 0.0]
                         :end   [0.0 0.0]
                         :prototype "Rail9.b3d"
                         :walls ["Wallr5.b3d"]}
                      1 {:start [-12.0 0.0]
                         :end   [-12.0 0.0]
                         :prototype "Rail8.b3d"
                         :walls ["Walll5.b3d"]}}
              :form {:rail1 0 :rail2 1
                     :form-idx 0 :roof-idx 1}}

])))))
