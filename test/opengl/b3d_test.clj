(ns opengl.b3d-test
  [:require
   [opengl.util :as u]
   [opengl.models :as m]]
  (:use [clojure.test]
        [opengl.b3d]))

;;; Verify meshbuilders have basic data
;;; when created
(deftest meshbuilder
  (testing "Mesh Builder"
    (is
     (=
      (:meshes (parse-string "[MeshBuilder] ; Mesh"))
      (list (m/create-mesh []))))

    (is
     (=
      (:meshes (parse-string "createmeshbuilder ; Mesh"))
      (list (m/create-mesh []))))))

(deftest vertices
  (testing "Bad vertex lines"
    (is
     (=
      (:errors
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Vertex\t1,2,3"
         "vertex    6,4,5,6"
         "Vertex    7,f,9 ; Comment"
         "AddVertex    10,11,12"
         "")))

      [{:line "vertex    6,4,5,6" :linenum 3
        :error (:vertex-count errors)}
       {:line "Vertex    7,f,9 ; Comment" :linenum 4
        :error (:number errors)}
       {:line "AddVertex    10,11,12" :linenum 5
        :error (:vertex-count errors)}]))))

(deftest faces
  (testing "Correctly built vertex list"
    (is
     (=
      (:meshes
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Vertex 1,2,3"
         "vertex 4,5,6"
         "Vertex 7,8,9"
         "Vertex 10,11,12"
         "Face 0,1,2,3"
         "Face2 1,2,3"
         "AddFace,0,2,3"
         "")))
      (list
       (m/create-mesh
        [(m/create-face
          [(m/create-vertex [1.0 2.0 3.0] nil)
           (m/create-vertex [4.0 5.0 6.0] nil)
           (m/create-vertex [7.0 8.0 9.0] nil)
           (m/create-vertex [10.0 11.0 12.0] nil)]
          (m/create-material
           (m/create-color-set nil)
           (m/create-texture-set nil nil 0.0)
           nil) false)
         (m/create-face
          [(m/create-vertex [4.0 5.0 6.0] nil)
           (m/create-vertex [7.0 8.0 9.0] nil)
           (m/create-vertex [10.0 11.0 12.0] nil)]
          (m/create-material
           (m/create-color-set nil)
           (m/create-texture-set nil nil 0.0)
           nil) true)
         (m/create-face
          [(m/create-vertex [1.0 2.0 3.0] nil)
           (m/create-vertex [7.0 8.0 9.0] nil)
           (m/create-vertex [10.0 11.0 12.0] nil)]
          (m/create-material
           (m/create-color-set nil)
           (m/create-texture-set nil nil 0.0)
           nil) false)])))))

  (testing "Bad face line"
    (is
     (=
      (:errors
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Vertex 1,2,3"
         "Vertex 1,2,3"
         "Vertex 1,2,3"
         "Face    1,2f,3"
         "AddFace,    1,2f,3"
         "Face 1,9,3"
         "AddFace, 6,1,3"
         "")))
      [{:line "Face    1,2f,3" :linenum 5
        :error (:number errors)}
       {:line "AddFace,    1,2f,3" :linenum 6
        :error (:number errors)}
       {:line "Face 1,9,3" :linenum 7
        :error "Vertex #9 not found"}
       {:line "AddFace, 6,1,3" :linenum 8
        :error "Vertex #6 not found"}])))

  (testing "Face outside of mesh builder"
    (is
     (=
      (:errors
       (parse-string
        (u/text
         "Face 1,2,3"
         "AddFace,1,2,3"
         "")))
      [{:line "Face 1,2,3" :linenum 1
        :error (:face-in-not-in-builder errors)}
       {:line "AddFace,1,2,3" :linenum 2
        :error (:face-in-not-in-builder errors)}]
      ))))

(deftest textures
  (testing "Textures are parsed (Load and Loadtexture, take last texture)"
    (is
     (=
      (:meshes
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Vertex 0,1,2"
         "Face 0"
         "Load ignoredMySpecialTexture\\blah.b3d"
         "Loadtexture,mySpecialTexture\\blah.b3d"
         "")))
      [(m/create-mesh
        [(m/create-face
          [(m/create-vertex [0.0 1.0 2.0]  nil)]
          (m/create-material
           (m/create-color-set nil nil nil)
           (m/create-texture-set "/mySpecialTexture/blah.b3d" nil 0.0)
           nil)
          false)])])))

  (testing "Textures are parsed but nothing happens without face"
    (is
     (=
      (:meshes
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Load mySpecialTexture\\blah.b3d"
         "")))
      [(m/create-mesh
        [])])))

  (testing "Texture coordinates are parsed"
    (is
     (=
      (:meshes
       (parse-string
        (u/text
         "[MeshBuilder] ; Mesh"
         "Vertex 1,2,3"
         "Vertex 4,5,6"
         "Face 0,1"
         "Load mySpecialTexture\\blah.b3d"
         "SetTextureCoordinates,0,1,0"
         "Coordinates 1,0,1"
         "")))
      [(m/create-mesh
        [(m/create-face
          [(m/create-vertex [1.0 2.0 3.0] [1.0 0.0])
           (m/create-vertex [4.0 5.0 6.0] [0.0 1.0])]
          (m/create-material
           (m/create-color-set nil nil nil)
           (m/create-texture-set "/mySpecialTexture/blah.b3d" nil 0.0)
           nil)
          false)])])))

  (testing "Texture error cases"
    (is
     (=
      (:errors
       (parse-string
        (u/text
         "Load mySpecialTexture\\blah.b3d"
         "[MeshBuilder] ; Mesh"
         "Vertex 1,2,3"
         "SetTextureCoordinates,1,1,0"
         "SetTextureCoordinates,1a,1,0"
         "Coordinates 1,0,1b"
         "")))
      [{:line "Load mySpecialTexture\\blah.b3d" :linenum 1
        :error (:texture-in-not-in-builder errors)}
       {:line "SetTextureCoordinates,1,1,0" :linenum 4
        :error "Vertex #1 not found"}
       {:line "SetTextureCoordinates,1a,1,0" :linenum 5
        :error (:number errors)}
       {:line "Coordinates 1,0,1b" :linenum 6
        :error (:number errors)}]))))

(defn f [v] (map float v))

(deftest complete-mesh
  (testing "the whole darn thing works"
    (is
     (=
      (:meshes
       (parse-string
        (u/text
         "[MeshBuilder]"
         "Vertex  -2.7, 1.1, 0"
         "Vertex  -2.7, 1.1, 25.3"
         "Vertex  -1.5, 1.1, 25.3"
         "Vertex  -1.5, 1.1, 0"
         ""
         "Face 3, 1, 2, 0"
         ""
         "[Texture]"
         "Load StaForm.bmp"
         "Coordinates 0, 0, 1"
         "Coordinates 1, 0, 0"
         "Coordinates 2, 1, 0"
         "Coordinates 3, 1, 1"
         ""
         "[MeshBuilder]"
         "Vertex -1.5, -0.3, 0"
         "Vertex -1.5, -0.3, 25.3"
         "Vertex -1.5, 1.1, 25.3"
         "Vertex -1.5, 1.1, 0"
         "Face2 3, 2, 1, 0"
         ""
         "Color 217,113,1   ; Orange"
         "")))
      (list
       (m/create-mesh
        [(m/create-face
          [(m/create-vertex (f [-1.5  1.1 0.0])  nil)
           (m/create-vertex (f [-1.5  1.1 25.3]) nil)
           (m/create-vertex (f [-1.5 -0.3 25.3]) nil)
           (m/create-vertex (f [-1.5 -0.3 0.0])  nil)]
          (m/create-material
           (m/create-color-set [217 113 1])
           (m/create-texture-set nil nil 0.0)
           nil)
          true)])
       (m/create-mesh
        [(m/create-face
          [(m/create-vertex (f [-1.5 1.1 0.0])  [1.0 1.0])
           (m/create-vertex (f [-2.7 1.1 25.3]) [0.0 0.0])
           (m/create-vertex (f [-1.5 1.1 25.3]) [1.0 0.0])
           (m/create-vertex (f [-2.7 1.1 0.0])  [0.0 1.0])]
          (m/create-material
           (m/create-color-set nil nil nil)
           (m/create-texture-set "/StaForm.bmp" nil 0.0)
           nil)
          false)]))))))
