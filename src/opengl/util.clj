(ns opengl.util
  (:gen-class))

(defn strip-comment [^String str]
  (let [idx (.lastIndexOf str ";")]
    (.trim
     (if (> idx 0)
       (.substring str 0 idx)
       str))))

(defn strip-non-ascii [^String string]
  (.replaceAll string "[^\\x00-\\x7F]" ""))

(defn lower-case [^String l] (.toLowerCase l))
(defn str-starts-with [^String s1 ^String s2] (.startsWith s1 s2))

(defn starts-with [^String thing ^String prefix]
  (-> thing strip-non-ascii lower-case (str-starts-with (lower-case prefix))))

(defn join [lst c] (reduce #(str %1 %2 c) "" lst))
(defn text [& r] (join r "\r\n"))

(defn trim [^String s] (.trim s))
(defn replace-windows-path-chars [^String s]
  (.replaceAll s "\\\\" "/"))

(defn split [^String s ^String t] (.split s t))
(defn split-f [^String s] (map #(float (read-string %)) (.split s ",")))
(defn split-i [^String s] (map #(int (read-string %)) (.split s ",")))

(defn c-split [^String s] (map read-string (rest (.split s ","))))
(defn c-split-f [^String s] (map float (c-split s)))


(defmacro wrap-nil [f]
  `(try (let [r# ~f]
          (if (seq? r#) (doall r#) r#))
        (catch Exception e# nil)))
