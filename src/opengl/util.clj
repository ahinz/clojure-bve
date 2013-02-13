(ns opengl.util
  (:gen-class))

(defn strip-non-ascii [^String string]
  (.replaceAll string "[^\\x00-\\x7F]" ""))

(defn lower-case [^String l] (.toLowerCase l))
(defn str-starts-with [^String s1 ^String s2] (.startsWith s1 s2))

(defn starts-with [^String thing ^String prefix]
  (-> thing strip-non-ascii lower-case (str-starts-with (lower-case prefix))))
