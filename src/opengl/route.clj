(ns opengl.route
  [:require
   [opengl.core :as core]
   [opengl.util :as util]
   [opengl.b3d :as b3d]]
  (:gen-class))

(set! *warn-on-reflection* true)

(def errors
  {:bad-line "Could not parse line"})

(defn- add-error [context error-key]
  (assoc
      context :errors
      (conj (or (:errors context) [])
            {:linenum (:linenum context)
             :line (:line context)
             :error (get errors error-key)})))

(defn- strip-comment [^String line]
  (let [idx (.indexOf line ";")]
    (if (> 0 idx)
      line
      (.substring line 0 idx))))

(defn- append-file-parse-error [context str]
  (assoc context :errors (conj (context :errors) str)))

(defn resolve-symbol-table [context]
  (let [symbol-table (:symbol-table context)]
    (reduce (fn [context [k ^String v]]
              (try
                (let [b3d (b3d/parse-file (java.io.File. v))]
                  (if (:errors b3d)
                    (append-file-parse-error context (:errors b3d))
                    (assoc context :symbol-table
                           (assoc (:symbol-table context)
                             k (:meshes b3d)))))
                (catch Exception e (append-file-parse-error context (format "Could not load %s" v)))))
            context
            (:symbol-table context))))

(defn- insert-into-symbol-table [context command index file]
  (let [symbol-table (:symbol-table context)]
    (assoc context :symbol-table
           (assoc symbol-table (str command index) file))))

(defn- trim-trailing-comma [^String s]
  (if (= \, (last s))
    (.substring s 0 (- (count s) 1))
    s))

(defn- trim-forward-slash [^String s]
  (if (= (first s) \/)
    (.substring s 1)
    s))

(defn- replace-all [^String base ^String pattern ^String repl]
  (.replaceAll base pattern repl))
(defn- equals-ignore-case [^String a ^String b] (.equalsIgnoreCase a b))

(defn- load-structure [context]
  (reduce
   (fn [context node]
     (insert-into-symbol-table context
                               (:type node)
                               (:arg node)
                               (strip-comment
                                (trim-forward-slash
                                 (trim-trailing-comma
                                  (util/trim (replace-all (:body node) "\\\\" "/")))))))
   context
   (filter
    #(equals-ignore-case (:prefix %) "structure")
    (:nodes context))))

(defn- convert-node-to-error [node descr]
  (assoc node :type :parse-error :description descr))

(defn- create-node [context type body]
  (merge
   {:type type
    :line (:line context)
    :file (:file context)
    :linenum (:linenum context)}
   body))

(defn- create-node-in-context [context type body]
  (assoc context :nodes
         (conj (or (:nodes context) [])
               (create-node context type body))))

(defn is-type [node type]
  (= (:type node) type))

(defn- substr [^String str e]
  (.trim (.substring str e)))

(defn- parse-simple-prefix [line]
  (second (re-matches #".+?\s(.*)$" line)))

(def prefix-re #"([a-zA-Z]+)?(\.([a-zA-Z]+)(\(([0-9]+)\))?)(\.([a-zA-Z.]+))?(.*)$")

(defn- prefixed-line [line]
  (re-matches prefix-re line))

(defn- num-prefixed-line [line]
  (re-matches #"^([0-9]+)(.*)$" line))

(def allowed-commands
  ["comment" "timetable" "developerid" "folder" "runinterval" "gauge" "rail"
   "freeobj" "wallr" "walll" "stop" "form" "sta" "wall" "railtype" "railstart"
   "ground" "pitch" "back" "roofcl" "background" "height" "railend"
   "signal" "curve" "wallend" "announce" "marker" "crack" "limit" "poleend"
   "dikeend" "turn" "roofr" "roofl" "roofcr" "forml" "formr" "formcr" "formcl"
   "crackl" "crackr"])

(defn- create-node-from-prefix [context line]
  (let [[_ pfx _ ^String cmd _ arg _ sfx rest] (prefixed-line line)
        cmd (util/trim (.toLowerCase cmd))]
    (if (some #{cmd} allowed-commands)
      (create-node-in-context
       context cmd
       {:prefix pfx
        :body (trim-trailing-comma
               (util/trim rest))
        :arg arg
        :suffix sfx})
      (add-error context :bad-line))))

(defn- parse-route-line [context ^String line]
  (cond
   (= (count (util/trim line)) 0)
   context

   (= \; (first line))
   context

   (util/starts-with line "with")
   (create-node-in-context context :with {:body (util/lower-case (substr line 4))})

   (prefixed-line line)
   (create-node-from-prefix context line)

   (num-prefixed-line line)
   (let [[_ num ^String subcommands] (num-prefixed-line line)
         subcommands (.split subcommands ",")
         context (create-node-in-context context :track-ref {:body num})]
     (reduce parse-route-line context subcommands))

   :else
   (add-error context :bad-line)))

(defn- parse-string
  ([s] (parse-string s {}))
  ([s context]
     (reduce (fn [context line]
                         (parse-route-line
                          (assoc context
                            :line line
                            :linenum (+ 1 (or (:linenum context) 0)))
                          line))
                       context
                       (map #(util/trim %) (util/split s "\n")))))

(defn- reduce-nodes [context f]
  (let [nodes (:nodes context)
        context (assoc context :nodes [])]
    (reduce f context nodes)))

(defn- append-node-with-track-ref [context node track-ref]
  (assoc context
    :nodes
    (conj (or (:nodes context) [])
          (assoc node :track-ref track-ref))))

(defn- append-node-with-prefix [context node prefix]
  (let [prefix (or (:prefix node) prefix)
        node (assoc node :prefix prefix)
        nodes (or (:nodes context) [])]
    (assoc context :nodes (conj nodes node))))

(defn- associate-track-refs [context]
  (reduce-nodes
   context
   (fn [context node]
     (if (is-type node :track-ref)
       (assoc context :track-ref (read-string (:body node)))
       (append-node-with-track-ref context node (:track-ref context))))))

(defn- associate-with-blocks [context]
  (reduce-nodes
   context
   (fn [context node]
     (if (is-type node :with)
       (assoc context :with (:body node))
       (append-node-with-prefix context node (:with context))))))

(defn- split [^String s ^String p] (.split s p))

(defn parse-route-file [^String file-path]
  (let [nodes (filter
               (comp not nil?)
               (flatten
                (map-indexed (fn [idx itm]
                               (parse-route-line idx itm file-path))
                             (split (slurp file-path) "\n"))))
        context {:nodes nodes :symbol-table {} :errors []}]
    (-> context
        load-structure
        )))
