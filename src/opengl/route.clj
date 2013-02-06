(ns opengl.route
  [:require [opengl.core :as core]]
  (:gen-class))

(defn- append-file-parse-error [context str]
  (assoc context :errors (conj (context :errors) str)))

(defn- resolve-symbol-table [context]
  (let [symbol-table (:symbol-table context)]
    (reduce (fn [context [k v]]
              (try
                (let [b3d (core/b3d-parse-file (java.io.File. v))]
                  (if (:error b3d)
                    (append-file-parse-error context (:error b3d))
                    (assoc context :symbol-table
                           (assoc (:symbol-table context)
                             k b3d))))
                (catch Exception e (append-file-parse-error context (format "Could not load %s" v)))))
            context
            (:symbol-table context))))

(defn- insert-into-symbol-table [context command index file]
  (let [symbol-table (:symbol-table context)]
    (assoc context :symbol-table
           (assoc symbol-table index file))))

(defn- trim-trailing-comma [s]
  (if (= \, (last s))
    (.substring s 0 (- (count s) 1))
    s))

(defn- load-structure [context]
  (reduce
   (fn [context node]
     (insert-into-symbol-table context
                               (:type node)
                               (:arg node)
                               (trim-trailing-comma (.trim (.replaceAll (:body node) "\\\\" "/")))))
   context
   (filter
    #(.equalsIgnoreCase (:prefix %) "structure")
    (:nodes context))))

(defn- convert-node-to-error [node descr]
  (assoc node :type :parse-error :description descr))

(defn- create-node [type line idx file stuff]
  (merge
   {:type type
    :fileinfo { :line line :file file :line-num (+ 1 idx) }}
   stuff))

(defn- create-error [line idx file descr]
  (create-node :parse-error line idx file { :description descr}))

(defn is-type [node type]
  (= (:type node) type))

(defn create-track-ref-node [num line idx file]
  (create-node :track-ref line idx file { :ref num }))

(defn- print-node [node]
  (if (= :parse-error (:type node))
    (printf "Parse Error:%s:%s: %s\n  %s\n"
            (:file (:fileinfo node))
            (:line-num (:fileinfo node))
            (:description node)
            (:line (:fileinfo node)))
    (println "AST(" (:type node) (assoc node :fileinfo nil) ")")))

(defn- substr [str e]
  (.trim (.substring str e)))

(defn- parse-simple-prefix [line]
  (second (re-matches #".+?\s(.*)$" line)))

(def prefix-re #"([a-zA-Z]+)?(\.([a-zA-Z]+)(\(([0-9]+)\))?)(\.[a-zA-Z.]+)?(.*)$")

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

(defn- create-node-from-prefix [line line-num file]
  (let [[_ pfx _ cmd _ arg sfx rest] (prefixed-line line)
        cmd (.trim (.toLowerCase cmd))]
    (if (some #{cmd} allowed-commands)
     (create-node cmd line line-num file { :prefix pfx :body rest :arg arg :suffix sfx})
     (create-error line line-num file (str "Unknown Command: " cmd)))
    ;
    ))

(defn- strip-comment [line]
  (let [idx (.indexOf line ";")]
    (if (> 0 idx)
      line
      (.substring line 0 idx))))

(defn- parse-route-line [idx line file]
  (cond
   (= (count (.trim line)) 0)
   nil

   (= \; (first line))
   nil

   (core/starts-with line "with")
   [(create-node :with line idx file { :scope (substr line 4)})]

   (prefixed-line line)
   [(create-node-from-prefix line idx file)]

   (num-prefixed-line line)
   (let [[_ num subcommands] (num-prefixed-line line)
         subcommands (.split subcommands ",")]
     (into [(create-track-ref-node num line idx file)]
           (map #(parse-route-line idx % file) subcommands)))

   :else
   (create-error line idx file "Could not parse line")))

(defn- update-prefix-if-needed [token last-with]
  (if (and last-with (not (:prefix token)))
    (assoc token :prefix last-with)
    token))

(defn- update-track-ref-if-needed [token track-ref]
  (if track-ref
    (assoc token :track-ref track-ref)
    token))


(defn- validate-commands-have-prefixes [context]
  (assoc context :nodes
         (map (fn [node]
                (if (:prefix node)
                  node
                  (convert-node-to-error node (str "Command must have prefix (given " (:type node) ")"))))
              (:nodes context))))

(def track-nodes-not-required-to-have-refs ["height"])

(defn- validate-track-commands-have-refs [context]
  (assoc context :nodes
         (let [nodes (:nodes context)]
           (map (fn [node]
                  (if (and (.equalsIgnoreCase (:prefix node) "track")
                           (nil? (:track-ref node))
                           (not (some #{(:type node)} track-nodes-not-required-to-have-refs)))
                    (convert-node-to-error node (str "Track command must have a reference"))
                    node))
                nodes))))

(defn- update-prefixes [context]
  (let [nodes (:nodes context)]
    (assoc context :nodes
           (last
            (reduce (fn [[last-with nodes] node]
                      (if (is-type node :with)
                        [(:scope node) nodes]
                        [last-with (conj nodes (update-prefix-if-needed node last-with))]))
                    [nil []]
                    nodes)))))

(defn- update-track-refs [context]
  (let [nodes (:nodes context)]
    (assoc context :nodes
           (last
            (reduce (fn [[last-ref nodes] node]
                      (if (is-type node :track-ref)
                        [(:ref node) nodes]
                        [last-ref (conj nodes (update-track-ref-if-needed node last-ref))]))
                    [nil []]
                    nodes)))))


(defn parse-route-file [^String file-path]
  (let [nodes (filter
               (comp not nil?)
               (flatten
                (map-indexed (fn [idx itm]
                               (parse-route-line idx (strip-comment itm) file-path))
                             (.split (slurp file-path) "\n"))))
        context {:nodes nodes :symbol-table {} :errors []}]
    (-> context
        update-prefixes
        update-track-refs
        validate-commands-have-prefixes
        validate-track-commands-have-refs
        load-structure
        )))

                                        ;(def p (parse-route-file "Flushing/test.csv"))
;; Phase 1:
;; Apply Track Refs and With Statements
;; Validation requires that no command be missing a prefix

(defn print-errors [tokens]
  (doseq [t tokens]
    (if (= :parse-error (:type t)) (print-node t))))

(defn print-all [tokens]
  (doseq [t tokens]
    (print-node t)))
