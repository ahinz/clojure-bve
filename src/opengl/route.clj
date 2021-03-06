(ns opengl.route
  [:require
   [opengl.core :as core]
   [opengl.util :as util]
   [opengl.b3d :as b3d]]
  (:gen-class))

(set! *warn-on-reflection* true)

(def errors
  {:unsupported-limit-construct "Posts on limits are not supported (yet)"
   :unknown-command "Could not parse command %s"
   :bad-line "Could not parse line"
   :bad-structure-command "Invalid command in structure"
   :option-not-yet-supported "This option is not yet supported"
   :route-not-yet-supported "This route option is not yet supported"
   :track-ref-req "Track references are required in the track section"
   :command-not-found "Invalid command"
   :symbol-not-found "Could not find index %s in %s"
   :rail-already-started "Rail %d already exists"
   :rail-not-found "Rail %s does not exist"
   :bad-track-command "Unknown track command"})

(defn- add-error [context error-key & rest]
  (assoc
      context :errors
      (conj (or (:errors context) [])
            {:linenum (:linenum context)
             :line (:line context)
             :error (apply format (concat [(get errors error-key)] rest))})))

(defn- add-node-error [context node error-key & rest]
  (assoc
      context :errors
      (conj (or (:errors context) [])
            {:linenum (:linenum node)
             :line (:line node)
             :file (:path context)
             :type (:type node)
             :prefix (:prefix node)
             :error (apply format (concat [(get errors error-key)] rest))})))

(defn- add-node-warning [context node error-key]
  (assoc
      context :errors
      (conj (or (:errors context) [])
            {:linenum (:linenum node)
             :line (:line node)
             :file (:path context)
             :error (get errors error-key)})))

(defn- strip-comment [^String line]
  (let [idx (.indexOf line ";")]
    (if (> 0 idx)
      line
      (.substring line 0 idx))))

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
      (add-error context :unknown-command cmd))))

(defn symbol-from-context [context type idx]
  (get (:symbol-table context) (str type idx)))

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
  ([s] (parse-string s {:block-size 25.0}))
  ([s context]
     (reduce (fn [context line]
               (parse-route-line
                (assoc context
                  :line line
                  :linenum (+ 1 (or (:linenum context) 0)))
                line))
             (assoc context :symbol-table {})
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

(def supported-structures
  ["ground" "rail" "walll" "wallr" "dikel" "diker"
   "forml" "formr" "formcl" "formcr"
   "roofl" "roofr" "roofcl" "roofcr"
   "crackl" "crackr" "freeobj" "beacon"])

(defn- read-string-if-not-empty [^String s]
  (if (= (count (.trim s)) 0) nil
      (read-string s)))

(defn- split-body [node]
  (if (nil? node)
    []
    (map read-string-if-not-empty
         (.split
          (.trim ^String (trim-trailing-comma (:body node))) ";"))))

(defn- strip-prefix-char [^String s]
  (if (= (first s) \/) (.substring s 1) s))

(defn- parse-ground-node [context node]
  (let [symbol-table (:symbol-table context)
        indexes (split-body node)
        prototypes (map #(get symbol-table (str "ground" %)) indexes)
        gidx (:arg node)]
    (assoc-in context [:grounds gidx] prototypes)))

(defn parse-structure-node [context node]
  (if (some #{(:type node)} supported-structures)
    (let [^String
          filepath (strip-prefix-char
                    (strip-comment
                     (util/replace-windows-path-chars
                      (:body node))))
          resolved (if (:debug-symbols context)
                     {:meshes filepath :errors nil}
                     (b3d/parse-file-from-string filepath))
          symbol-name (str (:type node) (:arg node))]
      (println "Resolving" symbol-name "with file" filepath
               (count (:errors resolved)))
      (if (> (count (:errors resolved)) 0)
        (do
          (println (:errors resolved))
          (assoc context
            :errors (concat (:errors context)
                           (:errors resolved))))
        (assoc context :symbol-table
               (assoc (:symbol-table context) symbol-name
                      (:meshes resolved)))))

    (add-error context :bad-structure-command)))

(defn- parse-route-node [context node]
  (cond
   (is-type node "comment")
   (assoc-in context [:route :comment] (:body node))
   (is-type node "timetable")
   (assoc-in context [:route :timetable] (:body node))
   (is-type node "developerid")
   (assoc-in context [:route :developerid] (:body node))

   :else
   (add-node-warning context node :route-not-yet-supported)))

(defn- update-block [context block f]
  (assoc context :blocks (nth (:blocks context))))

(defn- parse-track-node [context node]
  (let [track-ref (or (:track-ref node) 0.0)
        block (int (/ track-ref (:block-size context)))]
    (update-in context [:blocks block :nodes]
               (fn [nodes]
                 (conj (or nodes []) node)))))

(defn- parse-node [context node]
  (cond
   (= (:prefix node) "options")
   (add-node-warning context node :option-not-yet-supported)

   (= (:prefix node) "route")
   (parse-route-node context node)

   (= (:prefix node) "structure")
   (parse-structure-node context node)

   (= (:prefix node) "track")
   (parse-track-node context node)

   (= (:prefix node) "cycle")
   (parse-ground-node context node)

   :else
   (add-node-error context node :command-not-found)))

(defn- parse-nodes-in-context [context]
  (reduce parse-node context (:nodes context)))

(defn begin-repeat [context node block railidx structidx dir key sym]
  (let [left-texture (symbol-from-context context (str key "l") structidx)
        right-texture (symbol-from-context context (str key "r") structidx)]
    (if-let [rail (get (:rails block) railidx)]
      (cond
       (and (or (= dir 0) (= dir -1)) (nil? left-texture))
       (add-node-error block node :symbol-not-found structidx (str key "l"))

       (and (or (= dir 0) (= dir  1)) (nil? right-texture))
       (add-node-error block node :symbol-not-found structidx (str key "r"))

       (= dir 1)
       (let [rail (assoc rail :walls {:right right-texture})
             rails (assoc (:rails block) railidx rail)]
         (assoc block :rails rails))

       (= dir -1)
       (let [rail (assoc rail :walls {:left left-texture})
             rails (assoc (:rails block) railidx rail)]
         (assoc block :rails rails))

       :else
       (assoc-in block [:rails railidx sym]
                 {:left left-texture
                  :right right-texture}))
      (add-node-error block node :rail-not-found railidx))))

(defn- copy-previous-data [old-block new-block]
  (assoc
      new-block
    :speed (or (:speed old-block) 0)
    :grounds (:grounds old-block)
    :rails
    (if (:rails old-block)
      (reduce (fn [rails [railidx rail]]
                (if (nil? (:end rail))
                  (println "nil end on rail" railidx
                           (dissoc rail :prototype :walls :freeobjs)))
                (if (:rail-ends rail)
                  rails
                  (assoc rails
                    railidx
                    (assoc rail
                      :freeobjs []
                      :start (:end rail)
                      :end (:end rail)))))
              {}
              (:rails old-block))
      {0 {:start [0.0 0.0] :end [0.0 0.0]}})))

(def signal-aspects
  {2  [[0 2] 0]
   -2 [[0 4] 1]
   3  [[0 2 4] 2]
   4  [[0 1 2 4] 3]
   -4 [[0 2 3 4] 4]
   5  [[0 1 2 3 4] 5]
   -5 [[0 2 3 4 5] 6]
   6  [[0 1 2 3 4 5] 7]})

(defn- create-section-for-signal [block node]
  (let [[aspect-id _ x y yaw pitch roll] (split-body node)
        x (or x 0.0)
        y (or y 0.0)
        yaw (* (or yaw 0.0) 0.0174532925199433)
        pitch (* (or pitch 0.0) 0.0174532925199433)
        roll (* (or roll 0.0) 0.0174532925199433)
        [aspects compat] (get signal-aspects aspect-id)]
    {:track-ref (:track-ref node)
     :position [x y (- (:track-ref node) (:start-ref block))]
     :yaw yaw
     :pitch pitch
     :roll roll
     :apsects aspects
     :compat compat
     :visible (not= x 0.0)}))

(defn- parse-nodes-in-block [context block prev-block]
  (reduce
   (fn [block node]
     (cond
      (is-type node "railstart")
      (let [[railidx x y railtyp] (split-body node)
            texture (symbol-from-context context "rail" railtyp)]
        (if texture
          (if (and (get (:rails block) railidx)
                   (not (:rail-ends (get (:rails block) railidx))))
            (add-node-error block node :rail-already-started railidx)
            (assoc-in block [:rails railidx]
                      {:start [(float x) (float y)]
                       :end   [(float x) (float y)]
                       :prototype texture
                       :rail-ends false}))
          (add-node-error block node :symbol-not-found railidx type)))

      (is-type node "rail")
      (let [[railidx x y railtyp] (split-body node)
            texture (symbol-from-context context "rail" railtyp)]
        (if texture
          (update-in
           block [:rails railidx]
           (fn [rail]
             (merge rail {:start [(float x) (float y)]
                          :end   [(float x) (float y)]
                          :prototype texture })))
          (add-node-error block node :symbol-not-found railtyp "rail")))

      (is-type node "railend")
      (let [[railidx x y] (split-body node)]
        (if-let [rail (get (:rails block) railidx)]
          (update-in
           block [:rails railidx]
           (fn [rail]
             (merge rail
                    {:end (if (and x y) [(float x) (float y)]
                              (or (:start rail) [0.0 0.0]))
                     :start (if (and x y) [(float x) (float y)]
                              (or (:start rail) [0.0 0.0]))
                     :rail-ends true})))
          (add-node-error block node :rail-not-found railidx)))

      (is-type node "railtype")
      (let [[railidx railtype] (split-body node)]
        (if-let [rail (get (:rails block) railidx)]
          (if-let [texture (symbol-from-context context "rail" railtype)]
            (assoc-in block [:rails railidx :prototype] texture)
            (add-node-error block node :symbol-not-found railtype "rail"))
          (add-node-error block node :rail-not-found railidx)))

      (is-type node "accuracy")
      (assoc block :accuracy (read-string (:body node)))

      (is-type node "adhesion")
      (assoc block :accuracy (read-string (:body node)))

      ;; Geometry
      (is-type node "pitch")
      (assoc block :pitch (read-string (:body node)))

      (is-type node "curve")
      (let [[radius cant] (split-body node)
            cant (or cant 0.0)]
        (assoc block :curve {:radius radius :cant cant}))

      (is-type node "turn")
      (assoc block :turn (read-string (:body node)))

      (is-type node "height")
      (assoc block :height (read-string (:body node)))

      (is-type node "sta")
      block ;; Ignored

      (is-type node "poleend")
      block ;; Ignored

      (is-type node "back")
      block ;; Ignored

      (is-type node "stop")
      block ;; Ignored

      (is-type node "signal")
      (update-in block [:sections]
                 (fn [sections]
                   (conj (or sections [])
                         (create-section-for-signal block node))))

      ;; TODO: Sound Support
      (is-type node "announce")
      block ;; Ignored

      (is-type node "marker")
      block ;; Ignored

      (is-type node "crack")
      block ;; Ignored

      (is-type node "limit")
      (let [[speed a b] (split-body node)]
        (if (and (nil? a) (nil? b))
          (assoc block :speed speed)
          (add-node-error block node :unsupported-limit-construct)))

      (is-type node "form")
      (let [[r1 r2 roof-idx form-idx] (split-body node)]
        (if (or (= r1 'l) (= r1 'r) (= r1 0) (get (:rails block) r1))
          (if (or (= r2 'l) (= r2 'r) (= r2 0) (get (:rails block) r2))
            (assoc block :forms
                   (conj (:forms block)
                         {:rail1 r1 :rail2 r2
                          :roof-idx roof-idx :form-idx form-idx}))
            (add-node-error block node :rail-not-found r2))
          (add-node-error block node :rail-not-found r1)))

      (is-type node "freeobj")
      (let [[railidx freeobj x y yaw pitch roll] (split-body node)
            x (or x 0.0)
            y (or y 0.0)
            yaw (or yaw 0.0)
            pitch (or pitch 0.0)
            roll (or roll 0.0)]
        (if-let [rail (get (:rails block) railidx)]
          (if-let [texture (symbol-from-context context "freeobj" freeobj)]
            (update-in block [:rails railidx :freeobjs]
                       (fn [freeobjs]
                         (conj (or freeobjs [])
                               {:x x :y y :z (- (:track-ref node) (:start-ref block))
                                :yaw yaw :pitch pitch :roll roll
                                :prototype texture})))
            (add-node-error block node :symbol-not-found freeobj "freeobj"))
          (add-node-error block node :rail-not-found railidx)))

      (is-type node "wall")
      (let [[railidx dir wallidx] (split-body node)]
        (begin-repeat context node block railidx wallidx dir "wall" :walls))

      (is-type node "wallend")
      (let [railidx (read-string (:body node))]
        (if (get (:rails block) railidx)
          (assoc-in block [:rails railidx :walls] {})
          (add-node-error block node :rail-not-found railidx)))

      (is-type node "dike")
      (let [[railidx dir wallidx] (split-body node)]
        (begin-repeat context node block railidx wallidx dir "dike" :dikes))

      (is-type node "dikeend")
      (let [railidx (read-string (:body node))]
        (if (get (:rails block) railidx)
          (assoc-in block [:rails railidx :dike-end] true)
          (add-node-error block node :rail-not-found railidx)))

      (is-type node "ground")
      (assoc block :grounds (get (:grounds context)
                                 (:body node)))

      :else
      (add-node-error block node :bad-track-command)))
   (copy-previous-data prev-block (dissoc block :nodes))
   (:nodes block)))

(defn- parse-nodes-in-blocks-in-context [context]
  (reduce (fn [context block]
            (let [block (parse-nodes-in-block
                         context block (last (:blocks context)))
                  context (assoc context :errors
                                 (concat (or (:errors context) []) (:errors block)))
                  block (dissoc block :errors)]
              (assoc context :blocks
                     (conj (or (:blocks context) []) block))))
          (assoc context :blocks [])
          (:blocks context)))

(defn- create-base-block [starting-position block-size]
  {:start-ref starting-position
   :end-ref (+ starting-position block-size)
   :nodes []
   :rails {0 {}}})

(defn- create-blocks [context]
  (let [refs (filter identity (map #(:track-ref %) (:nodes context)))
        max-ref (apply max refs)
        block-size 25]
    (map #(create-base-block % block-size)
         (range 0 (+ block-size max-ref) block-size))))

(defn- create-blocks-in-context [context]
  (assoc context :blocks (into [] (create-blocks context))))

(defn next-block [context block]
  (nth (:blocks context)
       (int (/ (:end-ref block) (:block-size context)))
       nil))

(defn- filter-end-rails [block]
  (assoc block :rails
         (reduce (fn [rails [railidx rail]]
                   (if (:rail-ends rail)
                     (do (println "Ending rail" (:rail-ends rail)) rails)
                     (assoc rails railidx rail)))
                 {} (:rails block))))

(defn- pull-rail-ends [block next-block]
  (let [next-rails (:rails next-block)
        cur-rails (:rails block)]
    (assoc
        block :rails
        (into
         {}
         (map
          (fn [[railidx rail]]
            [railidx
             (if-let [next-rail (get next-rails railidx)]
               (assoc rail :end (:start next-rail))
               rail)])
          cur-rails)))))

(defn- pull-rail-ends-in-context [context]
  (assoc
      context :blocks
      (reduce (fn [blocks block]
                (conj blocks
                      (pull-rail-ends
                       block
                       (next-block context block))))
              []
              (:blocks context))))

(defn- split [^String s ^String p] (.split s p))

(defn parse-route-string
  ([str file] (parse-route-string str file {}))
  ([str file rest]
     (->> (merge {:block-size 25.0 :file file} rest)
          (parse-string str)
          associate-with-blocks
          associate-track-refs
          create-blocks-in-context
          parse-nodes-in-context
          parse-nodes-in-blocks-in-context
          pull-rail-ends-in-context)))

(defn parse-route-file [^String file-path]
  (parse-route-string (slurp file-path) file-path))
