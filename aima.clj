;;; -*- Mode: Lisp; Syntax: Clojure; -*- File: aima.clj

(ns aima
  "Provides a simple build and load facility with the functions:
  aima-load, aima-load-if-unloaded, and aima-compile, and 
  which each have an optional argument of a system name (default all).
  Files can be compiled with compile-load and Clojure compile,
  which take namespace arguments rather than filenames.
  Filenames for namespaces with dashes (-) replace them with underscores (_).
  Because Clojure loads the newer of source and binary,
  other Common Lisp operations are not available or changed.

  In general, Common Lisp names and algorithms have been preserved except for
  Clojure name conflicts and the optimization of Clojure for vectors over seqs,
  which are the Clojure generalization of the Clojure list type.
  Common Lisp earmuffs (*) for global variables are removed from the name
  when the Clojure variable is not dynamic, which is true throughout this file.

  Except when an environmental variable AIMA_HOME is defined, uses
  the JVM \"user.dir\" startup current directory as the root of the source tree.
  All files must be loaded from within the source tree,
  whose root must be on the class path.
  If native file names are used, they must either be relative to the root
  or a name that starts with the root of the source tree in order to be loaded.
  In order to compile, the directory in *compile-path*,
  by default named \"classes\", must already exist on the JVM class path."

  {:author "Initially translated from Common Lisp AIMA code of Peter Norvig by
Robert L. Kirby http://www.linkedin.com/in/bobkirby"}
  (:use (clojure repl [stacktrace :rename {root-cause trace-root-cause}]))
  (:import (java.io File FileNotFoundException)))

(def ^:private aima-ns
  "This namespace for locking since routines can mutate the global environment,
  including the file system, only one thread should operate at a time."
  *ns*)

(def ^:private ^String FS
  (or (System/getProperty "file.separator") "/"))

(def aima-root
  "The root directory where the code is stored
  with all trailing file separators removed,
  which may be an empty string."
  (loop [^String root (or (System/getenv "AIMA_HOME")
                          (System/getProperty "user.dir")
                          FS)]
    (if (.endsWith root FS) (recur (.substring root 0 (dec (count root))))
        root)))

;;; No Clojure earmuffs for Common Lisp parameter *aima-binary-type*,
;;; which is not a dynamic global variable in Clojure.
(def aima-binary-type
  "Binary type suffix of one file that the Clojure compiler produces
  with the same capitalization as the namespace.
  Where the namespace has hyphens (-), the file name has underscores (_)."
  (str clojure.lang.RT/LOADER_SUFFIX ".class"))

(def aima-source-type
  "Type of Clojure source files."
  ".clj")
;;; No Clojure earmuffs for Common Lisp constant *aima-version*,
;;; which is not a dynamic global variable in Clojure.
(def aima-version "0.99 AIMA Code, Appomattox Version, 09-Apr-2002")

;;; No Clojure earmuffs for Common Lisp parameter *aima-system-names*,
;;; which is not a dynamic global variable in Clojure.
(defonce ^{:doc "A map from names to the systems that have been defined."}
  aima-system-names
  (atom {}))

;;;; Support Functions

;;; The Common Lisp DEFSTRUCT "aima-system" is not translated to Clojure defrecord,
;;; which would be used if inheritance were needed, but is just a hash-map instead
;;; with keyword access, including redundant keywords, some with null values,
;;; for debugging.

(defn add-aima-system [& pairs]
  (let [{:keys [name requires doc parts examples]} (apply hash-map pairs)]
    (swap! aima-system-names
           #(assoc % name
                   (hash-map :name name, :requires requires, :doc doc,
                             :parts parts, :examples examples, :loaded? false)))))

(defmacro update-aima-system
  "Update fields of aima-system with keywords and their values."
  [name k v & kvs]
  `(let [name# ~name]
     (swap! aima-system-names
          (fn [systems#]
            (assoc systems# name# (assoc (get systems# name#)
                                   ~k ~v ~@kvs))))))

(defn get-aima-system
  "Return the system with this name.  (If argument is a system, return it.)"
  [name]
  (cond	(symbol? name) (get @aima-system-names name),
        (contains? name :name) name))

(defmacro aima-system-requires
  "The requires field of aima-system"
  [aima-system]
  `(get ~aima-system :requires))

(defmacro aima-system-parts
  "The parts field of aima-system"
  [aima-system]
  `(get ~aima-system :parts))

(defmacro aima-system-loaded?
  "The loaded? field of aima-system"
  [aima-system]
  `(get ~aima-system :loaded?))

(defmacro aima-system-examples
  "The examples field of aima-system"
  [aima-system]
  `(get ~aima-system :examples))

(defn mklist [x]
  "If x is a list, return it; otherwise return a singleton list, (x)."
  (if (list? x) x (list x)))

(defn aima-file
  "Given a file name with an optional \".clj\" suffix and a relative path
  from the AIMA directory, return an unsuffixed, absolute resource name,
  which the Clojure load and compile functions use,
  rather than the right complete pathname of Common Lisp.
  Converts native file system names prefixed with the aima-root."
  [^String name & pairs]
  (let [{:keys [type path]} (apply hash-map pairs),
        name (if (.startsWith name aima-root)
               (.substring name (count aima-root))
               name),
        name (if (neg? (.indexOf name FS)) name
                 (.replaceAll name FS "/")),
        name (if-not (.endsWith name aima-source-type) name
                     (.substring name 0
                                 (- (count name) (count aima-source-type))))]
    (apply str (mapcat #(if (.startsWith ^String % "/") [%] [\/ %])
                       (concat path [name])))))

(defn- load-something
  "Try to load a file from its resource name operation for operate-on-aima-system.
  Complain if we can't find anything and return non-nil."
  [file]
  (try (load file)
       (catch FileNotFoundException fnfe
         (println (or (.getMessage fnfe) fnfe) "Did not load" file)
         file)
       (catch NullPointerException npe
         (println (or (.getMessage npe) npe) "Did not"
                  (if *compile-files* "compile" "load") file)
         file)))

(defn operate-on-aima-system
  "Perform the operation on the part (or system) and its subparts (if any).
  Reasonable operations are load-something, compile-load, and echo.
  If REQUIRED is true, then load any required systems that are unloaded.
  Returns non-nil if anything was not completely loaded, unlike with Common Lisp."
  [part operation & pairs]
  (let [{:keys [path required directory-operation],
         :or {required true, directory-operation identity}}
        (apply hash-map pairs)]
;;;    (apply prn 'operate-on-aima-system part operation pairs)
    (cond
     (string? part) (operation (aima-file part :path path)),

     (and (seq? part) (= (second part) '/))
     (let [subdirectory (mklist (first part)),
           new-path (concat path subdirectory)]
       (directory-operation new-path)
       (-> (for [subpart (nnext part),
               :when (operate-on-aima-system
                      subpart operation,
                      :directory-operation directory-operation,
                      :required required, :path new-path)] subpart)
           doall not-empty)),

     (seq? part)
     (-> (for [subpart part,
                             :when (operate-on-aima-system
                                    subpart operation,
                                    :directory-operation directory-operation,
                                    :required required, :path path)] subpart)
         doall not-empty),

     :else
     (if-let [system (get-aima-system part)]
       ;; Load the required systems, then operate on the parts
       (let [requires (aima-system-requires system),
             unloaded (when required
                        ;; Does not use aima-load-if-unloaded so that
                        ;; echo will not cause loading.
                        (-> (for [requirement requires,
                                  :let [subsystem (get-aima-system requirement)],
                                  :when (not (aima-system-loaded? subsystem))]
                              (operate-on-aima-system
                               subsystem operation,
                               :directory-operation directory-operation,
                               :required required, :path nil))
                            doall not-empty)),
             u (operate-on-aima-system (aima-system-parts system) operation,
                                       :directory-operation directory-operation,
                                       :required required, :path path),
             unloaded (cond (and u unloaded) (conj u unloaded),
                            unloaded unloaded,
                            u u)]
         (if (and (not unloaded) (or (empty? requires) required)) 
           (do (update-aima-system part :loaded? true) nil)
           part))
       ;; Common Lisp WARN becomes a print since Clojure has no RESTART.
       (do (println "Unrecognized part:" part "in path" path) part)))))

(defn- echo
  "Print file name if not nil operation for operate-on-aima-system."
  [file]
  (when file (prn 'echo file))
  file)

(defn aima-echo
  "Print file names."
  ([] (aima-echo 'all :required false,
                 :directory-operation #(prn :directory-operation %)))
  ([name & pairs]
     (locking aima-ns (apply operate-on-aima-system name echo pairs))))

(defn aima-load
  "Load file(s), trying the system-dependent method first."
  ([] (aima-load 'all))
  ([name & pairs]
     (locking aima-ns (apply operate-on-aima-system name load-something pairs))))

(defn aima-load-if-unloaded [name & pairs]
  (when-let [system (get-aima-system name)]
    (if-not (aima-system-loaded? system)
      (apply aima-load system pairs))
    system))

(defn- compile-load
  "Compile file and then load it operation for operate-on-aima-system."
  [file]
  ;; This could be made more sophisticated, to compile only when out of date.
  ;; Clojure always loads files as they are compiled.
  ;; Binding *compile-files* eliminates the need to decode resource path
  ;; but depends on an undocumented feature.
  (binding [*compile-files* true]
    (load-something file)))

(defn aima-compile
  "Compile (and load) the file or files that make up an AIMA system."
  ([] (aima-compile 'all))
  ([name & pairs]
     (locking aima-ns (apply operate-on-aima-system name compile-load pairs))))

;;;; The Top-Level Functions:

(defmacro def-aima-system
  "Define a system as a list of parts.  A part can be a string, which denotes
  a file name; or a symbol, which denotes a (sub)system name; or a list of the
  form (subdirectory / part...), which means the parts are in a subdirectory.
  The REQUIRES argument is a list of systems that must be loaded before this 
  one.  Note that a documentation string is mandatory."
  [name requires doc & parts]
  `(add-aima-system :name '~name
		    :requires '~requires :doc '~doc :parts '~parts))

;;; ----------------------------------------------------------------------
;;;; Definitions of Systems
;;; ----------------------------------------------------------------------

(def-aima-system utilities ()
  "Basic functions that are loaded every time, and used by many other systems."
  ("utilities" / "utilities" "binary-tree" "queue"
   ;;; Common Lisp the Language 2 compatibility is not translated to Clojure.
   ;; "cltl2"
   "test-utilities"))

(def-aima-system agents (utilities)
  "Code from Part I: Agents and Environments"
  ("agents" / "test-agents"
   ("environments" / "basic-env" "grid-env" "vacuum" "wumpus")
   ("agents" / "agent" "vacuum" "wumpus")
   ("algorithms" / "grid")))

(def-aima-system search (agents)
  "Code from Part II: Problem Solving and Search"
  ("search" / "test-search" 
   ("algorithms" / "problems" "simple" "repeated" 
    "csp" "ida" "iterative" "sma" "minimax")
   ("environments" / "games" "prob-solve")
   ("domains" / "cannibals" "ttt" "cognac" "nqueens" "path-planning" 
    "puzzle8" "route-finding" "tsp" "vacuum")
   ("agents" / "ps-agents" "ttt-agent")))

(def-aima-system logic (agents)
  "Code from Part III: Logic, Inference, and Knowledge Representation"
   ("logic" / "test-logic"
    ("algorithms" / "tell-ask" "unify" "normal" "prop" "horn" "fol" "infix")
    ("environments" / "shopping")))

(def-aima-system planning ()
  "Code from Part IV: Planning and Acting"
   ("planning" / ))

(def-aima-system uncertainty (agents)
  "Code from Part V: Uncertain Knowledge and Reasoning"
  ("uncertainty" / "test-uncertainty"
   ("agents" / "mdp-agent")
   ("domains" / "mdp" "4x3-mdp")
   ("environments" / "mdp")
   ("algorithms" / "dp" "stats")))

(def-aima-system learning (uncertainty)
  "Code from Part VI: Learning"
   ("learning" / "test-learning"
    ("algorithms" / "inductive-learning" "learning-curves" "dtl" "dll"
     "nn" "perceptron" "multilayer" "q-iteration")
    ("domains" / "restaurant-multivalued" "restaurant-real"
     "restaurant-boolean" "majority-boolean" "ex-19-4-boolean"
     "and-boolean" "xor-boolean" "4x3-passive-mdp")
    ("agents" / "passive-lms-learner" "passive-adp-learner"
     "passive-td-learner" "active-adp-learner" "active-qi-learner"
     "exploring-adp-learner" "exploring-tdq-learner")))

(def-aima-system language (logic)
  "Code from Part VII, Chapters 22-23: Natural Language and Communication"
   ("language" / "test-language"
    ("algorithms" / "chart-parse")
    ("domains" / "grammars" )))

(def-aima-system all ()
  "All systems except the utilities system, which is always already loaded"
  agents search logic planning uncertainty learning language)

(def-aima-system everything ()
  "All the code, including the utilities"
  utilities all)

;;;; Always load the utilities

'(aima-load 'utilities)
