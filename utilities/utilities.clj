;;; -*- Mode: Lisp; Syntax: Clojure; -*- File: utilities.clj

(ns utilities.utilities
  "Basic utility functions and macros, used throughout the AIMA Common Lisp code.
  Some, if not most, may not be needed with Clojure.

  The utilities are divided into control flow macros, list
  utilities, functions for 2-dimensional points, numeric utilities,
  some trivial functions, utilities for strings, symbols and
  printing, a debugging tool, and a testing tool.

  In general, Common Lisp names and algorithms have been preserved except for
  name conflicts and the optimization of Clojure for vectors over seqs,
  which are the Clojure generalization of the Clojure list type."
  {:author "Initially translated from Common Lisp AIMA code of Peter Norvig by
Robert L. Kirby http://www.linkedin.com/in/bobkirby"}
  (:use aima
        [clojure.pprint :only (pprint get-pretty-writer *print-pretty*)])
  (:import (java.io Writer) (java.util ArrayList Collection)))

;;; The AIMA Common Lisp macros "while", "for", "dotimes", and "deletef"
;;; are not translated.  "define-if-undefined" should not be used as is.
;;; The Clojure "@" read macro conflict is resolved with "make-xy".
;;; The AIMA function "sample-without-replacement" has a different algorithm
;;; that takes advantage of the Java runtime environment.

;;;; Control Flow Macros

;;; We define iteration macros to match the book's pseudo-code.
;;; This could all be done with LOOP, but some users don't have
;;; the LOOP from the 2nd edition of 'Common Lisp: the Language'.

;;; Clojure "while" is a macro with the same semantics as Common Lisp AIMA.

;;; Clojure "for" and "doseq" are macros that do the operations
;;; of the AIMA Common Lisp "for-each" macro but without the noise (in do) args.
;;; Since the core Clojure "for" and "doseq" macros have
;;; a standard, idomatic syntax, no Clojure "for-each" macro is defined.

;;; Clojure "dotimes" is a macro that does the operations of the AIMA Common Lisp
;;; "for" macro but without the noise (= to do) args.
;;; Since Clojure has a core "for" macro, no Clojure macro is defined.

;;; Clojure uses "filter", "filterv", "disj", or "dissoc" to produce a new
;;; immutable collection instead of destructively altering a sequence
;;; like AIMA Common Lisp "deletef".
;;; Any "deletef" usage should be checked for side effects.

(def sink-writer
  "Common Lisp BROADCAST-STREAM with no outputs equivalent"
  (proxy [Writer]
      []                                ; no super constructor arguments
    ;; override abstract methods with empty implementations
    (write [^chars cbuf, off, len])
    (close [])
    (flush [])))

;;; Clojure "declare" should replace "define-if-undefined" calls
;;; rather than using this macro call.
(defmacro define-if-undefined
  "Use this to conditionally define functions, variables, or macros that
  may or may not be pre-defined in this Lisp.  This can be used to provide
  CLtL2 compatibility for older Lisps."
  [& definitions]
  `(declare ~@definitions))

;;;; List Utilities
;;; Since Clojure is a lisp-1, single namespace Lisp unlike
;;; Common Lisp, which is a lisp-2 with several namespaces,
;;; instances of Clojure global function name "list" are shortened to
;;; a more idomatic "x" or "s" and potentially refer to
;;; Clojure seqs rather than the more specialized, rarer list.
;;; Similarly the Clojure special form name "fn" is shortened to "f".

(defn length>1
  "Is this a seq of 2 or more elements?"
  [x]
  (and (seq? x) (next x)))

(defn length=1
  "Is this a seq of exactly one element?"
  [x]
  (and (seq? x) (nil? (next x))))

(defn random-element
  "Return some element of the seq, chosen at random."
  [s]
  (rand-nth s))

(defn mappend
  "Lazily apply f to respective elements of coll(s), and append results."
  [f & colls]
  (mapcat #(map f %) colls))

(defn starts-with
  "Is this already a seq that starts with the given element?
Uses equality rather than identity comparison"
  [x element]
  (and (seq? x) (= (first x) element)))

(defmacro last1
  "Return the last element of a coll."
  [coll]
  `(last ~coll))

(defn left-rotate
  "Move the first element to the end of the seq, becoming a vector."
  [s]
  ;; Throwaway vectors, which are seqs too, are created faster than lists.
  (conj (subvec (vec s) 1) (first list)))

(defn right-rotate
  "Move the last element to the front of the seq, becoming a vector."
  [s]
  (apply conj [(last s)] (butlast s)))

(defn transpose
  "Transpose a matrix represented as a seq of seqs.
Creates vectors for the inner rows.
Example: (transpose '((a b c) (d e f))) => ([a d] [b e] [c f])."
  [seq-of-seqs]
  (apply #'map vector seq-of-seqs))

;; This is a Common Lisp optimization that may work poorly with Clojure.
(defn reuse-cons
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  [x y x-y]
  (if (and (= x (first x-y)) (= y (next x-y)))
      x-y
      (cons x y)))

;;; An expression is a seq consisting of a prefix operator followed by args,
;;; Or it can be a symbol, denoting an operator with no arguments.
;;; Expressions are used in Logic, and as actions for agents.

(defn make-exp [op & xs] (apply vector op xs))
(defn op "Operator of an expression" [exp] (if (seq? exp) (first exp) exp))
(defn args "Arguments of an expression" [exp] (if (seq? exp) (next exp) nil))
(defn arg1 "First argument" [exp] (if (seq? exp) (second exp)))
(defn arg2 "Second argument" [exp] (if (seq? exp) (first (nnext exp))))

;; Common Lisp "defsetf" of "args" modifies structure in a way
;; that is antithetical to Clojure philosophy.

;; Switched the order of "insert-between" and "prefix->infix"
;; to avoid a forward declaration.
(defmacro insert-between
  "Insert item between every element of seq."
  [item coll]
  `(interpose ~item ~coll))

(defn prefix->infix
  "Convert a fully parenthesized prefix expression into infix notation."
  [exp]
  (cond (nil? exp) nil,
        (not (seq? exp)) exp,
	(nil? (next exp)) exp,
	:else (insert-between (op exp) (map prefix->infix (args exp)))))

;;;; Numeric Utilities
;; These utilities must come first in order to avoid forward declarations.
;; Complex numbers are not handled unlike the Common Lisp version.

(def infinity
  "Most positive single float rather than infinity"
  Float/MAX_VALUE)
;; (def infinity Float/POSITIVE_INFINITY)
(def minus-infinity
  "Most negative single float rather than negative infinity"
  (- Float/MAX_VALUE))
;; (def minus-infinity Float/NEGATIVE_INFINITY)

(defn sum
  "Add up all the numbers; if function k is given, apply it to each number first."
  ([numbers]
     (apply + numbers))
  ([numbers k]
     (sum (map k numbers))))

(defn average
  "Numerical average (mean) of a list of numbers."
  [numbers]
  (/ (sum numbers) (count numbers)))

(defn running-average
  "Calculate new average given previous average over n data points"
  [avg new-val n] ;; Clojure uses "new" as a special form unlike Common Lisp.
  (/ (+ new-val (* avg n)) (inc n)))

(defn square [x] (* x x))

(defn between
  "Predicate; return t iff number x is between numbers y and z."
  [x y z]
  (or (<= y x z) (>= y x z)))

(defn ms-error
  "Compute mean square error between predicted seq and target seq"
  [predicted target]
  (/ (apply + (map #((square (- %1 %2)))
                   predicted target))
     (count predicted)))

(defn rms-error
  "Compute root mean square error between predicted list and target list"
  [predicted target]
  (Math/sqrt (double (ms-error predicted target))))

(defn boolean-error [predicted target]
  (if (== predicted target) 0 1))

(defn dot-product
  "dot product of two seqs"
  [s1 s2]
  (apply + (map #(* %1 %2)
                s1 s2)))

(defn iota
  "Return a list of n consecutive integers, by default starting at 0."
  ([n] (iota n 0))
  ([n start-at]
     (if (not (pos? n))
       nil (range start-at n 1))))

(defn random-integer
  "Return an integer chosen at random from the given interval."
  [from to]
  (+ from (rand-int (inc (- to from)))))

(defn normal [x mu sigma]
  (/ (Math/exp (double (/ (- (square (- x mu))) (* 2 (square sigma))))) 
     (* (Math/sqrt (* 2 Math/PI)) sigma)))

(defn sample-with-replacement [n population]
  (let [p (if (counted? population) population (vec population))]
    (repeatedly n (fn [] (random-element p)))))

(defn sample-without-replacement
  ([n population] (sample-without-replacement n population (count population)))
  ;; Assumes that m = (count population)
  ([n population m]
     (cond
      (<= n 0) nil,
      ;; Apparently, if the entire population is sampled,
      ;; randomness does not matter.
      (>= n m) population,
      ;; Common Lisp algorithm changed to use java.util.ArrayList
      ;; to have linear rather than quadratic behavior.
      :else (let [array-list (ArrayList. ^Collection (seq population))]
              (map #(.remove array-list (rand-int %)) (range m (- m n) -1))))))

(defn round-off
  "Round off the number to specified precision. E.g. (round-off 1.23 .1) = 1.2"
  [number precision]
  (* precision (Math/round (double (* number (/ precision))))))

(defn fuzz
  "Add and also subtract a random fuzz-factor to a quantity."
  ([quantity] (fuzz quantity 0.1 0.01))
  ([quantity proportion] (fuzz quantity proportion 0.01))
  ([quantity proportion rounding]
     (round-off (+ quantity
                   (* quantity (- (rand (double proportion))
                                  (rand (double proportion)))))
                rounding)))

;;;; Functions for manipulating 2-dimensional points 
;; Unlike expensive Common Lisp vectors, Clojure vectors are a preferred type.
;; Hence, a two-dimensional point will be a vector but not a defstruct,
;; which is an almost deprecated type, or a defrecord or deftype,
;; which have more infrastructure than is needed for a point.

(defn xy-p
  "Is the argument a 2-D point?"
  [arg]
  (and (vector? arg) (= (count arg) 2) (every? number? arg)))

;; Since "@" is deref in Clojure and no struct is defined, either
;; the longer "make-xy" is needed to create points or use a literal [x y].
(defmacro make-xy "Create a 2-D point" [x y] [x y])

(defn xy-equal [p q] (= p q))

(defn xy-add
  "Add two points, component-wise."
  [p q]
  ;; vectors are functions that return their value for a given index.
  [(+ (p 0) (q 0)) (+ (p 1) (q 1))])

(defn xy-distance
  "The distance between two points without ratio or BigNumber coordinates."
  [p q]
  (Math/sqrt (double (+ (square (- (p 0) (q 0)))
                        (square (- (p 1) (q 1)))))))

(defn x+y-distance
  "The 'city block distance' between two points
without ratio or BigNumber coordinates."
  [p q]
  (let [d0 (- (get p 0) (get q 0)), d1 (- (get p 1) (get q 1))]
    (+ (if (< (neg? d0)) (- d0) d0)
       (if (< (neg? d1)) (- d1) d1))))

(defn xy-between
  "Predicate; return t iff xy1 is between xy2 and xy3. Points are collinear."
  [xy1 xy2 xy3]
  (and (between (xy1 0) (xy2 0) (xy3 0))
       (between (xy1 1) (xy2 1) (xy3 1))))

(defn rotate [o a b c d]
  (let [x (o 0), y (o 1)]
    (make-xy (+ (* a x) (* b y)) (+ (* c x) (* d y)))))

(defn inside
  "Is the point l inside a rectangle from 0,0 to xmax,ymax?"
  [l xmax ymax]
  (let [x (l 0), y (l 1)]
    (and (>= x 0) (>= y 0) (< x xmax) (< y ymax))))

;;;; Trivial Functions

(defn nothing [& args]
  "Don't do anything with evaluated arguments except return nil."
  nil)

(defn declare-ignore
  "Ignore the arguments."
  [& _]
  ;; This is used to avoid compiler warnings in defmethod.
  ;; Some Common Lisp compilers warn "Variable unused" if it is bound by a method
  ;; but does not appear in the body.  However, if you put in a
  ;; (declare (ignore var)), then other compilers warn "var declared
  ;; ignored, but is actually used", on the grounds that it is implicitly
  ;; used to do method dispatch.  So its safest to use declare-ignore.
  ;; If you like, you can redefine declare-ignore to be a macro that
  ;; expands to either (declare (ignore args)), or to nothing, depending
  ;; on the implementation.
  nil)

;;; Use the Clojure forms (constantly true) and (constantly nil)
;;; in place of Common Lisp forms (true) and (false),
;;; which always return true (T in Common Lisp) and nil
;;; (which is the only false in Common Lisp) respectively.

(defn required
  "If this ever gets called, something that was required was not supplied.
Used as default value for Common Lisp &key args or defstruct slots.
Any msg and args should be accepted by Clojure format,
which may have different requirements than a Common Lisp error designator."
  ([] (required "A required argument is missing."))
  ([msg & args]
     (throw (RuntimeException. ^String (apply format msg args)))))

;;;; Utilities for strings and symbols and printing

(defn stringify
  "Coerce argument to a string."
  [exp]
  (if (nil? exp) "nil" (str exp)))

(defn concat-symbol [& args]
  "Concatenate the args into one string, and turn that into a symbol."
  (intern *ns* (apply str args)))

(defn print-repeated
  "Print the string n times."
  ([string n] ;; Avoids rebinding *out*
     (dotimes [_ n] (print string)))
  ([string n stream]
     (binding [*out* stream]
       (print-repeated string n))))

(defn print-centered
  "Print STRING centered in a field WIDTH wide."
  ([string width] ;; Avoids rebinding *out*
     (let [s (stringify string),
           blanks (- width (count s))]
       (print-repeated " " (quot blanks 2))
       (print s)
       (print-repeated " " (quot (inc blanks) 2))))
  ([string width stream]
     (binding [*out* stream]
       (print-centered string width))))

(defn print-dashes
  "Print a line of dashes WIDTH wide."
  ([width] ;; Avoids rebinding *out*
     (print-repeated "-" width))
  ([width stream]
     (binding [*out* stream]
       (print-dashes width)))
  ([width stream separate-line]
     (binding [*out* stream]
       (when separate-line (newline))
       (print-repeated "-" width)
       (when separate-line (newline)))))

(defn print-grid
  "Print the contents of a 2-D array, numbering the edges."
  [array {stream :stream, key :key, width :width,
          :or {stream *out*, key identity, width 3 }}]
  ;; Rebinds *out* as little as possible for idiomatic Clojure printing.
  (binding [*out* stream]
    ;; Unlike with Common Lisp AIMA code, the upper bound excludes the last value
    ;; into order to work better with Clojure "range".
    (let [max-x (count array),
          max-y (count (get array 0))]
      ;; Print the header
      (newline) (print-repeated " " width)
      (doseq [x (range 0 max-x 1)]
        (print "|") (print-dashes width))
      (print "|") (newline)
      ;; Print each row
      (doseq [y1 (range 1 (inc max-y) 1)]
        (let [y (- max-y y1)]
          (print-centered y width)
          ;; Print each location
          (doseq [x (range 0 max-x 1)]
            (print "|")
            ;; Use get for access since arrays may not always be functions.
            (print-centered (-> array (get x) (get y) key) width))
          (print "|") (newline)
          ;; Print a dashed line
          (print-repeated " " width)
          (doseq [x (range 0 max-x 1)]
            (print "|") (print-dashes width))))
      ;; Print the X-coordinates along the bottom
      (print-repeated " " width)
      (doseq [x (range 0 max-x 1)]
        (print " ") (print-centered x width))
      array)))

;;;; Assorted conversion utilities and predicates

(defn copy-array
  "Make a copy of an array.  (With Common Lisp)
Clojure immutable arrays do not need an explicit copy
as long as no identity checking is needed."
  [a]
  a)

;;; "copy-subarray" was a Common Lisp helper for "copy-array".

;;; "array->vector" created a displaced vector array in Common Lisp.
;;; Clojure needs some other approach so "array->vector" is not implemented.

(defn plot-alist
  "Writes alist with each pair on a separate line to output that file names.
Use Clojure conventions rather than superceding any existing output.
Clojure alist is a map instead of the Common Lisp list of consed pairs."
  [alist file]
  (with-open [stream (clojure.java.io/writer file)]
    (binding [*out* stream]
      (doseq [[x y] alist]
        (print x) (print " ") (println y))
      (flush))))

(defn copy-hash-table
  "Clojure copy of immutable hash table (any map) just returns the map
unless optional copy function k is supplied to be applied to each value."
  ([h] h)
  ([h k] (reduce-kv #(assoc %1 %2 (k %3)) (empty h) h)))

(defn hash-table->list
  "Return table, which is a map, which already is a seq, as is rather than
Common Lisp behavior that convert a hash table into a list of (key . val) pairs."
  [table]
  table)

(defn hprint
  "prints a hash table, which actually may be any map, line by line"
  ([h]
     (doseq [[x y] h]
       (newline) (print x) (print " ") (print y))
     (flush)
     h)
  ([h stream] (binding [*out* stream])))

(defmacro compose
  "Return a function h such that (h x) = (f (g x))."
  [f g]
  `(comp ~f ~g))

(defn the-biggest
  "Return first item from seq s whose numerical f value is greatest."
  [f s]
  (when-not (empty? s)
    (apply max-key f s)))

(defn the-biggest-random-tie
  "Return random item from seq s whose numerical f value is greatest."
  [f s]
  (when-not (empty? s)
    (rand-nth
     (with-local-vars [best-val (f (first s))]
       (reduce #(let [val (f %2)]
                  (cond (== val (var-get best-val)) (conj %1 %2),
                        (> val (var-get best-val)) (do (var-set best-val val)
                                                       [%2]),
                        :else %1))
               [(first s)] (next s))))))

(defn the-biggest-that
  "Return first item from seq s satisfying predicate p whose f value is biggest."
  [f p s]
  (the-biggest f (filter p s)))

(defn the-smallest
  "Return first item from seq s whose numerical f value is least."
  [f s]
  (when-not (empty? s)
    (apply min-key f s)))

(defn the-smallest-random-tie
  "Return random item from seq s whose numerical f value is least."
  [f s]
  (when-not (empty? s)
    (rand-nth
     (with-local-vars [best-val (f (first s))]
       (reduce #(let [val (f %2)]
                  (cond (== val (var-get best-val)) (conj %1 %2),
                        (< val (var-get best-val)) (do (var-set best-val val)
                                                       [%2]),
                        :else %1))
               (vector (first s))
               (next s))))))

(defn the-smallest-that
  "Return first item from seq s satisfying predicate p whose f value is least"
  [f p s]
  (the-smallest f (filter p s)))

;;;; Debugging tool

(defonce ^:dynamic *debugging* nil)

(defonce ^:dynamic *debug* *out*)

(defn dprint
  "Echo all the args when *debugging* is true.  Return the first one."
  [& args]
  (when *debugging*
    (binding [*out* *debug*]
      (newline) (apply prn args)))
  (first args))

;;;; Testing Tool: deftest and test
;;; The macro "deftest" and functions "add-test", "test", and "test-example"
;;; are implemented with name changes to avoid conflicts.
;;; However, testing in Clojure relies on the clojure.test library,
;;; which is a Clojure standard providing more functionality.

(defn add-aima-test
  "The functional interface for deftest: adds test examples to a system."
  [name examples]
  (if (get-aima-system name)
    (update-aima-system name :examples examples)
    (add-aima-system :name name :examples examples))
  name)

(defmacro def-aima-test
  "Define a set of test examples.  Each example is of the form (exp test)
  or (exp).  Evaluate exp and see if the result passes the test. Within the
  test, the result is bound to *.  The example ((f 2))) has no test to
  fail, so it always passes the test.  But ((+ 2 2) (= * 3)) has the test
  (= * 3), which fails because * will be bound to the result 4, so the test
  fails.  Call (TEST name) to count how many tests are failed within the
  named test.  NAME is the name of an aima-system."
  [name & examples]
  `(add-aima-test '~name '~examples))

(defn test-example
  "Does the EXP part of this example pass the TEST?"
  ([example] (test-example true))
  ([example print?]
     (if (string? example)
       (do (when (= print? true) (println ";;;" example))
           true)
       (let [exp (first example),
             test (cond (not (second example)) true,
                        (not (seq? (second example))) `(equal * ~(second example)),
                        :else (second example)),
             _ (when (= print? true) (println exp))
             * (eval exp)]
         (when (= print? true)
           (println *))
         (if-let [test-result (eval test)]
           test-result
           (case print?
             fail (println ";;; FAILURE on" (str exp \;)
                           "expected" (str test ", got:\n;;;") *),
             true (println ";;; FAILURE: expected" test),
             nil))))))

(defn test-aima
  "Run a test suite and sum the number of errors.  If all is well, this
  should return 0.  The second argument says what to print: nil for
  nothing, true for everything, or FAIL for just those examples that fail.
  If there are no test examples in the named system, put the system has
  other systems as parts, run the tests for all those and sum the result."
  ([] (test-aima 'all true))
  ([name] (test-aima name true))
  ([name print?]
     (let [system (aima-load-if-unloaded name)]
       (binding [*print-pretty* true,
                 *out* (if print? *out*
                           sink-writer)]
         (cond (not system) (println "No such system as" (str name \.)),

               (and (not (aima-system-examples system))
                    (every? symbol? (aima-system-parts system)))
               (sum (aima-system-parts system) #(test % print?)),

               :else
               (let [_ (when print? (println "Testing System" name)),
                     errors (count (remove #(test-example % print?)
                                           (aima-system-examples system)))]
                 (when print?
                   ;; *err* is closest equivalent to Common Lisp *DEBUG-IO*
                   (binding [*out* *err*]
                     (println errors (str "error" (if (not= errors 1) "s"))
                              "on system" name)))
                 errors))))))
