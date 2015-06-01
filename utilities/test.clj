;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/test.lisp
(ns ^{:author "Translated from Common Lisp AIMA code by
Robert L. Kirby http://www.linkedin.com/in/bobkirby and
Paul O'Rorke http://www.linkedin.com/in/paulororke"}
utilities.test
  "Tests for basic utility functions used throughout the AIMA Common Lisp code."
  (:use aima)
  (:use clojure.test)
  (:use utilities.utilities)
  )

;;;; Test cases for the basic utilities
;;;; Note: this translation to Clojure uses clojure.test rather than the test facility provided in AIMA

(testing "utilities"

  (testing "Test all the utility functions."

         ;"Test the CLOS implementation"
         ;((defstruct shape))
         ;((defstruct (triangle (:include shape))
         ;               base height))
         ;((defstruct (rectangle (:include shape))
         ;               height width))
         ;((defmethod area ((x triangle))
         ;            (* 1/2 (triangle-base x) (triangle-height x))))
         ;((defmethod area ((x rectangle))
         ;            (* (rectangle-height x) (rectangle-width x))))
         ;((area (make-triangle :base 10 :height 10)) (equal * 50))
         ;((area (make-rectangle :width 10 :height 10)) (equal * 100))
         ;((defmethod features ((x shape))
         ;            '(shapely)))
         ;((defmethod features ((x triangle))
         ;            (cons (if (eql 0 (triangle-base x)) 'line 'triangle)
         ;                  (call-next-method))))
         ;((features (make-triangle :base 0 :height 10)) (equal * '(line shapely)))
         ;((features (make-triangle :base 1 :height 10)) (equal * '(triangle shapely)))

         (testing "Now, some operations on lists."
           (is (length>1 '(a b c)))
           (is (let [r (random-element '(a b c))] (some #(= r %) '(a b c))))
           (is (= (mappend #'reverse '((a b c) (1 2 3)) '(c b a 3 2 1))))
           (is (starts-with '(hi there) 'hi))
           (is (= (last1 '(a b c)) 'c))
           (is (= (transpose '((a b c) (d e f))) '((a d) (b e) (c f))))
           ; drop deletef which has side effects
           ; replace any calls to deletef with clojure filter
           (is (let [l (filter #(not (= 'a %)) '(a b c))]
                 (= l '(b c))))
         )

         (testing "Now for 2-dimensional points."
           (is (= (xy-add (make-xy 1 2) (make-xy 10 20)) (make-xy 11 22)))
           ; Clojure numerical equivalence == is used rather than = when comparing integers and floating point numbers
           (is (== (xy-distance (make-xy 0 0) (make-xy 3 4)) 5))
           )

         (testing "Numeric utilities"
           (is  (= (average '(10 20 30)) 20))
           (is (= (sum '(10 20 30)) 60))
           (is (= (sum '(1 2 3) #'square) 14))
           (is (let [i (random-integer 8 10)] (some #(= i %) '(8 9 10))))
           (is (<= 9 (fuzz 10) 11))
           (is (< 3.139 (round-off 3.14159 0.01) 3.141))
           )
         )
  )
         ;
         ;"Other"
         ;((stringify '(a b c)) (equalp * "(A B C)"))
         ;((concat-symbol 'a 1) (eq * 'a1))
         ;((funcall (compose #'- #'sqrt) 16) (= * -4))
         ;((setq nums '(1 2 3 4 -5 -2 -1)))
         ;((the-biggest #'identity nums) (eql * 4))
         ;((the-biggest #'abs nums) (eql * -5))
         ;((the-biggest-that #'identity #'oddp nums) (eql * 3))
         ;((the-smallest-random-tie #'abs nums) (member * '(1 -1)))
         ;
         ;"Now test the priority queue code."
         ;((heap-sort '(1 4 3 5 2 0)) (equal * '(0 1 2 3 4 5)))
         ;((heap-sort '(1 4 3 5 2 6) :key #'-) (equal * '(6 5 4 3 2 1)))
         ;
         ;"Now destructuring-bind"
         ;((destructuring-bind ((a . b) c &rest d &key e (f 5)) '((1 . 2) 3 :e 4)
         ;                     (list a b c d e f)) (equal * '(1 2 3 (:e 4) 4 5)