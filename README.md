straightforward
===============

This Clojure code is a straightforward translation of the Common Lisp code that Peter Norvig wrote to implement the algorithms in [*Artificial Intelligence: a Modern Approach*](http://aima.cs.berkeley.edu/) by Peter Norvig and Stuart Russell.  The code is more pedagogical than practical.

The code maintains the names, style, and algorithms of the Common Lisp original but uses Clojure idioms.
Names in *lisp-2* Common Lisp that conflict with standard *lisp-1* Clojure names typically have aima inserted or are converted to the typical short names of `clojure.core`.
Non-dynamic Common Lisp names, such as parameters, do not have earmuffs (asterisks) in Clojure.
Using standard Clojure libraries, such as `clojure.test`, and a few Java collections, like `java.util.ArrayList`, follow typical Clojure style.

Most algorithms maintain the Common Lisp structure except that:
- Clojure seqs, often implemented as vectors, replace many Common Lisp lists;
- Clojure maps, typically hash-maps, or Clojure defrecords, when inheritance is approrpriate, replace Common Lisp `DEFSTRUCT` records;
- Clojure destructuring arguments replace more complicated Common Lisp;
- Clojure multi form functions replace Common Lisp optional arguments;
- Clojure let forms and synchronization constructs replace Common Lisp `SETF`;
- Clojure loop-recur and trampoline replace some Common Lisp recursion, which automatically optimizes for tail recursion;
- In order to match the directory heirarchy, Clojure namespaces appear more frequently than simply replacing Common Lisp packages;
- Clojure closures simulate CLOS; and
- Common Lisp debugging, restart, and logical file name capabilities have workarounds, since Clojure does not support them.

The code is intended to be tested with Clojure version 1.5.1, which is the last Clojure version that will run with Java 5, in order to remain compatible with a wide range of environments.
