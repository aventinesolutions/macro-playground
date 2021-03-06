(ns macro-playground.core
  (require [clojure.pprint :as p]))

(defmacro square [x]
  `(let [x# ~x]
     (* x# x#)))

(defmacro when* [test & body]
  `(if ~test
     (do
       ~@body)
     nil))

(defmacro while* [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defmacro regex [re s & body]
  `(let [match# (re-find ~re ~s)]
     (when match#
       (let [[~'%0 ~'%1 ~'%2 ~'%3 ~'%4 ~'%5 ~'%6 ~'%7 ~'%8]
             (if (string? match#)
               [match#]
               match#)]
         ~@body))))

(defn with-open*-fn [to-close f]
  (try
    (apply f to-close)
    (finally
      (doseq [c (reverse to-close)]
        (.close c)))))

(defmacro with-open* [bindings & body]
  (let [pairs (partition 2 bindings)]
    `(with-open*-fn ~(mapv second pairs)
       (fn ~(mapv first pairs)
         ~@body))))

;; alternative implementation
(defmacro with-open2* [bindings & body]
  `(let ~bindings
     (try
       ~@body
       (finally
         ~@(for [[sym _] (reverse (partition 2 bindings))]
             `(.close ~sym))))))

;; (mf or) => (fn [a b] (or a b))

(defmacro mf [m]
  (let [letters "abcdefghijklmnop"
        syms (map (comp gensym str) letters)]
    `(fn
       ~@(for [n (range (inc (count letters)))]
           `([~@(take n syms)] (~m ~@(take n syms)))))))

(defn -main []
  (p/pprint (macroexpand-1 '(regex #"a(bc)" "abc" (println %0)(println %1))))
  (p/pprint (macroexpand '(regex #"([aA]ve)" "Aventine Solutions (matthew.eichler@aventinesolutions.nl" (println %0)(println %1))))
  (println (regex #"([aA]ve)" "Aventine Solutions (matthew.eichler@aventinesolutions.nl" (println %0)(println %1)))
  (p/pprint (macroexpand-1 '(with-open* [in (clojure.java.io/input-stream (clojure.java.io/file "/tmp/test.txt"))](println (slurp in)))))
  (p/pprint (macroexpand-1 '(with-open2* [in (clojure.java.io/input-stream (clojure.java.io/file "/tmp/test.txt"))
                                          out (clojure.java.io/writer (clojure.java.io/file "/tmp/out.txt"))](println (slurp in)))))
  (p/pprint (macroexpand-1 '(apply (mf or) [false true nil false false false]))))

