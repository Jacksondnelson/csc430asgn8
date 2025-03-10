(defn numC [num] 
  {:tag :numC :n num})

(defn strC [str]
  {:tag :strC :s str})

(defn idC [sym]
  {:tag :idC :s sym})

(def num1 (numC 20))
(print (get num1 :n))

(def str1 (strC "Hello"))
(print (get str1 :s))

(def idc1 (idC 'x))
(print (get idc1 :s))

(defn interp [expr]
  (cond
    (= (expr :tag) :numC) (print (get expr :n))
    (= (expr :tag) :strC) (print (get expr :s))
    (= (expr :tag) :idC) (print (get expr :s))
    true (print "Unknown expression type"))
)


(interp (numC 10))
(interp (strC "No"))
(interp (idC 'z))
