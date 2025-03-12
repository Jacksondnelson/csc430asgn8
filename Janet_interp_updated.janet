# Value Constructors (defined first to avoid reference errors)
(defn numV [n] 
  {:tag :numV :n n})

(defn strV [s] 
  {:tag :strV :s s})

(defn boolV [b] 
  {:tag :boolV :b b})

(defn procV [params body env]
  {:tag :procV :params params :body body :env env})

# AST Constructors
(defn numC [num] 
  {:tag :numC :n num})

(defn strC [str]
  {:tag :strC :s str})

(defn idC [sym]
  {:tag :idC :s sym})

# Addition expression constructor
(defn plusC [left right]
  {:tag :plusC :left left :right right})

# If expression constructor
(defn ifC [cond then-expr else-expr]
  {:tag :ifC :cond cond :then then-expr :else else-expr})

# Boolean expression constructor
(defn boolC [b]
  {:tag :boolC :b b})

# Procedure expression constructor
(defn procC [params body]
  {:tag :procC :params params :body body})

# Application constructor
(defn appC [func args]
  {:tag :appC :func func :args args})

# Variable declaration constructor
(defn declareC [bindings body]
  {:tag :declareC :bindings bindings :body body})

# Environment functions
(defn empty-env [] @{})

(defn extend-env [env bindings vals]
  (def new-env (table/clone env))
  (var i 0)
  (while (< i (length bindings))
    (put new-env (get bindings i) (get vals i))
    (set i (+ i 1)))
  new-env)

(defn lookup [env sym]
  (if (env sym)
    (env sym)
    (do 
      (print "QWJZ error: unbound identifier: " sym)
      nil)))

# Basic interpreter that returns values
(defn interp [expr env]
  (cond
    (= (expr :tag) :numC) (numV (expr :n))
    
    (= (expr :tag) :strC) (strV (expr :s))
    
    (= (expr :tag) :idC) (lookup env (expr :s))
    
    (= (expr :tag) :boolC) (boolV (expr :b))
    
    (= (expr :tag) :plusC)
      (let [lv (interp (expr :left) env)
            rv (interp (expr :right) env)]
        (if (and (= (lv :tag) :numV) (= (rv :tag) :numV))
          (numV (+ (lv :n) (rv :n)))
          (do (print "QWJZ error: + expects numbers") nil)))
    
    (= (expr :tag) :ifC)
      (let [cv (interp (expr :cond) env)]
        (if (and (= (cv :tag) :boolV) (cv :b))
          (interp (expr :then) env)
          (interp (expr :else) env)))
    
    (= (expr :tag) :declareC)
      (let [bindings (expr :bindings)
            names (map |(get $ 0) bindings)
            exprs (map |(get $ 1) bindings)
            vals (map |(interp $ env) exprs)
            new-env (extend-env env names vals)]
        (interp (expr :body) new-env))
    
    # Procedure definition
    (= (expr :tag) :procC)
      (procV (expr :params) (expr :body) env)
    
    # Function application
    (= (expr :tag) :appC)
      (let [fv (interp (expr :func) env)
            arg-vals (map |(interp $ env) (expr :args))]
        (if (= (fv :tag) :procV)
          (let [proc-env (extend-env (fv :env) (fv :params) arg-vals)]
            (interp (fv :body) proc-env))
          (do (print "QWJZ error: not a procedure") nil)))
    
    true (do (print "Unknown expression type: " (expr :tag)) nil)))

# Create empty environment for tests
(def base-env (empty-env))

# Print test sections
(print "\n Basic AST Constructor Tests \n")

(def num1 (numC 20))
(print "numC: " (get num1 :n) "\n")

(def str1 (strC "Hello"))
(print "strC: " (get str1 :s) "\n")

(def idc1 (idC 'x))
(print "idC: " (get idc1 :s) "\n")

(print "\n Value Interpreter Tests \n")

(def result1 (interp (numC 15) base-env))
(print "numC result: " (get result1 :n) "\n")

(def result2 (interp (strC "Test") base-env))
(print "strC result: " (get result2 :s) "\n")

(def result3 (interp (plusC (numC 10) (numC 32)) base-env))
(print "plusC result: " (get result3 :n) "\n")

# If expression tests
(print "\n If Expression Tests \n")

(def if-result1 (interp (ifC (boolC true) (numC 42) (numC 0)) base-env))
(print "If (true) result: " (get if-result1 :n) "\n")

(def if-result2 (interp (ifC (boolC false) (numC 42) (numC 0)) base-env))
(print "If (false) result: " (get if-result2 :n) "\n")

# Variable declaration tests
(print "\n Variable Declaration Tests \n")

# Simple variable declaration and use
(def decl-expr (declareC [['x (numC 100)]] (idC 'x)))
(def decl-result (interp decl-expr base-env))
(print "Declare and use x=100: " (get decl-result :n) "\n")

# Multiple variable declaration
(def multi-decl (declareC [['x (numC 10)] ['y (numC 20)]] 
                         (plusC (idC 'x) (idC 'y))))
(def multi-result (interp multi-decl base-env))
(print "Declare x=10, y=20 and compute x+y: " (get multi-result :n) "\n")

# Nested declarations
(def nested-decl (declareC [['x (numC 5)]]
                           (declareC [['y (plusC (idC 'x) (numC 3))]]
                                     (plusC (idC 'x) (idC 'y)))))
(def nested-result (interp nested-decl base-env))
(print "Nested declarations x=5, y=x+3, compute x+y: " (get nested-result :n) "\n")

# Procedure and Application Tests
(print "\n Procedure and Application Tests \n")

# Define a simple procedure that adds 1 to its argument
(def inc-proc (procC ['x] (plusC (idC 'x) (numC 1))))
(def proc-val (interp inc-proc base-env))
(print "Procedure defined: " (proc-val :tag) "\n")

# Apply the procedure to an argument
(def app-expr (appC inc-proc [(numC 41)]))
(def app-result (interp app-expr base-env))
(print "Apply procedure to 41: " (get app-result :n) "\n")

# Define and use a procedure in a declaration
(def decl-proc (declareC [['add (procC ['x 'y] (plusC (idC 'x) (idC 'y)))]]
                       (appC (idC 'add) [(numC 5) (numC 7)])))
(def decl-proc-result (interp decl-proc base-env))
(print "Declare add procedure and call it with 5 and 7: " (get decl-proc-result :n) "\n")

# Test closures (lexical scoping)
(def closure-test 
  (declareC [['x (numC 10)]]
            (declareC [['f (procC ['y] (plusC (idC 'x) (idC 'y)))]]
                     (declareC [['x (numC 20)]]
                              (appC (idC 'f) [(numC 5)])))))
(def closure-result (interp closure-test base-env))
(print "Closure test (should be 15): " (get closure-result :n) "\n")
