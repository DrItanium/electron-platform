; point.clp - classes and methods to wrap around the Point native type

(defgeneric quickpoint)
(defgeneric defpoint)
(defgeneric to-point)
(defclass Point
  (is-a Pointer)
  (slot pointer-class 
        (source composite)
        (default Point))
  (slot x
        (type INTEGER)
        (access initialize-only)
        (storage local)
        (default ?NONE))
  (slot y
        (type INTEGER)
        (access initialize-only)
        (storage local)
        (default ?NONE))
  (message-handler get-points primary))

(defmessage-handler Point get-points ()
                    (create$ ?self:x ?self:y))

(defmethod defpoint
  "Creates a new point object and corresponding pointer"
  ((?name SYMBOL INSTANCE-NAME)
   (?x INTEGER)
   (?y INTEGER))
  (make-instance ?name of Point
                 (x ?x)
                 (y ?y)
                 (pointer (quickpoint ?x ?y))))

(defmethod defpoint
  ((?x INTEGER)
   (?y INTEGER))
  (defpoint (gensym*) ?x ?y))

(defmethod quickpoint
  ((?x INTEGER)
   (?y INTEGER))
  (new Point ?x ?y))

(defmethod to-point
  ((?mf INTEGER MULTIFIELD (= (length$ ?mf) 2)))
  (quickpoint (expand$ ?mf)))

(defmethod to-point
  ((?mf INTEGER MULTIFIELD (> (length$ ?mf) 2)))
  (quickpoint (nth$ 1 ?mf)
              (nth$ 2 ?mf)))
(defmethod to-point
  (($?mf INTEGER (>= (length$ ?mf) 2)))
  (to-point ?mf))

(definstances default-point-classes
              (ZP of Point 
                  (x 0) 
                  (y 0)
                  (pointer (quickpoint 0 0))))
