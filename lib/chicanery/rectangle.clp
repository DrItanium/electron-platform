(defgeneric defrectangle)
(defgeneric quickrect)
(defgeneric defpixel)
(defgeneric pixel:nxn)
(defgeneric pixel:1x1)

(defclass Rectangle
  (is-a Pointer)
  (slot pointer-class
        (source composite)
        (default Rectangle))
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
  (slot bx
        (type INTEGER)
        (access initialize-only)
        (storage local)
        (default ?NONE))
  (slot by
        (type INTEGER)
        (access initialize-only)
        (storage local)
        (default ?NONE))
  (message-handler get-points primary))

(defmessage-handler Rectangle get-points () 
                    (create$ ?self:x ?self:y ?self:bx ?self:by))


(defmethod defrectangle 
  "Creates a new rectangle object and corresponding pointer"
  ((?name SYMBOL INSTANCE-NAME)
   (?x INTEGER)
   (?y INTEGER)
   (?bx INTEGER)
   (?by INTEGER))
  (make-instance ?name of Rectangle 
                 (x ?x) 
                 (y ?y)
                 (bx ?bx)
                 (by ?by)
                 (pointer (quickrect ?x ?y ?bx ?by))))
(defmethod defrectangle
  ((?x INTEGER)
   (?y INTEGER)
   (?bx INTEGER)
   (?by INTEGER))
  (defrectangle (gensym*) ?x ?y ?bx ?by))


(defmethod quickrect
  ((?x INTEGER)
   (?y INTEGER)
   (?bx INTEGER)
   (?by INTEGER))
  (new Rectangle ?x ?y ?bx ?by))

(defmethod quickrect
  ((?mf MULTIFIELD INTEGER (>= (length$ ?mf) 4)))
  (quickrect (expand$ (subseq$ ?mf 1 4))))

(defmethod quickrect
  (($?mf MULTIFIELD INTEGER (>= (length$ ?mf) 4)))
  (quickrect ?mf))

(defmethod defpixel
  ((?name SYMBOL INSTANCE-NAME)
   (?x INTEGER)
   (?y INTEGER)
   (?factor INTEGER))
  (defrectangle ?name ?x ?y (+ ?x ?factor) (+ ?y ?factor)))

(defmethod pixel:nxn
  ((?x INTEGER)
   (?y INTEGER)
   (?n INTEGER))
  (quickrect ?x ?y (+ ?x ?n) (+ ?y ?n)))

(defmethod pixel:nxn
  ((?n INTEGER))
  (pixel:nxn 0 0 ?n))

(defmethod pixel:1x1
  ((?x INTEGER)
   (?y INTEGER))
  (pixel:nxn ?x ?y 1))

(defmethod pixel:1x1 () 
  (pixel:1x1 0 0))

(defglobal MAIN 
           ?*rect:single-pixel* = (send (defrectangle rect:single-pixel 0 0 1 1) get-pointer))
