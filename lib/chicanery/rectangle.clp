(defgeneric defrectangle)
(defgeneric quickrect)
(defglobal MAIN 
           ?*rect:single-pixel* = (send (defrectangle rect:single-pixel 0 0 1 1) get-pointer))

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
  ((?name SYMBOL INSTANCE-NAME)
   (?ptr EXTERNAL-ADDRESS))
  (make-instance ?name of Rectangle 
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
  (($?mf MULTIFIELD INTEGER (>= (length$ ?mf 4))))
  (quickrect ?mf))
