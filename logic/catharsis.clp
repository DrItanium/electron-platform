; Catharsis is the engine abstraction over chicanery
(load* /lib/core.clp)
(load* /lib/chicanery.clp)

(defclass slice
  "Represents a slice of the viewport"
  (is-a rectangle)
  (slot replicate
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)
        (visibility public))
  (slot color
        (type INSTANCE INTEGER)
        (allowed-classes color)
        (storage local)
        (default-dynamic 0))
  (message-handler get-image-pointer primary))
(defmessage-handler slice get-image-pointer primary
                    ()
                    (new Image ?self:pointer
                         (if ?self:replicate then 1 else 0)
                         (if (instancep ?self:color) then
                           (send ?self:color to-native-color)
                           else
                           ?self:color)))

(defmethod screen/draw
  ((?slice slice)
   (?p point))
  (screen/draw (send ?slice get-pointer)
               (send ?slice get-image-pointer)
               (new Image)
               (send ?p get-pointer)))

(definstances engine-pieces
              (background of slice
                          (x 0)
                          (y 0)
                          (bx 1920)
                          (by 1080)
                          (replicate TRUE)
                          (color (get-standard-color black)))
              (playfield of slice
                         (x 0)
                         (y 0)
                         (bx 1280)
                         (by 720)
                         (replicate TRUE)
                         (color (get-standard-color paleyellow)))
              (inventory of slice
                         (x 1283)
                         (y 3)
                         (bx 1920)
                         (by 720)
                         (replicate TRUE)
                         (color (get-standard-color medblue)))
              (log of slice
                   (x 3)
                   (y 723)
                   (bx 1280)
                   (by 1077)
                   (replicate TRUE)
                   (color (get-standard-color white)))
              (misc of slice
                    (x 1283)
                    (y 723)
                    (bx 1917)
                    (by 1077)
                    (replicate TRUE)
                    (color (get-standard-color white))))

(deffacts query-operation
          (query input))
(defgeneric engine-update)

; Handle on-resized
(defmethod on-resized
  "method to handle resizing of the window"
  ((?value SYMBOL (not (neq ?value FALSE TRUE))))
  (if (and ?value (< (getwindow) 0)) then
    (printout werror "ERROR: couldn't reattach to window" crlf)
    (exit)
    else
    (engine-update)))


(defmethod engine-update
  ()
  (screen/draw [background] [ZP])
  (screen/draw [playfield] [ZP])
  (screen/draw [inventory] [ZP])
  (screen/draw [log] [ZP])
  (screen/draw [misc] [ZP])
  (screen/flush 1))

(defrule query-input
         ?f <- (query input)
         =>
         (retract ?f)
         (send [mouse] query)
         (send [keyboard] query)
         (assert (check mouse)
                 (check keyboard)))

(defrule process-mouse-inputs
         (declare (salience -1))
         ?f <- (check mouse)
         =>
         (retract ?f)
         (assert (query mouse)))
(defrule process-mouse-inputs:button1
         ?f <- (check mouse)
         (object (is-a mouse)
                 (name [mouse])
                 (buttons button1)
                 (position ?x ?y))
         (object (is-a slice)
                 (name ?name))
         (test (intersects ?x ?y ?name))
         =>
         (retract ?f)
         (assert (query mouse))
         (quickmenu/show (create$ (instance-name-to-symbol ?name)) 1))

(defrule process-keyboard-inputs:quit
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ESC))
         =>
         (retract ?f)
         (exit))

(defrule process-keyboard-inputs:nil
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys NIL))
         =>
         (retract ?f)
         (assert (query keyboard)))

(defrule process-keyboard-inputs
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ?b&~NIL))
         =>
         (retract ?f)
         (printout t "Pressed " ?b crlf)
         (assert (query keyboard)))

(defrule ready-to-query-input-again 
         ?f <- (query keyboard)
         ?f2 <- (query mouse)
         =>
         (retract ?f ?f2)
         (send [mouse] clear)
         (send [keyboard] clear)
         (assert (query input)))

(batch* /lib/reset-run-exit.clp)
