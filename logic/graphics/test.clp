(defglobal MAIN
           ?*system-initialized* = FALSE)
(deffacts query-operation
          (query mouse))
(defmethod bool ((?number INTEGER)) (if (= ?number 0) then FALSE else TRUE))
(defmethod translate/mouse/buttons
  ()
  ; Mouse combinations
  ; m.buttons & 1 => left-click
  ; m.buttons & 2 => middle-click 
  ; m.buttons & 3 => left + middle
  ; m.buttons & 4 => right-click 
  ; m.buttons & 5 => left + right
  ; m.buttons & 6 => middle + right 
  ; m.buttons & 7 => left + middle + right
  (bind ?z (mouse/buttons))
  (return (create$ 
            (if (bool (binary-and ?z 1)) then button1 else (create$))
            (if (bool (binary-and ?z 2)) then button2 else (create$))
            (if (bool (binary-and ?z 4)) then button3 else (create$)))))

(defrule initialize 
         (declare (salience 10000))
         (initial-fact)
         =>
         (if (not ?*system-initialized*) then
           (initdraw)
           (mouse/init)
           (eresized 0)
           (bind ?*system-initialized* TRUE)))

(defrule on-resized
         ?f <- (event resized new ?value)
         =>
         (if (and ?value (< (getwindow) 0)) then
           (printout werror "ERROR: couldn't reattach to window" crlf)
           (exit)
           else
           (retract ?f)))
(defrule query-mouse
         ?f <- (query mouse)
         =>
         (retract ?f)
         (mouse/query)
         (assert (mouse input buttons: (translate/mouse/buttons) 
                        position: (mouse/position)
                        time-stamp: (mouse/timestamp))))


(defrule process-mouse-inputs
         ?f <- (mouse input
                      buttons: $?z&:(not (member$ button3 ?z))
                      position: ? ?
                      time-stamp: ?)
         =>
         (retract ?f)
         (assert (query mouse)))


(defrule process-mouse-inputs:exit
         ?f <- (mouse input 
                      buttons: $? button3 $? 
                      position: ? ?
                      time-stamp: ?)
         =>
         (retract ?f)
         (exit))
