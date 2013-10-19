(defglobal MAIN
           ?*system-initialized* = FALSE)
(deffacts query-operation
          (query input))
(defgeneric translate/kbd/query)
(defgeneric translate/mouse/buttons)
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

(defmethod translate/kbd/query
  ()
  ; This method has to be defined by the programmer because it is application
  ; specific. By default, if we don't know what the value is then just return
  ; the original rune value.
  (bind ?rune (kbd/query))
  (return (switch ?rune
          (case -1 then NIL)
          (case 0 then NIL)
          (case 27 then ESC)
          (case 61454 then UP)
          (case 63488 then DOWN) ; Plan9's down :/
          (case 128 then DOWN) ; this is the value I get for down
          (case 61457 then LEFT)
          (case 61458 then RIGHT)
          (default ?rune))))

(defrule initialize 
         (declare (salience 10000))
         (initial-fact)
         =>
         (if (not ?*system-initialized*) then
           (initdraw)
           (input/init)
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
(defrule query-input 
         ?f <- (query input)
         =>
         (retract ?f)
         (mouse/query)
         (assert (input mouse
                        buttons: (translate/mouse/buttons)
                        position: (mouse/position)
                        time-stamp: (mouse/timestamp))
                 (input keyboard
                        button: (translate/kbd/query))))



(defrule process-mouse-inputs
         ?f <- (input mouse
                      buttons: $?z&:(not (member$ button3 ?z))
                      position: ? ?
                      time-stamp: ?)
         =>
         (retract ?f)
         (assert (query mouse)))


(defrule process-mouse-inputs:menu
         ?f <- (input mouse 
                      buttons: $? button3 $? 
                      position: ? ?
                      time-stamp: ?)
         =>
         (retract ?f)
         ; Display a menu
         (assert (query mouse))
         )


(defrule process-keyboard-inputs:quit
         (declare (salience 1))
         ?f <- (input keyboard button: ESC)
         =>
         (retract ?f)
         (exit))

(defrule process-keyboard-inputs:passthrough
         (declare (salience 1))
         ?f <- (input keyboard button: NIL)
         =>
         (retract ?f)
         (assert (query keyboard)))

(defrule process-keyboard-inputs
         ?f <- (input keyboard
                      button: ?b)
         =>
         (retract ?f)
         (printout t "Pressed " ?b crlf)
         (assert (query keyboard)))

(defrule ready-to-query-input-again 
         ?f <- (query keyboard)
         ?f2 <- (query mouse)
         =>
         (retract ?f ?f2)
         (assert (query input)))
