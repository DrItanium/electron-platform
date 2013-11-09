(load* /lib/chicanery/input.clp)
(load* /lib/chicanery/menu.clp)

(defglobal MAIN
	   ?*system-initialized* = FALSE)

(deffacts query-operation
	  (query input)
	  (defmenu menu1 cut copy paste)
	  (defmenu menu2 eat sleep drink))

(defrule initialize 
	 (declare (salience 10000))
	 (initial-fact)
	 =>
	 (if (not ?*system-initialized*) then
	   (eresized 0)
	   (bind ?*system-initialized* TRUE)))

(defrule build-menus
	 (declare (salience 9999))
	 ?f <- (defmenu ?a $?b)
	 =>
	 (retract ?f)
	 (defmenu ?a ?b))

(defrule on-resized
	 (declare (salience 1000))
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
	 (declare (salience -1))
	 ?f <- (input mouse
		      buttons: $?
		      position: ? ?
		      time-stamp: ?)
	 =>
	 (retract ?f)
	 (assert (query mouse)))


(defrule process-mouse-inputs:menu1
	 ?f <- (input mouse 
		      buttons: button3 
		      position: ? ?
		      time-stamp: ?)
	 =>
	 (retract ?f)
	 ; Display a menu
	 (printout t (send [menu1] show-menu 3) crlf)
	 (assert (query mouse)))

(defrule process-mouse-inputs:menu2
	 ?f <- (input mouse 
		      buttons: button2
		      position: ? ?
		      time-stamp: ?)
	 =>
	 (retract ?f)
	 ; Display a menu
	 (printout t (send [menu2] show-menu 2) crlf)
	 (assert (query mouse)))


(defrule process-keyboard-inputs:quit
	 (declare (salience 1))
	 ?f <- (input keyboard 
		      button: ESC)
	 =>
	 (retract ?f)
	 (exit))

(defrule process-keyboard-inputs:nil
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
