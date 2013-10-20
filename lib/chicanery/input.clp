; /lib/chicanery/input.clp - Contains methods and classes for handling the
; chicanery input layer

(defgeneric translate/kbd/query)
(defgeneric translate/mouse/buttons)
(defgeneric bool)
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
