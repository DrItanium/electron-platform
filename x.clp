(defclass Pointer
  (is-a USER)
  (slot pointer 
        (type EXTERNAL-ADDRESS)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot pointer-class
        (type SYMBOL)
        (storage shared)
        (create-accessor read)
        (visibility public)
        (access read-only)
        (default unknown)))
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
; /lib/chicanery/menu.clp - Contains methods and classes to simplify accessing
; menus
(defgeneric quickmenu)
(defgeneric quickmenu/show)
(defgeneric defmenu)
(defgeneric translate-menu-id)
(defclass Menu
  (is-a Pointer)
  (slot pointer-class 
        (source composite)
        (default menu))
  (multislot menu-elements
             (type LEXEME)
             (storage local)
             (access initialize-only)
             (default ?NONE))
  (message-handler show-menu primary))

(defmessage-handler Menu show-menu primary 
                    "shows the target menu and translates it to the corresponding symbolic
                    representation"
                    (?button)
                    (translate-menu-id (menu/show ?self:pointer ?button)
                                       ?self:menu-elements))

(defmethod quickmenu 
  "Defines a menu pointer without any associated object"
  (($?entries LEXEME))
  (quickmenu ?entries))

(defmethod quickmenu
  "Defines a menu pointer without any associated object"
  ((?entries MULTIFIELD LEXEME))
  (new menu (expand$ ?entries)))

(defmethod defmenu
  "Constructs a Menu object and corresponding pointer"
  ((?name SYMBOL)
   ($?entries LEXEME))
  (defmenu ?name ?entries))
(defmethod defmenu
  "Constructs a Menu object and corresponding pointer"
  ((?name SYMBOL)
   (?entries MULTIFIELD LEXEME))
  (make-instance ?name of Menu
                 (pointer (quickmenu ?entries))
                 (menu-elements ?entries)))

(defmethod translate-menu-id
  "If we get -1 back then return the nil symbol"
  ((?id INTEGER (= ?id -1))
   (?elements MULTIFIELD LEXEME))
  nil)

(defmethod translate-menu-id
  "Translates numeric input from menu/show to symbolic representation"
  ((?id INTEGER (> ?id -1))
   (?elements MULTIFIELD LEXEME))
  ; We are one indexed while libevent is zero indexed
  (return (nth$ (+ ?id 1) ?elements)))

(defmethod translate-menu-id
  "Translates numeric input from menu/show to symbolic representation"
  ((?id INTEGER)
   ($?elements LEXEME))
  (translate-menu-id ?id ?elements))

(defmethod quickmenu/show 
  "Construct a quick list and return a symbolic representation"
  ((?elements MULTIFIELD LEXEME)
   (?button INTEGER))
  (translate-menu-id (menu/show (quickmenu ?elements) ?button) ?elements))
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

(definstances default-rect-classes
              (rect:single-pixel of Rectangle
                (x 0)
                (y 0)
                (bx 1)
                (by 1)
                (pointer (quickrect 0 0 1 1))))

; /lib/chicanery/color.clp - Routines for handling chicanery color features

(defgeneric translate/standard-color)
(defgeneric intensity-to-cmap)
(defmethod translate/standard-color
  ((?color LEXEME))
  (switch (lowcase (string-to-field ?color))
          (case opaque then 0)
          (case transparent then 1)
          (case black then 2)
          (case white then 3)
          (case red then 4)
          (case green then 5)
          (case blue then 6)
          (case cyan then 7)
          (case magenta then 8)
          (case yellow then 9)
          (case paleyellow then 10)
          (case darkyellow then 11)
          (case darkgreen then 12)
          (case palegreen then 13)
          (case medgreen then 14)
          (case darkblue then 15)
          (case palebluegreen then 16)
          (case paleblue then 17)
          (case bluegreen then 18)
          (case greygreen then 19)
          (case palegreygreen then 20)
          (case yellowgreen then 21)
          (case medblue then 22)
          (case greyblue then 23)
          (case palegreyblue then 24)
          (case purpleblue then 25)
          (case notacolor then 26)
          (case nofill then 27)
          (default 28)))

(defmethod get-standard-color
  ((?color LEXEME INSTANCE-NAME))
  (get-standard-color 
    (translate/standard-color ?color)))

(defmethod intensity-to-cmap
  "Converts an intensity value to a greyscale color"
  ((?intensity INTEGER (<= 0 ?intensity 255)))
  (rgb-to-cmap ?intensity
               ?intensity
               ?intensity))

(defmethod rgb-to-cmap
  ((?tuple INTEGER MULTIFIELD (>= (length$ ?tuple) 3)))
  (rgb-to-cmap (nth$ 1 ?tuple)
               (nth$ 2 ?tuple)
               (nth$ 3 ?tuple)))

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
