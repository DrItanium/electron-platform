(defclass native-pointer
  (is-a USER)
  (role abstract)
  (slot pointer-class
        (type LEXEME)
        (visibility public)
        (access read-only)
        (default nil))
  (slot pointer
        (type EXTERNAL-ADDRESS)
        (storage local)
        (visibility public))
  (message-handler init around)
  (message-handler build-pointer primary)
  (message-handler get-native-arguments primary))

(defmessage-handler native-pointer init around 
                    ()
                    (call-next-handler)
                    (send ?self build-pointer))

(defmessage-handler native-pointer build-pointer primary
                    ()
                    (bind ?self:pointer
                          (new (dynamic-get pointer-class)
                               (expand$ (send ?self get-native-arguments)))))

(defclass point
  (is-a native-pointer)
  (role concrete)
  (pattern-match reactive)
  (slot pointer-class
        (source composite)
        (default Point))
  (slot x 
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot y
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler get-native-arguments primary))

(defmessage-handler point get-native-arguments primary () 
                    (create$ ?self:x ?self:y))
(definstances default-points
              (ZP of point
                  (x 0)
                  (y 0)))
(defclass rectangle
  (is-a native-pointer)
  (role concrete)
  (pattern-match reactive)
  (slot pointer-class
        (source composite)
        (default Rectangle))
  (slot x 
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot y
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot bx 
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot by
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler get-native-arguments primary))

(defmessage-handler rectangle get-native-arguments primary
                    ()
                    (create$ ?self:x ?self:y ?self:bx ?self:by))

(definstances default-rectangle-types
              (pixel of rectangle
                     (x 0)
                     (y 0)
                     (bx 1)
                     (by 1)))
(defclass color
  (is-a USER)
  (slot red
        (type INTEGER)
        (range 0 255)
        (visibility public))
  (slot green
        (type INTEGER)
        (range 0 255)
        (visibility public))
  (slot blue
        (type INTEGER)
        (range 0 255)
        (visibility public))
  (message-handler to-native-color primary))

(defmessage-handler color to-native-color primary 
                    ()
                    (rgb-to-cmap ?self:red
                                 ?self:green
                                 ?self:blue))
(defclass image
  (is-a native-pointer)
  (slot pointer-class
        (source composite)
        (default Image))
  (slot rectangle
        (type INSTANCE)
        (allowed-classes rectangle)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot replicate
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)
        (visibility public))
  (slot color
        (type INSTANCE INTEGER)
        (allowed-classes color)
        (storage local)
        (default-dynamic 0))
  (message-handler get-native-arguments primary))

(defmessage-handler image get-native-arguments primary
                    ()
                    (create$ (send ?self:rectangle get-pointer)
                             (if ?self:replicate then 1 else 0)
                             (if (instancep ?self:color) then
                               (send ?self:color to-native-color)
                               else
                               ?self:color)))
(defmethod screen/draw
  ((?r rectangle)
   (?src image)
   (?mask image)
   (?p point))
  (screen/draw (send ?r get-pointer)
               (send ?src get-pointer)
               (send ?mask get-pointer)
               (send ?p get-pointer)))

(defmethod screen/draw
  ((?r rectangle)
   (?src image)
   (?p point))
  (screen/draw (send ?r get-pointer)
               (send ?src get-pointer)
               (new Image)
               (send ?p get-pointer)))

(defglobal MAIN
           ?*default-screen-flush-behavior* = TRUE)
(defmethod screen/flush
  ((?vis SYMBOL (not (neq ?vis TRUE FALSE))))
  (screen/flush (if ?vis then 1 else 0)))

(defmethod screen/flush 
  ()
  (screen/flush ?*default-screen-flush-behavior*))

(defmethod screen/draw-text
  ((?p point)
   (?src image)
   (?sp point)
   (?str LEXEME))
  (screen/draw-text (send ?p get-pointer)
                    (send ?src get-pointer)
                    (send ?sp get-pointer)
                    ?str))

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
(defclass mouse
  (is-a USER)
  (multislot position
             (type INTEGER)
             (create-accessor read))
  (multislot buttons 
             (type SYMBOL)
             (create-accessor read))
  (multislot timestamp
             (type INTEGER)
             (create-accessor read))
  (message-handler query primary))

(defmessage-handler mouse query primary
                    ()
                    (bind ?out (mouse/query))
                    (if ?out then
                      (bind ?self:position (bind ?tmp (mouse/position)))
                      (bind ?self:buttons
                            (translate/mouse/buttons (mouse/buttons)))
                      (bind ?self:timestamp (mouse/timestamp)))
                    (return ?out))
(definstances mouse-object
              (mouse of mouse))

(defclass keyboard
  (is-a USER)
  (multislot keys
             (type SYMBOL INTEGER)
             (create-accessor read))
  (slot length
        (type INTEGER)
        (create-accessor read))
  ; behavior slots
  ; should we append to the list of keys?
  (slot append-on-query
        (type SYMBOL)
        (allowed-symbols FALSE TRUE))
  ; Should we attempt to convert from raw codes to symbols?
  (slot raw-keycodes
        (type SYMBOL)
        (allowed-symbols FALSE TRUE))
  (message-handler clear primary)
  (message-handler query primary))

(defmessage-handler keyboard clear primary 
                    () 
                    (bind ?self:keys (create$))
                    (bind ?self:length 0))

(defmessage-handler keyboard query primary
                    "Get a key from the native runtime queue"
                    ()
                    (bind ?key (if ?self:raw-keycodes then
                                 (kbd/query)
                                 else
                                 (translate/kbd/query)))

                    (if ?self:append-on-query then
                      (slot-direct-insert$ keys 
                                           (+ ?self:index 1)
                                           ?key)
                      else
                      (bind ?self:keys (create$ ?key)))
                    (bind ?self:length (+ ?self:length 1))
                    (return ?key))

(definstances keyboard-interface
              (keyboard of keyboard))

(defgeneric quickmenu)
(defgeneric quickmenu/show)
(defgeneric translate-menu-id)
(defclass menu
  (is-a native-pointer)
  (role concrete)
  (pattern-match reactive)
  (slot pointer-class
        (source composite)
        (default Menu))
  (multislot menu-entries 
             (type LEXEME)
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler get-native-arguments primary)
  (message-handler show-menu primary))

(defmessage-handler menu get-native-arguments primary
                    ()
                    ?self:menu-entries)
(defmessage-handler menu show-menu primary 
                    "shows the target menu and translates it to the corresponding symbolic
                    representation"
                    (?button)
                    (translate-menu-id (menu/show ?self:pointer ?button)
                                       ?self:menu-elements))

(defmethod quickmenu/show 
  "Construct a quick list and return a symbolic representation"
  ((?elements MULTIFIELD LEXEME)
   (?button INTEGER))
  (translate-menu-id (menu/show (quickmenu ?elements) ?button) ?elements))

(defmethod quickmenu 
  "Defines a menu pointer without any associated object"
  (($?entries LEXEME))
  (quickmenu ?entries))

(defmethod quickmenu
  "Defines a menu pointer without any associated object"
  ((?entries MULTIFIELD LEXEME))
  (new menu (expand$ ?entries)))

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
