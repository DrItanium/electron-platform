;------------------------------------------------------------------------------
;electron-platform
;Copyright (c) 2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
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
(defgeneric quickpoint)
(defgeneric to-point)
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
(definstances default-points
              (ZP of point
                  (x 0)
                  (y 0)))

(defgeneric quickrect)
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
  (message-handler get-native-arguments primary)
  (message-handler intersects primary))

(defmessage-handler rectangle get-native-arguments primary
                    ()
                    (create$ ?self:x ?self:y ?self:bx ?self:by))

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

(definstances default-rectangle-types
              (pixel of rectangle
                     (x 0)
                     (y 0)
                     (bx 1)
                     (by 1)))

(defgeneric translate/standard-color)
(defgeneric intensity-to-cmap)
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

(defclass image
  (is-a native-pointer)
  (role concrete)
  (pattern-match reactive)
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
  ((?r INSTANCE-NAME)
   (?src INSTANCE-NAME)
   (?mask INSTANCE-NAME)
   (?p INSTANCE-NAME))
  (screen/draw (instance-address ?r)
               (instance-address ?src)
               (instance-address ?mask)
               (instance-address ?p)))

(defmethod screen/draw
  ((?r INSTANCE-NAME)
   (?src INSTANCE-NAME)
   (?p INSTANCE-NAME))
  (screen/draw (instance-address ?r)
               (instance-address ?src)
               (instance-address ?p)))

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
             (create-accessor read))
  (multislot buttons 
             (create-accessor read))
  (slot timestamp
        (type INTEGER)
        (create-accessor read))
  (message-handler clear primary)
  (message-handler query primary))

(defmessage-handler mouse clear primary
                    ()
                    (bind ?self:position (create$))
                    (bind ?self:buttons (create$))
                    (bind ?self:timestamp 0))

(defmessage-handler mouse query primary
                    ()
                    (bind ?out (mouse/query))
                    (if ?out then
                      (bind ?self:position (bind ?tmp (mouse/position)))
                      (bind ?self:buttons (translate/mouse/buttons))
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
                    (slot-direct-delete$ keys 1 ?self:length)
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
                                           (+ ?self:length 1)
                                           ?key)
                      (bind ?self:length (+ ?self:length 1))
                      else
                      (bind ?self:keys ?key)
                      (bind ?self:length 1))
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
        (default menu))
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
                                       ?self:menu-entries))

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
  "Handles the case when -1 is passed"
  ((?id INTEGER (= ?id -1))
   (?elements MULTIFIELD LEXEME))
  (return NIL))
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

; system initialization features
(defglobal MAIN
           ?*system-initialized* = FALSE)

(defrule initialize 
         (declare (salience 10000))
         (initial-fact)
         =>
         (if (not ?*system-initialized*) then
           (eresized 0)
           (bind ?*system-initialized* TRUE)))

; on-resized handling

(defgeneric on-resized)

(defrule on-resized
         "library defined rule which calls the on-resized method"
         (declare (salience 10000))
         ?f <- (event resized new ?value)
         =>
         (retract ?f)
         (on-resized ?value))

; intersection methods for points and rectangles
(defgeneric intersects)

(defmethod intersects 
  "Checks to see if the given point intersects with the given area"
  ((?px NUMBER)
   (?py NUMBER)
   (?rx NUMBER)
   (?ry NUMBER)
   (?rbx NUMBER)
   (?rby NUMBER))
  (and (<= ?rx ?px ?rbx)
       (<= ?ry ?py ?rby)))

(defmethod intersects
  ((?px NUMBER)
   (?py NUMBER)
   (?rectangle rectangle))
  =>
  (send ?rectangle intersects ?px ?py))

(defmessage-handler rectangle intersects primary
                    (?px ?py)
                    (intersects ?px ?py ?self:x ?self:y ?self:bx ?self:by))

(defgeneric screen/dimensions/width)
(defgeneric screen/dimensions/height)
(defgeneric as-point:screen/dimensions)

(defmethod screen/dimensions/width
  ()
  (nth$ 3 (screen/dimensions)))

(defmethod screen/dimensions/height
  ()
  (nth$ 4 (screen/dimensions)))

(defmethod as-point:screen/dimensions
  ()
  (new Point (screen/dimensions/width)
       (screen/dimensions/height)))

