; /lib/chicanery/menu.clp - Contains methods and classes to simplify accessing
; menus
(defgeneric quickmenu)
(defgeneric quickmenu/show)
(defgeneric defmenu)
(defgeneric translate-menu-id)
(defclass Menu
  (is-a USER)
  (slot pointer 
        (type EXTERNAL-ADDRESS)
        (access initialize-only)
        (storage local)
        (default ?NONE))
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
