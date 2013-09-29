; This does semi-nlp on the input from the prompt
; The nice thing is that the form I expect things is quite flexible but
; there is some order to it

(deftemplate token 
             (slot index
                   (type INTEGER)
                   (default ?NONE))
             (slot value
                   (type LEXEME)
                   (default ?NONE))
             (slot type
                   (type SYMBOL)
                   (default unknown)))
(defrule parse-input:tokenize
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (from ?ind)
                        (contents ?tok $?rest))
         =>
         (modify ?f (from (+ ?ind 1))
                 (contents $?rest))
         (assert (token (index ?ind)
                        (value ?tok))))

(defrule parse-input:finish-tokenizing
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (from ?)
                        (contents))
         =>
         (retract ?f))


;-----------------------------------------------------------------------------
; Tokens are processed specially by different rules which represent different
; items and such. Generally speaking it is necessary to do this using as
; simple and independent of rules as possible. Then we can do checks after
; the fact to make sure of things but generally speaking we don't want to
; have too complex of rules. We just match up the different tokens to things
; to see if we have something meaningful and if we do then we simplify it 
; to a form that makes it easy to define rooms. 
