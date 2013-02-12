;------------------------------------------------------------------------------
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
;    * Neither the name of Joshua Scoggins nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; GLAPIConversion.clp - an expert system that reads a define and generates
; corresponding CLIPS functions that convert the CLIPS values into
; corresponding opengl types. This gets a little goofy when dealing with
; multifields but the idea is sound. 
;------------------------------------------------------------------------------
; We need to add functionality to diverge asterisks from a symbol
;------------------------------------------------------------------------------
(target-symbol-is-special "*" 
                          identify-symbols-with-asterisks 
                          "Splits asterisks (*) out of symbols if necessary")
;------------------------------------------------------------------------------
; Unlike GLConstantConversion, I don't need to use the heading span objects.
; This is because each file-line has already been correctly merged.
;------------------------------------------------------------------------------
(defrule build-groups::build-grouping
         ?msg <- (message (to build-groups)
                          (action add-to-span)
                          (arguments ?id))
         (object (is-a file-line) 
                 (id ?id)
                 (type GLAPI-DEF))
         =>
         ;we need to set this up to do conversion of the different arguments
         (retract ?msg))
;------------------------------------------------------------------------------
; Alright, we now need to build a corresponding procedure from each heading
; There are several ways to do this. The easiest would be to just do it
; procedurally in a single rule fire. 
;------------------------------------------------------------------------------
(deffunction grouping-update::retrieve-element (?s)
             (nth 1 (send (instance-address * (symbol-to-instance-name ?s))
                          get-contents)))
;------------------------------------------------------------------------------
(deffunction grouping-update::to-conditional-field (?symbol ?if)
             (bind ?str (str-cat (retrieve-element ?symbol)))
             (create$ (format nil "%s(strcmp(input, \"%s\") == 0) {" 
                              (if ?if then "if" else "} else if")
                              (sub-string (+ (str-index "_" ?str) 1) 
                                          (str-length ?str) ?str))
                      (format nil "return %s" ?str)))
;------------------------------------------------------------------------------
(defrule grouping-update::build-constant-conversion-procedure
         ?obj <- (object (is-a heading-span)
                         (header-name ?group)
                         (contents $?entries))
         (test (> (length$ $?entries) 0))
         =>
         (unmake-instance ?obj)
         (bind ?target (format nil "//%s" ?group))
         (bind ?header (format nil "extern GLenum To%s(char* input) {" ?group))
         (bind ?first (to-conditional-field (nth$ 1 (first$ ?entries)) TRUE))
         (bind ?result (create$ ?target ?header ?first))
         (progn$ (?e (rest$ ?entries))
                 (bind ?result 
                       (create$ ?result (to-conditional-field ?e FALSE))))
         (bind ?result (create$ ?result "} else {" "return 0;" "}" "}"))
         (progn$ (?r ?result) (printout t ?r crlf))
         (printout t crlf crlf))
;------------------------------------------------------------------------------
(defrule grouping-update::skip-constant-conversion
         ?obj <- (object (is-a heading-span)
                         (header-name ?group)
                         (contents $?entries))
         (test (= (length$ $?entries) 0))
         =>
         (unmake-instance ?obj))
;------------------------------------------------------------------------------
