;Copyright (c) 2012, Joshua Scoggins 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass Object (is-a USER)
  (slot parent (visibility public) (type SYMBOL))
  (slot id (visibility public) (type SYMBOL) (access initialize-only))
  (message-handler init around)
  (message-handler as-string primary))

(defmessage-handler Object init around ()
						  (call-next-handler)
						  (bind ?self:id (instance-name-to-symbol (instance-name ?self))))

(defmessage-handler Object as-string primary ()
						  (return (str-cat ?self:id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defclass DependencyChain 
;  (is-a Object)
;  (multislot producers)
;  (multislot consumers)
;  (message-handler as-string primary))
;
;(defmessage-handler DependencyChain as-string primary ()
;						  (format nil "{%s,%s,%s}" ?self:id 
;									 (implode$ ?self:producers) 
;									 (implode$ ?self:consumers)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass Instruction (is-a Object)
  (slot Predicate (type SYMBOL))
  (slot Name (type SYMBOL))
  (slot TimeIndex (type NUMBER))
  (slot ExecutionLength (type NUMBER))
  (multislot destination-registers (type SYMBOL))
  (multislot source-registers (type SYMBOL))
  (multislot producers (type SYMBOL))
  ;(slot DependencyChain (type SYMBOL))
  (slot InstructionType (type SYMBOL) (default-dynamic nil))
  (message-handler as-string primary))
;  (message-handler get-producers)
;  (message-handler get-consumers))

;(defmessage-handler Instruction get-producers ()
; (send (symbol-to-instance-name ?self:DependencyChain)
;       get-producers))
;
;(defmessage-handler Instruction get-consumers ()
; (send (symbol-to-instance-name ?self:DependencyChain)
;       get-consumers))

(defmessage-handler Instruction as-string primary ()
						  (if (= (length$ ?self:source-registers) 0) then
							 (str-cat "(" ?self:Predicate ") " ?self:Name " "
										 (implode$ ?self:destination-registers))
							 else
							 (str-cat "(" ?self:Predicate ") " ?self:Name " "
										 (implode$ ?self:destination-registers) " = " 
										 (implode$ ?self:source-registers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ExecutionObject 
  (is-a Object)
  (slot Name   (visibility public) (type SYMBOL) (default ?DERIVE))
  (slot Class  (visibility public) (type SYMBOL) (default ?DERIVE))
  (slot Length (visibility public) (type NUMBER) (range 0 ?VARIABLE) (default 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Operation (is-a ExecutionObject))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Register (is-a ExecutionObject)
  (multislot OtherNames (type SYMBOL) (default ?DERIVE))
  (message-handler .RegisterEquals primary))


(defmessage-handler Register .RegisterEquals (?sym) 
						  "Checks to see if the register is equal to the given symbol.
						  It also checks to see if the symbol is the memory access version of the given
						  register."
						  (return (and (symbolp ?sym) 
											(or (eq ?sym ?self:Name) 
												 (not (eq (member$ ?sym ?self:OtherNames) 
															 FALSE))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass InstructionGroup 
  (is-a Object)
  (multislot contents)
  (slot TimeIndex (type NUMBER))
  (slot Printed (type SYMBOL) (default-dynamic FALSE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Schedule
          (is-a Object)
			 (multislot collect)
			 (multislot at))
