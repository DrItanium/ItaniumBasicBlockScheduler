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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate Dependency 
				 "Represents a Data Dependency between two instructions"
				 ;(slot firstInstructionID (type INSTANCE))
				 (slot secondInstructionID (type INSTANCE)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass Object (is-a USER)
  (message-handler as-string primary))

(defmessage-handler Object as-string primary ()
						  (str-cat (instance-name-to-symbol (instance-name ?self))))

(defclass Instruction (is-a Object)
  (slot Predicate (type SYMBOL))
  (slot Name (type SYMBOL))
  (slot TimeIndex (type NUMBER))
  (slot ExecutionLength (type NUMBER))
  (slot scheduled (type SYMBOL) (allowed-symbols FALSE TRUE))
  (slot InstructionType (type SYMBOL) (default-dynamic nil))
  (multislot destination-registers (type SYMBOL))
  (multislot source-registers (type SYMBOL))
  (slot producer-count (type INTEGER))
  (multislot consumers (type SYMBOL))
  (message-handler increment-producer-count primary)
  (message-handler decrement-producer-count primary)
  (message-handler as-string primary))

(defmessage-handler Instruction increment-producer-count primary ()
						  (bind ?self:producer-count (+ ?self:producer-count 1)))

(defmessage-handler Instruction decrement-producer-count primary ()
              (bind ?self:producer-count (- ?self:producer-count 1)))

(defmessage-handler Instruction as-string primary ()
						  (format nil "(%s) %s %s %s" 
									 ?self:Predicate 
									 ?self:Name 
									 (implode$ ?self:destination-registers)
									 (if (= (length$ ?self:source-registers) 0) then
										""
										else
										(implode$ (create$ =
																 ?self:source-registers)))))
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
						  (or (eq ?sym ?self:Name) 
								(member$ ?sym ?self:OtherNames)))


