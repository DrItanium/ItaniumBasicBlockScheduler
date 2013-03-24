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
;; Instruction.clp - Contains Rules and Functions pertaining to Instructions
;; Written by Joshua Scoggins


(defmethod make-instruction
  ((?time-index INTEGER)
	(?predicate SYMBOL)
	(?name SYMBOL STRING)
	(?destination-registers MULTIFIELD)
	(?source-registers MULTIFIELD))
  (make-instance of Instruction
					  (Predicate ?predicate)
					  (TimeIndex ?time-index)
					  (Name ?name)
					  (DestinationRegisters ?destination-registers)
					  (SourceRegisters ?source-registers)))
(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination-registers MULTIFIELD)
	(?source-registers MULTIFIELD))
  (make-instruction (new-time-index)
						  ?predicate ?operation ?destination-registers
						  ?source-registers))
(defmethod simple-make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination-registers MULTIFIELD)
	(?source-registers MULTIFIELD))
  (make-instruction ?predicate ?operation ?destination-registers
						  ?source-registers))

(defmethod make-instruction 
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?d0 SYMBOL STRING INSTANCE)
	(?d1 SYMBOL STRING INSTANCE)
	(?s0 SYMBOL STRING INSTANCE)
	(?s1 SYMBOL STRING INSTANCE))
  (make-instruction
	 ?predicate
	 ?operation
	 (create$ ?d0 ?d1)
	 (create$ ?s0 ?s1)))

(defmethod make-predicate-instruction 
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?d0 SYMBOL STRING INSTANCE)
	(?d1 SYMBOL STRING INSTANCE)
	(?s0 SYMBOL STRING INSTANCE)
	(?s1 SYMBOL STRING INSTANCE))
  (make-instruction
	 ?predicate
	 ?operation
	 ?d0 ?d1
	 ?s0 ?s1))

(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination SYMBOL STRING)
	(?s0 SYMBOL STRING INSTANCE)
	(?s1 SYMBOL STRING INSTANCE))
  (make-instruction ?predicate ?operation 
						  (create$ ?destination) 
						  (create$ ?s0 ?s1)))

(defmethod make-binary-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination SYMBOL STRING)
	(?s0 SYMBOL STRING INSTANCE)
	(?s1 SYMBOL STRING INSTANCE))
  (make-instruction ?predicate ?operation 
						  ?destination
						  ?s0 ?s1))

(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination SYMBOL STRING INSTANCE)
	(?source SYMBOL STRING INSTANCE))
  (make-instruction 
	 ?predicate 
	 ?operation
	 (create$ ?destination)
	 (create$ ?source)))

(defmethod make-instruction 
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination MULTIFIELD))
  (make-instance 
	 ?predicate
	 ?op
	 ?destination
	 (create$)))


(deffunction defop "Defines an operation" (?Op ?Type ?Length)
				 (return (make-instance (gensym*) of Operation (Name ?Op) 
												(Class ?Type) (Length ?Length))))

(deffunction defregister "Defines a register for a given class" (?Name ?Class ?Size)
				 (return (make-instance (gensym*) of Register (Name ?Name)
												(Class ?Class) (Length ?Size))))

(deffunction defregister-range "Defines a range of registers of a given type"
				 (?Prefix ?Start ?Stop ?Class ?Size)
				 (bind ?i (+ 1 ?Start))
				 (bind ?total (sym-cat ?Prefix ?Start))
				 (defregister ?total ?Class ?Size)
				 (while (< ?i ?Stop) do
						  (bind ?Name (sym-cat ?Prefix ?i))
						  (defregister ?Name ?Class ?Size)
						  (bind ?i (1+ ?i))))

(deffunction defop-range 
				 (?Type ?Length $?Ops)
				 (bind ?Count (length ?Ops))
				 (bind ?i 1)
				 (while (<= ?i ?Count) do
						  (defop (nth ?i $?Ops) ?Type ?Length)
						  (bind ?i (+ 1 ?i))))

(deffunction registers "Defines a list of registers of a given type"
				 (?Class ?Size $?Registers)
				 (bind ?Count (length ?Registers))
				 (bind ?Total (nth 1 $?Registers))
				 (defregister ?Total ?Class ?Size)
				 (bind ?i 2)
				 (while (<= ?i ?Count) do
						  (bind ?Total (nth ?i $?Registers))
						  (defregister ?Total ?Class ?Size)
						  (bind ?i (1+ ?i))))

