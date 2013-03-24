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

(defgeneric make-instruction)

(defmethod make-instruction
  ((?time-index INTEGER)
	(?predicate SYMBOL)
	(?name SYMBOL STRING)
	(?destination-registers MULTIFIELD)
	(?source-registers MULTIFIELD))
  (bind ?n0 (gensym*))
  (bind ?n1 (gensym*))
  (make-instance ?n0 of Instruction
					  (Predicate ?predicate)
					  (TimeIndex ?time-index)
					  (Name ?name)
					  (DependencyChain ?n1)
					  (destination-registers ?destination-registers)
					  (source-registers ?source-registers))
  (make-instance ?n1 of DependencyChain
	              (parent ?n0)))

(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination-registers MULTIFIELD)
	(?source-registers MULTIFIELD))
  (make-instruction (new-time-index)
						  ?predicate 
						  ?operation 
						  ?destination-registers
						  ?source-registers))
(defmethod make-instruction 
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?d0 NUMBER SYMBOL STRING INSTANCE)
	(?d1 NUMBER SYMBOL STRING INSTANCE)
	(?s0 NUMBER SYMBOL STRING INSTANCE)
	(?s1 NUMBER SYMBOL STRING INSTANCE))
  (make-instruction ?predicate
						  ?operation
						  (create$ ?d0 ?d1)
						  (create$ ?s0 ?s1)))


(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination SYMBOL STRING)
	(?s0 NUMBER SYMBOL STRING INSTANCE)
	(?s1 NUMBER SYMBOL STRING INSTANCE))
  (make-instruction ?predicate 
						  ?operation 
						  (create$ ?destination) 
						  (create$ ?s0 ?s1)))

(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination NUMBER SYMBOL STRING INSTANCE)
	(?source NUMBER SYMBOL STRING INSTANCE))
  (make-instruction ?predicate 
						  ?operation
						  (create$ ?destination)
						  (create$ ?source)))

(defmethod make-instruction 
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination MULTIFIELD))
  (make-instruction ?predicate
						  ?operation
						  ?destination
						  (create$)))
(defmethod make-instruction
  ((?predicate SYMBOL)
	(?operation SYMBOL)
	(?destination SYMBOL STRING NUMBER))
  (make-instruction ?predicate
	                 ?operation
						  (create$ ?destination)))


(defmethod defop 
  "Defines an operation"
  ((?Op SYMBOL STRING)
	(?Type SYMBOL STRING)
	(?Length INTEGER (> ?Length 0)))
  (make-instance of Operation
					  (Name ?Op)
					  (Class ?Type)
					  (Length ?Length)))

(defmethod defregister 
  "Defines a register for a given class"
  ((?Name SYMBOL STRING)
	(?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0)))
  (make-instance of Register
					  (Name ?Name)
					  (Class ?Class)
					  (Length ?Size)))
(defmethod defregister-range
  ((?Prefix SYMBOL STRING)
	(?Start INTEGER (>= ?Start 0))
	(?Stop INTEGER (and (>= ?Stop ?Start)
							  (>= ?Stop 0)))
	(?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0)))
  (bind ?i ?Start)
  (while (< ?i ?Stop) do
			(defregister (sym-cat ?Prefix ?i)
							 ?Class
							 ?Size)
			(bind ?i (+ ?i 1))))

(defmethod defop-range
  ((?Type SYMBOL STRING)
	(?Length INTEGER)
	(?Ops MULTIFIELD SYMBOL STRING))
  (progn$ (?op ?Ops)
			 (defop ?op ?Type ?Length)))

(defmethod defop-range
  ((?Type SYMBOL STRING)
	(?Length INTEGER)
	($?Ops SYMBOL STRING (> (length$ $?Ops) 1)))
  (defop-range ?Type ?Length $?Ops))

(defmethod defop-range
  ((?Type SYMBOL STRING)
	(?Length INTEGER)
	(?Op SYMBOL STRING))
  (defop ?Op ?Type ?Length))

(defmethod registers 
  "Defines a list of registers of a given type"
  ((?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0))
	(?registers MULTIFIELD SYMBOL STRING (> (length$ ?registers) 1)))
  (progn$ (?register $?registers)
			 (defregister ?register ?Class ?Size)))

(defmethod registers
  ((?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0))
	(?registers MULTIFIELD SYMBOL STRING (= (length$ ?registers) 1)))
  (defregister (nth$ 1 ?registers) ?Class ?Size))

(defmethod registers
  ((?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0))
	($?registers SYMBOL STRING (> (length$ ?registers) 1)))
  (registers ?Class ?Size ?registers))

(defmethod registers
  ((?Class SYMBOL STRING)
	(?Size INTEGER (>= ?Size 0))
	(?register SYMBOL STRING))
  (defregister ?register ?Class ?Size))
