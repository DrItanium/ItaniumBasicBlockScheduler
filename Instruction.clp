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


(deffunction mk-instruction
 (?TimeIndex ?Predicate ?Name 
  ?DestinationRegisters ?SourceRegisters)
 (bind ?inst (make-instance (gensym*) of Instruction 
				  (Predicate ?Predicate)
				  (Name ?Name)
				  (TimeIndex ?TimeIndex)))
 (send ?inst .AddSourceRegisters ?SourceRegisters)
 (send ?inst .AddDestinationRegisters ?DestinationRegisters)
 (return ?inst))

(deffunction simple-mk-instruction "Creates a new instruction with the guid and time index automatically set"
 (?Predicate ?Op ?DRegisters ?SRegisters)
 (mk-instruction (new-time-index) ?Predicate ?Op ?DRegisters ?SRegisters))

(deffunction mk-predicate-instruction "Creates a Predicate Instruction with a custom predicate"
 (?Predicate ?Op ?D0 ?D1 ?S0 ?S1)
 (bind ?DR (create$ ?D0 ?D1))
 (bind ?SR (create$ ?S0 ?S1))
 (mk-instruction (new-time-index) ?Predicate ?Op ?DR ?SR))

(deffunction mk-binary-instruction "Creates a standard binary instruction"
 (?Predicate ?Op ?Destination ?S0 ?S1)
 (bind ?SR (create$ ?S0 ?S1))
 (mk-instruction (new-time-index) ?Predicate ?Op (create$ ?Destination) ?SR))

(deffunction mk-unary-instruction "Creates a standard unary instruction"
 (?Predicate ?Op ?Destination ?Source)
 (mk-instruction (new-time-index) ?Predicate ?Op 
  (create$ ?Destination) (create$ ?Source)))

(deffunction mk-noary-instruction "Creates a standard noary instruction"
 (?Predicate ?Op ?Source) (mk-instruction (new-time-index) ?Predicate ?Op
   ?Source (create$)))

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

