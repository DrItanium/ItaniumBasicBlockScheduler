;------------------------------------------------------------------------------
;Copyright (c) 2012-2014, Joshua Scoggins 
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
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; Globals
;------------------------------------------------------------------------------
(defglobal MAIN 
		   ; Temporary storage for dependency resolution
		   ?*TemporaryList* = (create$)
		   ; TIME Index generator
		   ?*TIME* = 0)
;------------------------------------------------------------------------------
; Classes
;------------------------------------------------------------------------------

(defclass Instruction 
  (is-a USER)
  (slot Predicate 
		(type SYMBOL))
  (slot Name 
		(type SYMBOL))
  (slot TimeIndex 
		(type NUMBER))
  (slot ExecutionLength 
		(type NUMBER))
  (slot scheduled 
		(type SYMBOL) 
		(allowed-symbols FALSE TRUE))
  (slot InstructionType 
		(type SYMBOL) 
		(default-dynamic nil))
  (multislot destination-registers 
			 (type SYMBOL))
  (multislot source-registers 
			 (type SYMBOL))
  (slot producer-count 
		(type INTEGER))
  (multislot consumers 
			 (type SYMBOL))
  (message-handler inject-consumers primary)
  (message-handler notify-scheduling primary)
  (message-handler mark-scheduled primary)
  (message-handler increment-producer-count primary)
  (message-handler decrement-producer-count primary))

(defmessage-handler Instruction increment-producer-count primary 
					()
					(bind ?self:producer-count (+ ?self:producer-count 1)))

(defmessage-handler Instruction decrement-producer-count primary 
					()
					(bind ?self:producer-count (- ?self:producer-count 1)))

(defmessage-handler Instruction inject-consumers 
					(?list)
					(progn$ (?a ?list)
							(send ?a increment-producer-count))
					(slot-direct-insert$ consumers 1 ?list))

(defmessage-handler Instruction notify-scheduling primary
					()
					(progn$ (?c ?self:consumers)
							(send ?c decrement-producer-count)))

(defmessage-handler Instruction mark-scheduled primary
					()
					(format t "(%s) %s %s %s%n" 
							?self:Predicate 
							?self:Name 
							(implode$ ?self:destination-registers)
							(if (= (length$ ?self:source-registers) 0) then
							  ""
							  else
							  (implode$ (create$ =
												 ?self:source-registers))))
					(bind ?self:scheduled TRUE))


(defclass ExecutionObject 
  (is-a USER)
  (slot Name   
		(visibility public) 
		(type SYMBOL) 
		(default ?DERIVE))
  (slot Class  
		(visibility public) 
		(type SYMBOL) 
		(default ?DERIVE)))


(defclass Operation 
  (is-a ExecutionObject))


(defclass Register 
  (is-a ExecutionObject)
  (slot Size
		(visibility public)
		(type NUMBER)
		(range 0 ?VARIABLE)
		(default 64))
  (multislot OtherNames 
			 (type SYMBOL) 
			 (default ?DERIVE)))


;------------------------------------------------------------------------------
; TimeIndex - Global Time Index Tracker
;------------------------------------------------------------------------------

(deffunction time-length 
			 "Gets the time count of the program" 
			 ()
			 (return ?*TIME*))

(deffunction new-time-index 
			 "Returns the current time index and increments the time by one" 
			 ()
			 (bind ?result ?*TIME*)
			 (bind ?*TIME* (+ ?*TIME* 1))
			 (return ?result))

(deffunction reset-time-index 
			 () 
			 (bind ?*TIME* 0))

;------------------------------------------------------------------------------
; Instruction construction functions and methods
;------------------------------------------------------------------------------

(defgeneric make-instruction)

(defmethod make-instruction
  ((?time-index INTEGER)
   (?predicate SYMBOL)
   (?name LEXEME)
   (?destination-registers MULTIFIELD)
   (?source-registers MULTIFIELD))
  (make-instance of Instruction
				 (Predicate ?predicate)
				 (TimeIndex ?time-index)
				 (Name ?name)
				 (destination-registers ?destination-registers)
				 (source-registers ?source-registers)))

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
   (?d0 NUMBER LEXEME INSTANCE)
   (?d1 NUMBER LEXEME INSTANCE)
   (?s0 NUMBER LEXEME INSTANCE)
   (?s1 NUMBER LEXEME INSTANCE))
  (make-instruction ?predicate
					?operation
					(create$ ?d0 ?d1)
					(create$ ?s0 ?s1)))


(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?destination LEXEME)
   (?s0 NUMBER LEXEME INSTANCE)
   (?s1 NUMBER LEXEME INSTANCE))
  (make-instruction ?predicate 
					?operation 
					(create$ ?destination) 
					(create$ ?s0 ?s1)))

(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?destination NUMBER LEXEME INSTANCE)
   (?source NUMBER LEXEME INSTANCE))
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
   (?destination LEXEME NUMBER))
  (make-instruction ?predicate
					?operation
					(create$ ?destination)))

(defmethod defop 
  "Defines an operation"
  ((?Op LEXEME)
   (?Type LEXEME))
  (make-instance of Operation
				 (Name ?Op)
				 (Class ?Type)))

(defmethod defregister 
  "Defines a register for a given class"
  ((?Name LEXEME)
   (?Class LEXEME)
   (?Size INTEGER (>= ?Size 0)))
  (make-instance of Register
				 (Name ?Name)
				 (Class ?Class)
				 (Size ?Size)))
(defmethod defregister-range
  ((?Prefix LEXEME)
   (?Start INTEGER (>= ?Start 0))
   (?Stop INTEGER (and (>= ?Stop ?Start)
					   (>= ?Stop 0)))
   (?Class LEXEME)
   (?Size INTEGER (>= ?Size 0)))
  (bind ?i ?Start)
  (while (< ?i ?Stop) do
		 (defregister (sym-cat ?Prefix ?i)
					  ?Class
					  ?Size)
		 (bind ?i (+ ?i 1))))

(defmethod defop-range
  ((?Type LEXEME)
   (?Ops MULTIFIELD LEXEME))
  (progn$ (?op ?Ops)
		  (defop ?op ?Type)))

(defmethod defop-range
  ((?Type LEXEME)
   ($?Ops LEXEME (> (length$ $?Ops) 1)))
  (defop-range ?Type $?Ops))

(defmethod defop-range
  ((?Type LEXEME)
   (?Op LEXEME))
  (defop ?Op ?Type))

(defmethod registers 
  "Defines a list of registers of a given type"
  ((?Class LEXEME)
   (?Size INTEGER (>= ?Size 0))
   (?registers MULTIFIELD LEXEME (> (length$ ?registers) 1)))
  (progn$ (?register $?registers)
		  (defregister ?register ?Class ?Size)))

(defmethod registers
  ((?Class LEXEME)
   (?Size INTEGER (>= ?Size 0))
   (?registers MULTIFIELD LEXEME (= (length$ ?registers) 1)))
  (defregister (nth$ 1 ?registers) ?Class ?Size))

(defmethod registers
  ((?Class LEXEME)
   (?Size INTEGER (>= ?Size 0))
   ($?registers LEXEME (> (length$ ?registers) 1)))
  (registers ?Class ?Size ?registers))

(defmethod registers
  ((?Class LEXEME)
   (?Size INTEGER (>= ?Size 0))
   (?register LEXEME))
  (defregister ?register ?Class ?Size))

;------------------------------------------------------------------------------
; Itanium specific types and classes
;------------------------------------------------------------------------------

(deffunction special-registers 
			 "Defines a list of special registers"
			 (?Size $?Registers)
			 (registers Special ?Size $?Registers))

(deffunction application-registers 
			 "Defines a list of application registers"
			 (?Size $?Registers)
			 (registers Application ?Size $?Registers))

(deffunction float-registers 
			 "Defines a list of floating point registers"
			 (?Size $?Registers)
			 (registers Float ?Size $?Registers))


(deffunction predicate-registers 
			 "Defines a list of floating point registers"
			 (?Size $?Registers)
			 (registers Predicate ?Size $?Registers))

(deffunction gpr-registers 
			 "Defines a list of floating point registers"
			 (?Size $?Registers)
			 (registers GPR ?Size $?Registers))

(deffunction br 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br ?Target))

(deffunction br.cond 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.cond ?Target))

(deffunction br.few 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.few ?Target))

(deffunction br.many 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.many ?Target))

(deffunction br.cond.dptk.few 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.cond.dptk.few ?Target))

(deffunction br.cond.dptk.many 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.cond.dptk.many ?Target))

(deffunction br.cond.sptk.few 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.cond.sptk.few ?Target))

(deffunction br.cond.sptk.many 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.cond.sptk.many ?Target))

(deffunction br.ret.dptk.few 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.ret.dptk.few ?Target))

(deffunction br.ret.dptk.many 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.ret.dptk.many ?Target))


(deffunction br.ret.sptk.few 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.ret.sptk.few ?Target))

(deffunction br.ret.sptk.many 
			 (?Predicate ?Target)
			 (make-instruction ?Predicate br.ret.sptk.many ?Target))

(deffunction br.call.dptk.few 
			 (?Predicate ?Target ?Source)
			 (make-instruction ?Predicate br.call.dptk.few ?Target ?Source))

(deffunction br.call.dptk.many 
			 (?Predicate ?Target ?Source)
			 (make-instruction ?Predicate br.call.dptk.many ?Target ?Source))

(deffunction br.call.sptk.few 
			 (?Predicate ?Target ?Source)
			 (make-instruction ?Predicate br.call.sptk.few ?Target ?Source))

(deffunction br.call.sptk.many 
			 (?Predicate ?Target ?Source)
			 (make-instruction ?Predicate br.call.sptk.many ?Target ?Source))

(deffunction adds 
			 (?Pred ?Dest ?Source0 ?Source1)
			 (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction addl 
			 (?Pred ?Dest ?Source0 ?Source1)
			 (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction add 
			 (?Pred ?Dest ?Source0 ?Source1)
			 (make-instruction ?Pred add ?Dest ?Source0 ?Source1))

(deffunction sub 
			 (?P ?D ?S0 ?S1)
			 (make-instruction ?P sub ?D ?S0 ?S1))

(deffunction and-inst 
			 (?Pred ?Dest ?Source0 ?Source1)
			 (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction xmpy.l 
			 (?Pred ?Dest ?Source0 ?Source1)
			 (make-instruction ?Pred xmpy.l ?Dest ?Source0 ?Source1))

(deffunction mov 
			 (?Pred ?Dest ?Source0)
			 (make-instruction ?Pred mov ?Dest ?Source0))

(deffunction mov.i 
			 (?Pred ?Dest ?Source0)
			 (make-instruction ?Pred mov.i ?Dest ?Source0))

(deffunction sxt4 
			 (?Pred ?Dest ?Source0)
			 (make-instruction ?Pred sxt4 ?Dest ?Source0))

(deffunction stf.spill 
			 (?Pred ?Dest ?Source0)
			 (make-instruction ?Pred stf.spill ?Dest ?Source0))

(deffunction ldfs 
			 (?P ?D ?S)
			 (make-instruction ?P ldfs ?D ?S))

(deffunction nop.b 
			 (?P)
			 (make-instruction ?P nop.b 0x0))

(deffunction nop.i 
			 (?P)
			 (make-instruction ?P nop.i 0x0))

(deffunction nop.f 
			 (?P)
			 (make-instruction ?P nop.f 0x0))

(deffunction nop.m 
			 (?P)
			 (make-instruction ?P nop.m 0x0))

(deffunction nop.x 
			 (?P)
			 (make-instruction ?P nop.x 0x0))

(deffunction cmp.eq 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp.eq ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.lt 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.gt 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp.gt ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.eq 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp4.eq ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.lt 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp4.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp4.gt 
			 (?Predicate ?D0 ?D1 ?S0 ?S1)
			 (make-instruction ?Predicate cmp4.gt ?D0 ?D1 ?S0 ?S1))

(deffunction setf.sig 
			 (?P ?D ?S)
			 (make-instruction ?P setf.sig ?D ?S))

(deffunction getf.sig 
			 (?P ?D ?S)
			 (make-instruction ?P getf.sig ?D ?S))

(deffunction ld8 
			 (?P ?D ?S)
			 (make-instruction ?P ld8 ?D ?S))

(deffunction ld4 
			 (?P ?D ?S)
			 (make-instruction ?P ld4 ?D ?S))

(deffunction st8 
			 (?P ?D ?S)
			 (make-instruction ?P st8 ?D ?S))

(deffunction st4 
			 (?P ?D ?S)
			 (make-instruction ?P st4 ?D ?S))

(deffunction alloc 
			 (?P ?D ?I ?L ?O ?R)
			 (make-instruction ?P alloc ?D (create$ ar.pfs ?I ?L ?O ?R)))

(deffunction init-ia64-machine 
			 ()
			 (defop-range A and or nop.a add)

			 (defop-range I mul sub cmp cmp.eq cmp.gt cmp.lt cmp.neq div shl 
						  shr sxt4 adds nop.i)

			 (defop-range M st8 ld8 ld4 st4 mov stf.spill nop.m alloc)

			 (defop-range B br br.ret br.call br.few br.many 
						  br.ret.sptk.many br.ret.sptk.few br.ret.dptk.many 
						  br.ret.dptk.few br.call.sptk.many br.call.sptk.few 
						  br.cond.dptk.few br.call.dptk.many br.call.dptk.few nop.b)
			 (defop-range F ldfs fma nop.f)
			 (defop-range X none nop.x)


			 ;;Define registers
			 (registers Special 64 pr um um.be um.up um.ac um.mfl um.mfh cfm ip
						gp)
			 (registers Application 64 ar.pfs rsc bsp bspstore rnat fcr eflag 
						csd ssd cflg fsr fir fdr ccv unat fpsr itc pfs lc ec)
			 (defregister-range cpuid 0 5 Special 64)

			 (defregister-range pm 0 8 Special 64) 
			 (defregister-range kr 0 8 Application 64)
			 (defregister-range a 0 128 Application 64)
			 (defregister-range r 0 128 GPR 64)
			 (defregister-range p 0 64 Predicate 1)
			 (defregister-range f 0 128 Float 82)
			 (defregister-range b 0 8 Branch 64))


;------------------------------------------------------------------------------
; REPL Interaction functions
;------------------------------------------------------------------------------
(deffunction reload-environment 
			 () 
			 (reset) 
			 (reset-time-index) 
			 (init-ia64-machine))

(deffunction block 
			 "Loads a new block from a file" 
			 (?B) 
			 (reload-environment)
			 (batch ?B))


(deffunction analyze 
			 "Use this function instead of run becase it asserts a specific rule that has to be initiated so that the objects don't start automatically running!"
			 ()
			 (run))

;------------------------------------------------------------------------------
; Templates
;------------------------------------------------------------------------------
(deftemplate stage 
			 (slot current (type SYMBOL))
			 (multislot rest (type SYMBOL)))

(deftemplate register-ref
			 (slot type
				   (default ?NONE))
			 (slot time-index
				   (type INTEGER)
				   (default ?NONE))
			 (slot target-register
				   (default ?NONE))
			 (slot parent
				   (default ?NONE)))
