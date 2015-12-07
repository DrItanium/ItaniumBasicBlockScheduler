;------------------------------------------------------------------------------
;Copyright (c) 2012-2015, Joshua Scoggins 
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
(defclass Register
  (is-a USER)
  (multislot queue))

(definstances registers
	      (p0 of Register)
	      (p1 of Register)
	      (p2 of Register)
	      (p3 of Register)
	      (p4 of Register)
	      (p5 of Register)
	      (p6 of Register)
	      (p7 of Register)
	      (p8 of Register)
	      (p9 of Register)
	      (p10 of Register)
	      (p11 of Register)
	      (p12 of Register)
	      (p13 of Register)
	      (p14 of Register)
	      (p15 of Register)
	      (p16 of Register)
	      (p17 of Register)
	      (p18 of Register)
	      (p19 of Register)
	      (p20 of Register)
	      (p21 of Register)
	      (p22 of Register)
	      (p23 of Register)
	      (p24 of Register)
	      (p25 of Register)
	      (p26 of Register)
	      (p27 of Register)
	      (p28 of Register)
	      (p29 of Register)
	      (p30 of Register)
	      (p31 of Register)
	      (p32 of Register)
	      (p33 of Register)
	      (p34 of Register)
	      (p35 of Register)
	      (p36 of Register)
	      (p37 of Register)
	      (p38 of Register)
	      (p39 of Register)
	      (p40 of Register)
	      (p41 of Register)
	      (p42 of Register)
	      (p43 of Register)
	      (p44 of Register)
	      (p45 of Register)
	      (p46 of Register)
	      (p47 of Register)
	      (p48 of Register)
	      (p49 of Register)
	      (p50 of Register)
	      (p51 of Register)
	      (p52 of Register)
	      (p53 of Register)
	      (p54 of Register)
	      (p55 of Register)
	      (p56 of Register)
	      (p57 of Register)
	      (p58 of Register)
	      (p59 of Register)
	      (p60 of Register)
	      (p61 of Register)
	      (p62 of Register)
	      (p63 of Register)
	      (r0 of Register)
	      (f0 of Register)
	      (r1 of Register)
	      (f1 of Register)
	      (r2 of Register)
	      (f2 of Register)
	      (r3 of Register)
	      (f3 of Register)
	      (r4 of Register)
	      (f4 of Register)
	      (r5 of Register)
	      (f5 of Register)
	      (r6 of Register)
	      (f6 of Register)
	      (r7 of Register)
	      (f7 of Register)
	      (r8 of Register)
	      (f8 of Register)
	      (r9 of Register)
	      (f9 of Register)
	      (r10 of Register)
	      (f10 of Register)
	      (r11 of Register)
	      (f11 of Register)
	      (r12 of Register)
	      (f12 of Register)
	      (r13 of Register)
	      (f13 of Register)
	      (r14 of Register)
	      (f14 of Register)
	      (r15 of Register)
	      (f15 of Register)
	      (r16 of Register)
	      (f16 of Register)
	      (r17 of Register)
	      (f17 of Register)
	      (r18 of Register)
	      (f18 of Register)
	      (r19 of Register)
	      (f19 of Register)
	      (r20 of Register)
	      (f20 of Register)
	      (r21 of Register)
	      (f21 of Register)
	      (r22 of Register)
	      (f22 of Register)
	      (r23 of Register)
	      (f23 of Register)
	      (r24 of Register)
	      (f24 of Register)
	      (r25 of Register)
	      (f25 of Register)
	      (r26 of Register)
	      (f26 of Register)
	      (r27 of Register)
	      (f27 of Register)
	      (r28 of Register)
	      (f28 of Register)
	      (r29 of Register)
	      (f29 of Register)
	      (r30 of Register)
	      (f30 of Register)
	      (r31 of Register)
	      (f31 of Register)
	      (r32 of Register)
	      (f32 of Register)
	      (r33 of Register)
	      (f33 of Register)
	      (r34 of Register)
	      (f34 of Register)
	      (r35 of Register)
	      (f35 of Register)
	      (r36 of Register)
	      (f36 of Register)
	      (r37 of Register)
	      (f37 of Register)
	      (r38 of Register)
	      (f38 of Register)
	      (r39 of Register)
	      (f39 of Register)
	      (r40 of Register)
	      (f40 of Register)
	      (r41 of Register)
	      (f41 of Register)
	      (r42 of Register)
	      (f42 of Register)
	      (r43 of Register)
	      (f43 of Register)
	      (r44 of Register)
	      (f44 of Register)
	      (r45 of Register)
	      (f45 of Register)
	      (r46 of Register)
	      (f46 of Register)
	      (r47 of Register)
	      (f47 of Register)
	      (r48 of Register)
	      (f48 of Register)
	      (r49 of Register)
	      (f49 of Register)
	      (r50 of Register)
	      (f50 of Register)
	      (r51 of Register)
	      (f51 of Register)
	      (r52 of Register)
	      (f52 of Register)
	      (r53 of Register)
	      (f53 of Register)
	      (r54 of Register)
	      (f54 of Register)
	      (r55 of Register)
	      (f55 of Register)
	      (r56 of Register)
	      (f56 of Register)
	      (r57 of Register)
	      (f57 of Register)
	      (r58 of Register)
	      (f58 of Register)
	      (r59 of Register)
	      (f59 of Register)
	      (r60 of Register)
	      (f60 of Register)
	      (r61 of Register)
	      (f61 of Register)
	      (r62 of Register)
	      (f62 of Register)
	      (r63 of Register)
	      (f63 of Register)
	      (r64 of Register)
	      (f64 of Register)
	      (r65 of Register)
	      (f65 of Register)
	      (r66 of Register)
	      (f66 of Register)
	      (r67 of Register)
	      (f67 of Register)
	      (r68 of Register)
	      (f68 of Register)
	      (r69 of Register)
	      (f69 of Register)
	      (r70 of Register)
	      (f70 of Register)
	      (r71 of Register)
	      (f71 of Register)
	      (r72 of Register)
	      (f72 of Register)
	      (r73 of Register)
	      (f73 of Register)
	      (r74 of Register)
	      (f74 of Register)
	      (r75 of Register)
	      (f75 of Register)
	      (r76 of Register)
	      (f76 of Register)
	      (r77 of Register)
	      (f77 of Register)
	      (r78 of Register)
	      (f78 of Register)
	      (r79 of Register)
	      (f79 of Register)
	      (r80 of Register)
	      (f80 of Register)
	      (r81 of Register)
	      (f81 of Register)
	      (r82 of Register)
	      (f82 of Register)
	      (r83 of Register)
	      (f83 of Register)
	      (r84 of Register)
	      (f84 of Register)
	      (r85 of Register)
	      (f85 of Register)
	      (r86 of Register)
	      (f86 of Register)
	      (r87 of Register)
	      (f87 of Register)
	      (r88 of Register)
	      (f88 of Register)
	      (r89 of Register)
	      (f89 of Register)
	      (r90 of Register)
	      (f90 of Register)
	      (r91 of Register)
	      (f91 of Register)
	      (r92 of Register)
	      (f92 of Register)
	      (r93 of Register)
	      (f93 of Register)
	      (r94 of Register)
	      (f94 of Register)
	      (r95 of Register)
	      (f95 of Register)
	      (r96 of Register)
	      (f96 of Register)
	      (r97 of Register)
	      (f97 of Register)
	      (r98 of Register)
	      (f98 of Register)
	      (r99 of Register)
	      (f99 of Register)
	      (r100 of Register)
	      (f100 of Register)
	      (r101 of Register)
	      (f101 of Register)
	      (r102 of Register)
	      (f102 of Register)
	      (r103 of Register)
	      (f103 of Register)
	      (r104 of Register)
	      (f104 of Register)
	      (r105 of Register)
	      (f105 of Register)
	      (r106 of Register)
	      (f106 of Register)
	      (r107 of Register)
	      (f107 of Register)
	      (r108 of Register)
	      (f108 of Register)
	      (r109 of Register)
	      (f109 of Register)
	      (r110 of Register)
	      (f110 of Register)
	      (r111 of Register)
	      (f111 of Register)
	      (r112 of Register)
	      (f112 of Register)
	      (r113 of Register)
	      (f113 of Register)
	      (r114 of Register)
	      (f114 of Register)
	      (r115 of Register)
	      (f115 of Register)
	      (r116 of Register)
	      (f116 of Register)
	      (r117 of Register)
	      (f117 of Register)
	      (r118 of Register)
	      (f118 of Register)
	      (r119 of Register)
	      (f119 of Register)
	      (r120 of Register)
	      (f120 of Register)
	      (r121 of Register)
	      (f121 of Register)
	      (r122 of Register)
	      (f122 of Register)
	      (r123 of Register)
	      (f123 of Register)
	      (r124 of Register)
	      (f124 of Register)
	      (r125 of Register)
	      (f125 of Register)
	      (r126 of Register)
	      (f126 of Register)
	      (r127 of Register)
	      (f127 of Register))

(defmessage-handler Register enqueue primary
		    (?target)
		    (bind ?self:queue
			  ?target
			  ?self:queue))
(defclass block
  (is-a USER)
  (multislot contents)
  (slot terminator))
(defclass Instruction 
  (is-a USER)
  (slot Predicate 
	(type SYMBOL))
  (slot Name 
	(type SYMBOL))
  (slot TimeIndex 
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
  (slot print-string)
  (message-handler init after)
  (message-handler inject-consumers primary)
  (message-handler notify-scheduling primary)
  (message-handler increment-producer-count primary)
  (message-handler decrement-producer-count primary))
(defmessage-handler Instruction init after 
		    ()
		    (bind ?self:print-string
			  (format nil "(%s) %s %s %s" 
				  ?self:Predicate 
				  ?self:Name 
				  (implode$ ?self:destination-registers)
				  (if (= (length$ ?self:source-registers) 0) then
				    ""
				    else
				    (implode$ (create$ =
						       ?self:source-registers))))))


(defmessage-handler Instruction increment-producer-count primary 
		    ()
		    (bind ?self:producer-count 
			  (+ ?self:producer-count 1)))

(defmessage-handler Instruction decrement-producer-count primary 
		    ()
		    (bind ?self:producer-count 
			  (- ?self:producer-count 1)))

(defmessage-handler Instruction inject-consumers primary
		    (?list)
		    (progn$ (?a ?list)
			    (send ?a increment-producer-count))
		    (slot-direct-insert$ consumers 1 ?list))

(defmessage-handler Instruction notify-scheduling primary
		    ()
		    (bind ?self:scheduled TRUE)
		    (progn$ (?c ?self:consumers)
			    (send ?c decrement-producer-count)))


(defclass Operation 
  (is-a USER)
  (slot Name   
	(visibility public) 
	(type SYMBOL) 
	(default ?DERIVE))
  (slot Class  
	(visibility public) 
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

;------------------------------------------------------------------------------
; Itanium specific types and classes
;------------------------------------------------------------------------------






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
	     (defop-range X none nop.x))




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
	     (batch* ?B))


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
	     (slot target-register
		   (default ?NONE))
	     (slot parent
		   (default ?NONE)))
