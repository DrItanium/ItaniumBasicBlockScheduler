;Copyright (c) 2012-2020, Joshua Scoggins 
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
(defclass register
  (is-a USER)
  (multislot stack))
(defgeneric translate-register)
(defmethod translate-register
  (?val)
  ?val)
(defmessage-handler register init after
		    ()
		    (bind ?iname 
			  (instance-name-to-symbol (instance-name ?self)))
		    (build (format nil "(defmethod translate-register
					  ((?in LEXEME 
						(not (neq ?current-argument
							  %s
							  {%s}
							  \"%s\"
							  \"{%s}\"))))
					  [%s])"
				   ?iname
				   ?iname
				   ?iname
				   ?iname
				   ?iname)))


(definstances registers
	      (p0 of register)
	      (p1 of register)
	      (p2 of register)
	      (p3 of register)
	      (p4 of register)
	      (p5 of register)
	      (p6 of register)
	      (p7 of register)
	      (p8 of register)
	      (p9 of register)
	      (p10 of register)
	      (p11 of register)
	      (p12 of register)
	      (p13 of register)
	      (p14 of register)
	      (p15 of register)
	      (p16 of register)
	      (p17 of register)
	      (p18 of register)
	      (p19 of register)
	      (p20 of register)
	      (p21 of register)
	      (p22 of register)
	      (p23 of register)
	      (p24 of register)
	      (p25 of register)
	      (p26 of register)
	      (p27 of register)
	      (p28 of register)
	      (p29 of register)
	      (p30 of register)
	      (p31 of register)
	      (p32 of register)
	      (p33 of register)
	      (p34 of register)
	      (p35 of register)
	      (p36 of register)
	      (p37 of register)
	      (p38 of register)
	      (p39 of register)
	      (p40 of register)
	      (p41 of register)
	      (p42 of register)
	      (p43 of register)
	      (p44 of register)
	      (p45 of register)
	      (p46 of register)
	      (p47 of register)
	      (p48 of register)
	      (p49 of register)
	      (p50 of register)
	      (p51 of register)
	      (p52 of register)
	      (p53 of register)
	      (p54 of register)
	      (p55 of register)
	      (p56 of register)
	      (p57 of register)
	      (p58 of register)
	      (p59 of register)
	      (p60 of register)
	      (p61 of register)
	      (p62 of register)
	      (p63 of register)
	      (r0 of register)
	      (f0 of register)
	      (r1 of register)
	      (f1 of register)
	      (r2 of register)
	      (f2 of register)
	      (r3 of register)
	      (f3 of register)
	      (r4 of register)
	      (f4 of register)
	      (r5 of register)
	      (f5 of register)
	      (r6 of register)
	      (f6 of register)
	      (r7 of register)
	      (f7 of register)
	      (r8 of register)
	      (f8 of register)
	      (r9 of register)
	      (f9 of register)
	      (r10 of register)
	      (f10 of register)
	      (r11 of register)
	      (f11 of register)
	      (r12 of register)
	      (f12 of register)
	      (r13 of register)
	      (f13 of register)
	      (r14 of register)
	      (f14 of register)
	      (r15 of register)
	      (f15 of register)
	      (r16 of register)
	      (f16 of register)
	      (r17 of register)
	      (f17 of register)
	      (r18 of register)
	      (f18 of register)
	      (r19 of register)
	      (f19 of register)
	      (r20 of register)
	      (f20 of register)
	      (r21 of register)
	      (f21 of register)
	      (r22 of register)
	      (f22 of register)
	      (r23 of register)
	      (f23 of register)
	      (r24 of register)
	      (f24 of register)
	      (r25 of register)
	      (f25 of register)
	      (r26 of register)
	      (f26 of register)
	      (r27 of register)
	      (f27 of register)
	      (r28 of register)
	      (f28 of register)
	      (r29 of register)
	      (f29 of register)
	      (r30 of register)
	      (f30 of register)
	      (r31 of register)
	      (f31 of register)
	      (r32 of register)
	      (f32 of register)
	      (r33 of register)
	      (f33 of register)
	      (r34 of register)
	      (f34 of register)
	      (r35 of register)
	      (f35 of register)
	      (r36 of register)
	      (f36 of register)
	      (r37 of register)
	      (f37 of register)
	      (r38 of register)
	      (f38 of register)
	      (r39 of register)
	      (f39 of register)
	      (r40 of register)
	      (f40 of register)
	      (r41 of register)
	      (f41 of register)
	      (r42 of register)
	      (f42 of register)
	      (r43 of register)
	      (f43 of register)
	      (r44 of register)
	      (f44 of register)
	      (r45 of register)
	      (f45 of register)
	      (r46 of register)
	      (f46 of register)
	      (r47 of register)
	      (f47 of register)
	      (r48 of register)
	      (f48 of register)
	      (r49 of register)
	      (f49 of register)
	      (r50 of register)
	      (f50 of register)
	      (r51 of register)
	      (f51 of register)
	      (r52 of register)
	      (f52 of register)
	      (r53 of register)
	      (f53 of register)
	      (r54 of register)
	      (f54 of register)
	      (r55 of register)
	      (f55 of register)
	      (r56 of register)
	      (f56 of register)
	      (r57 of register)
	      (f57 of register)
	      (r58 of register)
	      (f58 of register)
	      (r59 of register)
	      (f59 of register)
	      (r60 of register)
	      (f60 of register)
	      (r61 of register)
	      (f61 of register)
	      (r62 of register)
	      (f62 of register)
	      (r63 of register)
	      (f63 of register)
	      (r64 of register)
	      (f64 of register)
	      (r65 of register)
	      (f65 of register)
	      (r66 of register)
	      (f66 of register)
	      (r67 of register)
	      (f67 of register)
	      (r68 of register)
	      (f68 of register)
	      (r69 of register)
	      (f69 of register)
	      (r70 of register)
	      (f70 of register)
	      (r71 of register)
	      (f71 of register)
	      (r72 of register)
	      (f72 of register)
	      (r73 of register)
	      (f73 of register)
	      (r74 of register)
	      (f74 of register)
	      (r75 of register)
	      (f75 of register)
	      (r76 of register)
	      (f76 of register)
	      (r77 of register)
	      (f77 of register)
	      (r78 of register)
	      (f78 of register)
	      (r79 of register)
	      (f79 of register)
	      (r80 of register)
	      (f80 of register)
	      (r81 of register)
	      (f81 of register)
	      (r82 of register)
	      (f82 of register)
	      (r83 of register)
	      (f83 of register)
	      (r84 of register)
	      (f84 of register)
	      (r85 of register)
	      (f85 of register)
	      (r86 of register)
	      (f86 of register)
	      (r87 of register)
	      (f87 of register)
	      (r88 of register)
	      (f88 of register)
	      (r89 of register)
	      (f89 of register)
	      (r90 of register)
	      (f90 of register)
	      (r91 of register)
	      (f91 of register)
	      (r92 of register)
	      (f92 of register)
	      (r93 of register)
	      (f93 of register)
	      (r94 of register)
	      (f94 of register)
	      (r95 of register)
	      (f95 of register)
	      (r96 of register)
	      (f96 of register)
	      (r97 of register)
	      (f97 of register)
	      (r98 of register)
	      (f98 of register)
	      (r99 of register)
	      (f99 of register)
	      (r100 of register)
	      (f100 of register)
	      (r101 of register)
	      (f101 of register)
	      (r102 of register)
	      (f102 of register)
	      (r103 of register)
	      (f103 of register)
	      (r104 of register)
	      (f104 of register)
	      (r105 of register)
	      (f105 of register)
	      (r106 of register)
	      (f106 of register)
	      (r107 of register)
	      (f107 of register)
	      (r108 of register)
	      (f108 of register)
	      (r109 of register)
	      (f109 of register)
	      (r110 of register)
	      (f110 of register)
	      (r111 of register)
	      (f111 of register)
	      (r112 of register)
	      (f112 of register)
	      (r113 of register)
	      (f113 of register)
	      (r114 of register)
	      (f114 of register)
	      (r115 of register)
	      (f115 of register)
	      (r116 of register)
	      (f116 of register)
	      (r117 of register)
	      (f117 of register)
	      (r118 of register)
	      (f118 of register)
	      (r119 of register)
	      (f119 of register)
	      (r120 of register)
	      (f120 of register)
	      (r121 of register)
	      (f121 of register)
	      (r122 of register)
	      (f122 of register)
	      (r123 of register)
	      (f123 of register)
	      (r124 of register)
	      (f124 of register)
	      (r125 of register)
	      (f125 of register)
	      (r126 of register)
	      (f126 of register)
	      (r127 of register)
	      (f127 of register))

(defmessage-handler register push primary
		    (?target)
		    (slot-direct-insert$ stack 1 ?target))
(defmessage-handler register top primary
		    ()
		    (nth$ 1 ?self:stack))
(defmessage-handler register pop primary
		    ()
		    (if (> (length$ ?self:stack) 0) then
		      (bind ?ret 
			    (nth$ 1 ?self:stack))
		      (slot-direct-delete$ stack 1 1)
		      ?ret))

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
  (multislot destination-queues)
  (multislot source-queues)
  (slot predicate-queue)
  (slot producer-count 
	(type INTEGER))
  (multislot consumers 
	     (type SYMBOL))
  (slot print-string)
  (multislot ok)
  (message-handler init after)
  (message-handler ready-to-schedule primary)
  (message-handler notify-scheduling primary))
(defmessage-handler Instruction ready-to-schedule primary
		    ()
		    (progn$ (?a ?self:ok)
			    (if (neq (send ?a top)
				     (instance-name ?self)) then
			      (return FALSE)))
		    TRUE)

(defmessage-handler Instruction init after 
		    ()
		    (bind ?self:ok 
			  ?self:destination-queues
			  ?self:source-queues
			  (if (neq ?self:predicate-queue
				   [p0]) then
			    ?self:predicate-queue
			    else
			    (create$)))
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



(defmessage-handler Instruction notify-scheduling primary
		    ()
		    (bind ?self:scheduled TRUE)
		    (printout stdout ?self:print-string crlf)
		    (progn$ (?c ?self:ok)
			    (send ?c pop)))

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
(defmethod apply$
  ((?func SYMBOL)
   (?args MULTIFIELD))
  (bind ?output
	(create$))
  (progn$ (?a ?args)
	  (bind ?output
		?output
		(funcall ?func
			 ?a))))
(defmethod apply$
  ((?func SYMBOL)
   $?args)
  (apply$ ?func
	  ?args))
(defmethod filter$
  ((?func SYMBOL)
   (?args MULTIFIELD))
  (bind ?output
	(create$))
  (progn$ (?a ?args)
	  (bind ?output
		?output
		(if (funcall ?func ?a) then
		  ?a
		  else
		  (create$)))))
(defmethod filter$
  ((?func SYMBOL)
   $?args)
  (filter$ ?func
	   ?args))

(deffunction instance-and-not-p0
	     (?a)
	     (and (instance-namep ?a)
		  (neq ?a [p0])))
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
		 (source-registers ?source-registers)
		 (predicate-queue (translate-register ?predicate))
		 (destination-queues (filter$ instance-and-not-p0
					      (apply$ translate-register
						      ?destination-registers)))
		 (source-queues (filter$ instance-and-not-p0
					 (apply$ translate-register
						 ?source-registers)))))

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
;------------------------------------------------------------------------------
; stage rules
;------------------------------------------------------------------------------
(deffacts startup
	  (stage (current Imbue)
		 (rest Analysis-Entry
		       Analysis
		       Schedule
		       Schedule-Update)))

(defrule end-stage-generation
	 (declare (salience -10000))
	 ?f <- (stage (rest))
	 =>
	 (retract ?f))

(defrule next-stage
	 (declare (salience -10000))
	 ?f <- (stage (rest ?now $?rest))
	 =>
	 (modify ?f (current ?now) 
		 (rest $?rest)))
;------------------------------------------------------------------------------
; dependency analysis rules
;------------------------------------------------------------------------------
(defrule imbue-op 
	 "Imbue's the operational type into the given instruction"
	 (declare (salience 1))
	 (stage (current Imbue))
	 (object (is-a Instruction) 
		 (Name ?oName) 
		 (name ?gid))
	 (object (is-a Operation) 
		 (Name ?oName) 
		 (Class ?Class))
	 =>
	 (modify-instance ?gid 
			  (InstructionType ?Class)))

(defrule prime-first-instruction
	 (stage (current Analysis-Entry))
	 =>
	 (assert (Next (- (time-length) 1))))
(defrule add-to-queues
	 (stage (current Analysis))
	 (Instruction ?g0)
	 (object (is-a Instruction)
		 (name ?g0)
		 (ok $? ?j $?))
	 =>
	 (send ?j push ?g0))
; This is a generic scheduler and doesn't take special cases into account
(defrule start-analysis-restart-process
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f <- (Instruction ?g0)
	 (object (is-a Instruction)
		 (name ?g0)
		 (TimeIndex ?ti))
	 =>
	 (retract ?f)
	 ; commit the dependencies we have found
	 (assert (Next (- ?ti 1))))

(defrule try-restart-analysis-process
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f2 <- (Next ?i)
	 (object (is-a Instruction)
		 (TimeIndex ?i)
		 (InstructionType ~B)
		 (name ?name))
	 =>
	 (retract ?f2)
	 (assert (Instruction ?name)))

(defrule try-restart-analysis-process:branch
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f2 <- (Next ?i)
	 (object (is-a Instruction)
		 (TimeIndex ?i)
		 (InstructionType B))
	 =>
	 (retract ?f2)
	 (assert (Next (- ?i 1))))

(defrule finish-analysis-process  
	 (declare (salience -3))
	 (stage (current Analysis))
	 ?f2 <- (Next ?)
	 =>
	 (retract ?f2))

;------------------------------------------------------------------------------
; Scheduling rules
;------------------------------------------------------------------------------
; The use of the producer-count slot instead of producers is because we don't
; actually care what the producer was, only if we have no producers remaining
; This allows us to just decrement the value instead of doing expensive pattern
; matching
;------------------------------------------------------------------------------
(defrule determine-scheduability
	 "An object is able to be scheduled if it has no remaining producers"
	 (stage (current Schedule))
	 (object (is-a register)
		 (stack ?q $?))
	 (test (send ?q ready-to-schedule))
	 =>
	 (assert (schedule (instance-address ?q))))



(defrule update-producer-set
	 (stage (current Schedule-Update))
	 ?f <- (schedule ?q)
	 =>
	 (retract ?f)
	 (send ?q notify-scheduling)
	 (assert (Restart Scheduling)))

(defrule restart-scheduling
	 (declare (salience -2))
	 ?f <- (Restart Scheduling)
	 ?stg <- (stage (current Schedule-Update) 
			(rest $?rest))
	 =>
	 (printout stdout ";;" crlf)
	 (retract ?f)
	 (modify ?stg 
		 (current Schedule) 
		 (rest Schedule-Update $?rest)))
(defrule insert-branch
	 (declare (salience -3))
	 (stage (current Schedule-Update))
	 (object (is-a Instruction)
		 (InstructionType B)
		 (name ?branch))
	 =>
	 (send ?branch notify-scheduling)
	 (printout stdout ";;" crlf))


(printout stdout
	  "Welcome to the Code Scheduler!" crlf
	  "To start scheduling type in (block \"/path/to/file\")" crlf
	  "Then type in (analyze)" crlf
	  "When it is finished the result will be printed out." crlf
	  "Parallel sections of code are separated by ;;." crlf
	  "The groups of instructions separated by ;; are known as Instruction Groups" crlf)

;(block "examples/BlockLarge.clp")
(deffunction start (?a)
	     (watch statistics)
	     (profile-reset)
	     (profile ?a)
	     (run)
	     (profile off)
	     (profile-info))

