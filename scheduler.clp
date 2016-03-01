;Copyright (c) 2012-2016, Joshua Scoggins 
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
;;Import
;------------------------------------------------------------------------------
; Globals
;------------------------------------------------------------------------------
(defglobal MAIN 
           ; TIME Index generator
           ?*output-router* = t)
;------------------------------------------------------------------------------
; generics
;------------------------------------------------------------------------------
(defgeneric translate-register)
(defgeneric translate-operation)
(defgeneric make-instruction)
;------------------------------------------------------------------------------
; Classes
;------------------------------------------------------------------------------
(defclass register
  (is-a USER)
  (multislot queue))


(defmethod translate-operation (?i) UNKNOWN)

(defmethod translate-register (?val) ?val)




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

(defmessage-handler register enqueue primary
                    (?target)
                    (slot-direct-insert$ queue (+ (length$ ?self:queue) 1) ?target))
(defmessage-handler register front primary
                    ()
                    (nth$ 1 ?self:queue))

(defmessage-handler register dequeue primary
                    ()
                    (if (not (empty$ ?self:queue)) then
                      (bind ?ret 
                            (nth$ 1 
                                  ?self:queue))
                      (slot-direct-delete$ queue 1 1)
                      ?ret))

(defclass Instruction 
  (is-a USER)
  (slot print-string
        (type STRING))
  (slot InstructionType 
        (type SYMBOL) 
        (default-dynamic nil))
  (multislot ok)
  (message-handler init after)
  (message-handler ready-to-schedule primary)
  (message-handler notify-scheduling primary))
(defmessage-handler Instruction ready-to-schedule primary
                    ()
                    (progn$ (?a ?self:ok)
                            (if (neq (send ?a front)
                                     ?self) then
                              (return FALSE)))
                    TRUE)
(deffunction instance-and-not-p0
             (?a)
             (and (instancep ?a)
                  (neq (instance-name ?a) [p0])))
(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?dreg MULTIFIELD)
   (?sreg MULTIFIELD))
  (bind ?op 
        (translate-operation ?operation))
  (make-instance of Instruction
                 (print-string (format nil
                                       "(%s) %s %s %s"
                                       ?predicate
                                       ?operation
                                       (implode$ ?dreg)
                                       (if (empty$ ?sreg) then
                                         ""
                                         else
                                         (implode$ (create$ =
                                                            ?sreg)))))
                 (InstructionType ?op)
                 (ok (if (neq ?op B) then
                       (filter instance-and-not-p0
                               (expand$ (map translate-register
                                             (expand$ ?dreg)
                                             (expand$ ?sreg)
                                             ?predicate)))
                       else
                       (create$)))))

(defmessage-handler Instruction init after 
                    ()
                    (progn$ (?a ?self:ok)
                            (send ?a 
                                  enqueue 
                                  ?self)))


(defmessage-handler Instruction notify-scheduling primary
                    ()
                    (progn$ (?c ?self:ok)
                            (send ?c 
                                  dequeue))
                    ?self:print-string)

;------------------------------------------------------------------------------
; Instruction construction functions and methods
;------------------------------------------------------------------------------

(defmethod make-instruction 
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?d0 NUMBER 
        LEXEME 
        INSTANCE)
   (?d1 NUMBER 
        LEXEME 
        INSTANCE)
   (?s0 NUMBER 
        LEXEME 
        INSTANCE)
   (?s1 NUMBER 
        LEXEME 
        INSTANCE))
  (make-instruction ?predicate
                    ?operation
                    (create$ ?d0 ?d1)
                    (create$ ?s0 ?s1)))

(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?destination LEXEME)
   (?s0 NUMBER 
        LEXEME 
        INSTANCE)
   (?s1 NUMBER 
        LEXEME 
        INSTANCE))
  (make-instruction ?predicate
                    ?operation
                    (create$ ?destination)
                    (create$ ?s0 ?s1)))

(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?destination NUMBER 
                 LEXEME 
                 INSTANCE)
   (?source NUMBER 
            LEXEME 
            INSTANCE))
  (make-instruction ?predicate
                    ?operation
                    (create$ ?destination)
                    (create$ ?source)))

(defmethod make-instruction
  ((?predicate SYMBOL)
   (?operation SYMBOL)
   (?destination LEXEME 
                 NUMBER))
  (make-instruction ?predicate
                    ?operation
                    (create$ ?destination)
                    (create$)))


;------------------------------------------------------------------------------
; Itanium specific types and classes
;------------------------------------------------------------------------------
(deffunction br 
             (?pred ?target)
             (make-instruction ?pred br ?target))

(deffunction br.cond 
             (?pred ?target)
             (make-instruction ?pred br.cond ?target))

(deffunction br.few 
             (?pred ?target)
             (make-instruction ?pred br.few ?target))

(deffunction br.many 
             (?pred ?target)
             (make-instruction ?pred br.many ?target))

(deffunction br.cond.dptk.few 
             (?pred ?target)
             (make-instruction ?pred br.cond.dptk.few ?target))

(deffunction br.cond.dptk.many 
             (?pred ?target)
             (make-instruction ?pred br.cond.dptk.many ?target))

(deffunction br.cond.sptk.few 
             (?pred ?target)
             (make-instruction ?pred br.cond.sptk.few ?target))

(deffunction br.cond.sptk.many 
             (?pred ?target)
             (make-instruction ?pred br.cond.sptk.many ?target))

(deffunction br.ret.dptk.few 
             (?pred ?target)
             (make-instruction ?pred br.ret.dptk.few ?target))

(deffunction br.ret.dptk.many 
             (?pred ?target)
             (make-instruction ?pred br.ret.dptk.many ?target))


(deffunction br.ret.sptk.few 
             (?pred ?target)
             (make-instruction ?pred br.ret.sptk.few ?target))

(deffunction br.ret.sptk.many 
             (?pred ?target)
             (make-instruction ?pred br.ret.sptk.many ?target))

(deffunction br.call.dptk.few 
             (?pred ?target ?src)
             (make-instruction ?pred br.call.dptk.few ?target ?src))

(deffunction br.call.dptk.many 
             (?pred ?target ?src)
             (make-instruction ?pred br.call.dptk.many ?target ?src))

(deffunction br.call.sptk.few 
             (?pred ?target ?src)
             (make-instruction ?pred br.call.sptk.few ?target ?src))

(deffunction br.call.sptk.many 
             (?pred ?target ?src)
             (make-instruction ?pred br.call.sptk.many ?target ?src))

(deffunction adds 
             (?pred ?dest ?src0 ?src1)
             (make-instruction ?pred adds ?dest ?src0 ?src1))

(deffunction addl 
             (?pred ?dest ?src0 ?src1)
             (make-instruction ?pred adds ?dest ?src0 ?src1))

(deffunction add 
             (?pred ?dest ?src0 ?src1)
             (make-instruction ?pred add ?dest ?src0 ?src1))

(deffunction sub 
             (?P ?D ?S0 ?S1)
             (make-instruction ?P sub ?D ?S0 ?S1))

(deffunction and-inst 
             (?pred ?dest ?src0 ?src1)
             (make-instruction ?pred adds ?dest ?src0 ?src1))

(deffunction xmpy.l 
             (?pred ?dest ?src0 ?src1)
             (make-instruction ?pred xmpy.l ?dest ?src0 ?src1))

(deffunction mov 
             (?pred ?dest ?src0)
             (make-instruction ?pred mov ?dest ?src0))

(deffunction mov.i 
             (?pred ?dest ?src0)
             (make-instruction ?pred mov.i ?dest ?src0))

(deffunction sxt4 
             (?pred ?dest ?src0)
             (make-instruction ?pred sxt4 ?dest ?src0))

(deffunction stf.spill 
             (?pred ?dest ?src0)
             (make-instruction ?pred stf.spill ?dest ?src0))

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
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp.eq ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.lt 
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.gt 
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp.gt ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.eq 
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp4.eq ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.lt 
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp4.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp4.gt 
             (?pred ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?pred cmp4.gt ?D0 ?D1 ?S0 ?S1))

(deffunction setf.sig 
             (?P ?D ?S)
             (make-instruction ?P setf.sig ?D ?S))

(deffunction getf.sig 
             (?P ?D ?S)
             (make-instruction ?P getf.sig ?D ?S))

(deffunction ld8 
             (?p ?d ?s)
             (make-instruction ?p ld8 ?d ?s))

(deffunction ld4 
             (?P ?D ?S)
             (make-instruction ?P ld4 ?D ?S))

(deffunction st8 
             (?P ?D ?S)
             (make-instruction ?P st8 ?D ?S))

(deffunction st4 
             (?P ?D ?S)
             (make-instruction ?P st8 ?D ?S))

(deffunction alloc 
             (?pred ?dest ?i ?l ?o ?r)
             (make-instruction ?pred alloc (create$ ?dest) (create$ ar.pfs ?i ?l ?o ?r)))





;------------------------------------------------------------------------------
; REPL Interaction functions
;------------------------------------------------------------------------------
(deffunction block 
             "Loads a new block from a file" 
             (?B) 
             (reset)
             (batch* ?B))

;------------------------------------------------------------------------------
; Autogen methods
;------------------------------------------------------------------------------
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p0
                  {p0}
                  "p0"
                  "{p0}"))))
  [p0])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p1
                  {p1}
                  "p1"
                  "{p1}"))))
  [p1])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p2
                  {p2}
                  "p2"
                  "{p2}"))))
  [p2])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p3
                  {p3}
                  "p3"
                  "{p3}"))))
  [p3])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p4
                  {p4}
                  "p4"
                  "{p4}"))))
  [p4])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p5
                  {p5}
                  "p5"
                  "{p5}"))))
  [p5])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p6
                  {p6}
                  "p6"
                  "{p6}"))))
  [p6])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p7
                  {p7}
                  "p7"
                  "{p7}"))))
  [p7])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p8
                  {p8}
                  "p8"
                  "{p8}"))))
  [p8])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p9
                  {p9}
                  "p9"
                  "{p9}"))))
  [p9])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p10
                  {p10}
                  "p10"
                  "{p10}"))))
  [p10])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p11
                  {p11}
                  "p11"
                  "{p11}"))))
  [p11])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p12
                  {p12}
                  "p12"
                  "{p12}"))))
  [p12])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p13
                  {p13}
                  "p13"
                  "{p13}"))))
  [p13])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p14
                  {p14}
                  "p14"
                  "{p14}"))))
  [p14])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p15
                  {p15}
                  "p15"
                  "{p15}"))))
  [p15])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p16
                  {p16}
                  "p16"
                  "{p16}"))))
  [p16])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p17
                  {p17}
                  "p17"
                  "{p17}"))))
  [p17])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p18
                  {p18}
                  "p18"
                  "{p18}"))))
  [p18])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p19
                  {p19}
                  "p19"
                  "{p19}"))))
  [p19])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p20
                  {p20}
                  "p20"
                  "{p20}"))))
  [p20])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p21
                  {p21}
                  "p21"
                  "{p21}"))))
  [p21])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p22
                  {p22}
                  "p22"
                  "{p22}"))))
  [p22])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p23
                  {p23}
                  "p23"
                  "{p23}"))))
  [p23])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p24
                  {p24}
                  "p24"
                  "{p24}"))))
  [p24])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p25
                  {p25}
                  "p25"
                  "{p25}"))))
  [p25])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p26
                  {p26}
                  "p26"
                  "{p26}"))))
  [p26])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p27
                  {p27}
                  "p27"
                  "{p27}"))))
  [p27])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p28
                  {p28}
                  "p28"
                  "{p28}"))))
  [p28])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p29
                  {p29}
                  "p29"
                  "{p29}"))))
  [p29])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p30
                  {p30}
                  "p30"
                  "{p30}"))))
  [p30])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p31
                  {p31}
                  "p31"
                  "{p31}"))))
  [p31])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p32
                  {p32}
                  "p32"
                  "{p32}"))))
  [p32])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p33
                  {p33}
                  "p33"
                  "{p33}"))))
  [p33])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p34
                  {p34}
                  "p34"
                  "{p34}"))))
  [p34])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p35
                  {p35}
                  "p35"
                  "{p35}"))))
  [p35])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p36
                  {p36}
                  "p36"
                  "{p36}"))))
  [p36])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p37
                  {p37}
                  "p37"
                  "{p37}"))))
  [p37])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p38
                  {p38}
                  "p38"
                  "{p38}"))))
  [p38])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p39
                  {p39}
                  "p39"
                  "{p39}"))))
  [p39])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p40
                  {p40}
                  "p40"
                  "{p40}"))))
  [p40])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p41
                  {p41}
                  "p41"
                  "{p41}"))))
  [p41])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p42
                  {p42}
                  "p42"
                  "{p42}"))))
  [p42])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p43
                  {p43}
                  "p43"
                  "{p43}"))))
  [p43])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p44
                  {p44}
                  "p44"
                  "{p44}"))))
  [p44])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p45
                  {p45}
                  "p45"
                  "{p45}"))))
  [p45])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p46
                  {p46}
                  "p46"
                  "{p46}"))))
  [p46])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p47
                  {p47}
                  "p47"
                  "{p47}"))))
  [p47])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p48
                  {p48}
                  "p48"
                  "{p48}"))))
  [p48])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p49
                  {p49}
                  "p49"
                  "{p49}"))))
  [p49])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p50
                  {p50}
                  "p50"
                  "{p50}"))))
  [p50])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p51
                  {p51}
                  "p51"
                  "{p51}"))))
  [p51])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p52
                  {p52}
                  "p52"
                  "{p52}"))))
  [p52])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p53
                  {p53}
                  "p53"
                  "{p53}"))))
  [p53])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p54
                  {p54}
                  "p54"
                  "{p54}"))))
  [p54])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p55
                  {p55}
                  "p55"
                  "{p55}"))))
  [p55])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p56
                  {p56}
                  "p56"
                  "{p56}"))))
  [p56])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p57
                  {p57}
                  "p57"
                  "{p57}"))))
  [p57])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p58
                  {p58}
                  "p58"
                  "{p58}"))))
  [p58])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p59
                  {p59}
                  "p59"
                  "{p59}"))))
  [p59])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p60
                  {p60}
                  "p60"
                  "{p60}"))))
  [p60])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p61
                  {p61}
                  "p61"
                  "{p61}"))))
  [p61])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p62
                  {p62}
                  "p62"
                  "{p62}"))))
  [p62])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  p63
                  {p63}
                  "p63"
                  "{p63}"))))
  [p63])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r0
                  {r0}
                  "r0"
                  "{r0}"))))
  [r0])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f0
                  {f0}
                  "f0"
                  "{f0}"))))
  [f0])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r1
                  {r1}
                  "r1"
                  "{r1}"))))
  [r1])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f1
                  {f1}
                  "f1"
                  "{f1}"))))
  [f1])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r2
                  {r2}
                  "r2"
                  "{r2}"))))
  [r2])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f2
                  {f2}
                  "f2"
                  "{f2}"))))
  [f2])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r3
                  {r3}
                  "r3"
                  "{r3}"))))
  [r3])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f3
                  {f3}
                  "f3"
                  "{f3}"))))
  [f3])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r4
                  {r4}
                  "r4"
                  "{r4}"))))
  [r4])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f4
                  {f4}
                  "f4"
                  "{f4}"))))
  [f4])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r5
                  {r5}
                  "r5"
                  "{r5}"))))
  [r5])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f5
                  {f5}
                  "f5"
                  "{f5}"))))
  [f5])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r6
                  {r6}
                  "r6"
                  "{r6}"))))
  [r6])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f6
                  {f6}
                  "f6"
                  "{f6}"))))
  [f6])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r7
                  {r7}
                  "r7"
                  "{r7}"))))
  [r7])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f7
                  {f7}
                  "f7"
                  "{f7}"))))
  [f7])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r8
                  {r8}
                  "r8"
                  "{r8}"))))
  [r8])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f8
                  {f8}
                  "f8"
                  "{f8}"))))
  [f8])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r9
                  {r9}
                  "r9"
                  "{r9}"))))
  [r9])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f9
                  {f9}
                  "f9"
                  "{f9}"))))
  [f9])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r10
                  {r10}
                  "r10"
                  "{r10}"))))
  [r10])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f10
                  {f10}
                  "f10"
                  "{f10}"))))
  [f10])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r11
                  {r11}
                  "r11"
                  "{r11}"))))
  [r11])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f11
                  {f11}
                  "f11"
                  "{f11}"))))
  [f11])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r12
                  {r12}
                  "r12"
                  "{r12}"))))
  [r12])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f12
                  {f12}
                  "f12"
                  "{f12}"))))
  [f12])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r13
                  {r13}
                  "r13"
                  "{r13}"))))
  [r13])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f13
                  {f13}
                  "f13"
                  "{f13}"))))
  [f13])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r14
                  {r14}
                  "r14"
                  "{r14}"))))
  [r14])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f14
                  {f14}
                  "f14"
                  "{f14}"))))
  [f14])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r15
                  {r15}
                  "r15"
                  "{r15}"))))
  [r15])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f15
                  {f15}
                  "f15"
                  "{f15}"))))
  [f15])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r16
                  {r16}
                  "r16"
                  "{r16}"))))
  [r16])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f16
                  {f16}
                  "f16"
                  "{f16}"))))
  [f16])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r17
                  {r17}
                  "r17"
                  "{r17}"))))
  [r17])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f17
                  {f17}
                  "f17"
                  "{f17}"))))
  [f17])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r18
                  {r18}
                  "r18"
                  "{r18}"))))
  [r18])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f18
                  {f18}
                  "f18"
                  "{f18}"))))
  [f18])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r19
                  {r19}
                  "r19"
                  "{r19}"))))
  [r19])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f19
                  {f19}
                  "f19"
                  "{f19}"))))
  [f19])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r20
                  {r20}
                  "r20"
                  "{r20}"))))
  [r20])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f20
                  {f20}
                  "f20"
                  "{f20}"))))
  [f20])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r21
                  {r21}
                  "r21"
                  "{r21}"))))
  [r21])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f21
                  {f21}
                  "f21"
                  "{f21}"))))
  [f21])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r22
                  {r22}
                  "r22"
                  "{r22}"))))
  [r22])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f22
                  {f22}
                  "f22"
                  "{f22}"))))
  [f22])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r23
                  {r23}
                  "r23"
                  "{r23}"))))
  [r23])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f23
                  {f23}
                  "f23"
                  "{f23}"))))
  [f23])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r24
                  {r24}
                  "r24"
                  "{r24}"))))
  [r24])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f24
                  {f24}
                  "f24"
                  "{f24}"))))
  [f24])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r25
                  {r25}
                  "r25"
                  "{r25}"))))
  [r25])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f25
                  {f25}
                  "f25"
                  "{f25}"))))
  [f25])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r26
                  {r26}
                  "r26"
                  "{r26}"))))
  [r26])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f26
                  {f26}
                  "f26"
                  "{f26}"))))
  [f26])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r27
                  {r27}
                  "r27"
                  "{r27}"))))
  [r27])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f27
                  {f27}
                  "f27"
                  "{f27}"))))
  [f27])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r28
                  {r28}
                  "r28"
                  "{r28}"))))
  [r28])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f28
                  {f28}
                  "f28"
                  "{f28}"))))
  [f28])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r29
                  {r29}
                  "r29"
                  "{r29}"))))
  [r29])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f29
                  {f29}
                  "f29"
                  "{f29}"))))
  [f29])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r30
                  {r30}
                  "r30"
                  "{r30}"))))
  [r30])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f30
                  {f30}
                  "f30"
                  "{f30}"))))
  [f30])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r31
                  {r31}
                  "r31"
                  "{r31}"))))
  [r31])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f31
                  {f31}
                  "f31"
                  "{f31}"))))
  [f31])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r32
                  {r32}
                  "r32"
                  "{r32}"))))
  [r32])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f32
                  {f32}
                  "f32"
                  "{f32}"))))
  [f32])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r33
                  {r33}
                  "r33"
                  "{r33}"))))
  [r33])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f33
                  {f33}
                  "f33"
                  "{f33}"))))
  [f33])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r34
                  {r34}
                  "r34"
                  "{r34}"))))
  [r34])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f34
                  {f34}
                  "f34"
                  "{f34}"))))
  [f34])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r35
                  {r35}
                  "r35"
                  "{r35}"))))
  [r35])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f35
                  {f35}
                  "f35"
                  "{f35}"))))
  [f35])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r36
                  {r36}
                  "r36"
                  "{r36}"))))
  [r36])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f36
                  {f36}
                  "f36"
                  "{f36}"))))
  [f36])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r37
                  {r37}
                  "r37"
                  "{r37}"))))
  [r37])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f37
                  {f37}
                  "f37"
                  "{f37}"))))
  [f37])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r38
                  {r38}
                  "r38"
                  "{r38}"))))
  [r38])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f38
                  {f38}
                  "f38"
                  "{f38}"))))
  [f38])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r39
                  {r39}
                  "r39"
                  "{r39}"))))
  [r39])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f39
                  {f39}
                  "f39"
                  "{f39}"))))
  [f39])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r40
                  {r40}
                  "r40"
                  "{r40}"))))
  [r40])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f40
                  {f40}
                  "f40"
                  "{f40}"))))
  [f40])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r41
                  {r41}
                  "r41"
                  "{r41}"))))
  [r41])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f41
                  {f41}
                  "f41"
                  "{f41}"))))
  [f41])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r42
                  {r42}
                  "r42"
                  "{r42}"))))
  [r42])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f42
                  {f42}
                  "f42"
                  "{f42}"))))
  [f42])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r43
                  {r43}
                  "r43"
                  "{r43}"))))
  [r43])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f43
                  {f43}
                  "f43"
                  "{f43}"))))
  [f43])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r44
                  {r44}
                  "r44"
                  "{r44}"))))
  [r44])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f44
                  {f44}
                  "f44"
                  "{f44}"))))
  [f44])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r45
                  {r45}
                  "r45"
                  "{r45}"))))
  [r45])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f45
                  {f45}
                  "f45"
                  "{f45}"))))
  [f45])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r46
                  {r46}
                  "r46"
                  "{r46}"))))
  [r46])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f46
                  {f46}
                  "f46"
                  "{f46}"))))
  [f46])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r47
                  {r47}
                  "r47"
                  "{r47}"))))
  [r47])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f47
                  {f47}
                  "f47"
                  "{f47}"))))
  [f47])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r48
                  {r48}
                  "r48"
                  "{r48}"))))
  [r48])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f48
                  {f48}
                  "f48"
                  "{f48}"))))
  [f48])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r49
                  {r49}
                  "r49"
                  "{r49}"))))
  [r49])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f49
                  {f49}
                  "f49"
                  "{f49}"))))
  [f49])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r50
                  {r50}
                  "r50"
                  "{r50}"))))
  [r50])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f50
                  {f50}
                  "f50"
                  "{f50}"))))
  [f50])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r51
                  {r51}
                  "r51"
                  "{r51}"))))
  [r51])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f51
                  {f51}
                  "f51"
                  "{f51}"))))
  [f51])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r52
                  {r52}
                  "r52"
                  "{r52}"))))
  [r52])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f52
                  {f52}
                  "f52"
                  "{f52}"))))
  [f52])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r53
                  {r53}
                  "r53"
                  "{r53}"))))
  [r53])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f53
                  {f53}
                  "f53"
                  "{f53}"))))
  [f53])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r54
                  {r54}
                  "r54"
                  "{r54}"))))
  [r54])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f54
                  {f54}
                  "f54"
                  "{f54}"))))
  [f54])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r55
                  {r55}
                  "r55"
                  "{r55}"))))
  [r55])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f55
                  {f55}
                  "f55"
                  "{f55}"))))
  [f55])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r56
                  {r56}
                  "r56"
                  "{r56}"))))
  [r56])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f56
                  {f56}
                  "f56"
                  "{f56}"))))
  [f56])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r57
                  {r57}
                  "r57"
                  "{r57}"))))
  [r57])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f57
                  {f57}
                  "f57"
                  "{f57}"))))
  [f57])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r58
                  {r58}
                  "r58"
                  "{r58}"))))
  [r58])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f58
                  {f58}
                  "f58"
                  "{f58}"))))
  [f58])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r59
                  {r59}
                  "r59"
                  "{r59}"))))
  [r59])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f59
                  {f59}
                  "f59"
                  "{f59}"))))
  [f59])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r60
                  {r60}
                  "r60"
                  "{r60}"))))
  [r60])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f60
                  {f60}
                  "f60"
                  "{f60}"))))
  [f60])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r61
                  {r61}
                  "r61"
                  "{r61}"))))
  [r61])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f61
                  {f61}
                  "f61"
                  "{f61}"))))
  [f61])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r62
                  {r62}
                  "r62"
                  "{r62}"))))
  [r62])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f62
                  {f62}
                  "f62"
                  "{f62}"))))
  [f62])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r63
                  {r63}
                  "r63"
                  "{r63}"))))
  [r63])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f63
                  {f63}
                  "f63"
                  "{f63}"))))
  [f63])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r64
                  {r64}
                  "r64"
                  "{r64}"))))
  [r64])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f64
                  {f64}
                  "f64"
                  "{f64}"))))
  [f64])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r65
                  {r65}
                  "r65"
                  "{r65}"))))
  [r65])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f65
                  {f65}
                  "f65"
                  "{f65}"))))
  [f65])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r66
                  {r66}
                  "r66"
                  "{r66}"))))
  [r66])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f66
                  {f66}
                  "f66"
                  "{f66}"))))
  [f66])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r67
                  {r67}
                  "r67"
                  "{r67}"))))
  [r67])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f67
                  {f67}
                  "f67"
                  "{f67}"))))
  [f67])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r68
                  {r68}
                  "r68"
                  "{r68}"))))
  [r68])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f68
                  {f68}
                  "f68"
                  "{f68}"))))
  [f68])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r69
                  {r69}
                  "r69"
                  "{r69}"))))
  [r69])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f69
                  {f69}
                  "f69"
                  "{f69}"))))
  [f69])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r70
                  {r70}
                  "r70"
                  "{r70}"))))
  [r70])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f70
                  {f70}
                  "f70"
                  "{f70}"))))
  [f70])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r71
                  {r71}
                  "r71"
                  "{r71}"))))
  [r71])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f71
                  {f71}
                  "f71"
                  "{f71}"))))
  [f71])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r72
                  {r72}
                  "r72"
                  "{r72}"))))
  [r72])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f72
                  {f72}
                  "f72"
                  "{f72}"))))
  [f72])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r73
                  {r73}
                  "r73"
                  "{r73}"))))
  [r73])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f73
                  {f73}
                  "f73"
                  "{f73}"))))
  [f73])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r74
                  {r74}
                  "r74"
                  "{r74}"))))
  [r74])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f74
                  {f74}
                  "f74"
                  "{f74}"))))
  [f74])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r75
                  {r75}
                  "r75"
                  "{r75}"))))
  [r75])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f75
                  {f75}
                  "f75"
                  "{f75}"))))
  [f75])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r76
                  {r76}
                  "r76"
                  "{r76}"))))
  [r76])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f76
                  {f76}
                  "f76"
                  "{f76}"))))
  [f76])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r77
                  {r77}
                  "r77"
                  "{r77}"))))
  [r77])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f77
                  {f77}
                  "f77"
                  "{f77}"))))
  [f77])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r78
                  {r78}
                  "r78"
                  "{r78}"))))
  [r78])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f78
                  {f78}
                  "f78"
                  "{f78}"))))
  [f78])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r79
                  {r79}
                  "r79"
                  "{r79}"))))
  [r79])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f79
                  {f79}
                  "f79"
                  "{f79}"))))
  [f79])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r80
                  {r80}
                  "r80"
                  "{r80}"))))
  [r80])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f80
                  {f80}
                  "f80"
                  "{f80}"))))
  [f80])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r81
                  {r81}
                  "r81"
                  "{r81}"))))
  [r81])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f81
                  {f81}
                  "f81"
                  "{f81}"))))
  [f81])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r82
                  {r82}
                  "r82"
                  "{r82}"))))
  [r82])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f82
                  {f82}
                  "f82"
                  "{f82}"))))
  [f82])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r83
                  {r83}
                  "r83"
                  "{r83}"))))
  [r83])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f83
                  {f83}
                  "f83"
                  "{f83}"))))
  [f83])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r84
                  {r84}
                  "r84"
                  "{r84}"))))
  [r84])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f84
                  {f84}
                  "f84"
                  "{f84}"))))
  [f84])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r85
                  {r85}
                  "r85"
                  "{r85}"))))
  [r85])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f85
                  {f85}
                  "f85"
                  "{f85}"))))
  [f85])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r86
                  {r86}
                  "r86"
                  "{r86}"))))
  [r86])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f86
                  {f86}
                  "f86"
                  "{f86}"))))
  [f86])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r87
                  {r87}
                  "r87"
                  "{r87}"))))
  [r87])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f87
                  {f87}
                  "f87"
                  "{f87}"))))
  [f87])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r88
                  {r88}
                  "r88"
                  "{r88}"))))
  [r88])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f88
                  {f88}
                  "f88"
                  "{f88}"))))
  [f88])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r89
                  {r89}
                  "r89"
                  "{r89}"))))
  [r89])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f89
                  {f89}
                  "f89"
                  "{f89}"))))
  [f89])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r90
                  {r90}
                  "r90"
                  "{r90}"))))
  [r90])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f90
                  {f90}
                  "f90"
                  "{f90}"))))
  [f90])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r91
                  {r91}
                  "r91"
                  "{r91}"))))
  [r91])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f91
                  {f91}
                  "f91"
                  "{f91}"))))
  [f91])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r92
                  {r92}
                  "r92"
                  "{r92}"))))
  [r92])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f92
                  {f92}
                  "f92"
                  "{f92}"))))
  [f92])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r93
                  {r93}
                  "r93"
                  "{r93}"))))
  [r93])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f93
                  {f93}
                  "f93"
                  "{f93}"))))
  [f93])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r94
                  {r94}
                  "r94"
                  "{r94}"))))
  [r94])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f94
                  {f94}
                  "f94"
                  "{f94}"))))
  [f94])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r95
                  {r95}
                  "r95"
                  "{r95}"))))
  [r95])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f95
                  {f95}
                  "f95"
                  "{f95}"))))
  [f95])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r96
                  {r96}
                  "r96"
                  "{r96}"))))
  [r96])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f96
                  {f96}
                  "f96"
                  "{f96}"))))
  [f96])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r97
                  {r97}
                  "r97"
                  "{r97}"))))
  [r97])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f97
                  {f97}
                  "f97"
                  "{f97}"))))
  [f97])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r98
                  {r98}
                  "r98"
                  "{r98}"))))
  [r98])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f98
                  {f98}
                  "f98"
                  "{f98}"))))
  [f98])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r99
                  {r99}
                  "r99"
                  "{r99}"))))
  [r99])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f99
                  {f99}
                  "f99"
                  "{f99}"))))
  [f99])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r100
                  {r100}
                  "r100"
                  "{r100}"))))
  [r100])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f100
                  {f100}
                  "f100"
                  "{f100}"))))
  [f100])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r101
                  {r101}
                  "r101"
                  "{r101}"))))
  [r101])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f101
                  {f101}
                  "f101"
                  "{f101}"))))
  [f101])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r102
                  {r102}
                  "r102"
                  "{r102}"))))
  [r102])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f102
                  {f102}
                  "f102"
                  "{f102}"))))
  [f102])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r103
                  {r103}
                  "r103"
                  "{r103}"))))
  [r103])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f103
                  {f103}
                  "f103"
                  "{f103}"))))
  [f103])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r104
                  {r104}
                  "r104"
                  "{r104}"))))
  [r104])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f104
                  {f104}
                  "f104"
                  "{f104}"))))
  [f104])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r105
                  {r105}
                  "r105"
                  "{r105}"))))
  [r105])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f105
                  {f105}
                  "f105"
                  "{f105}"))))
  [f105])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r106
                  {r106}
                  "r106"
                  "{r106}"))))
  [r106])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f106
                  {f106}
                  "f106"
                  "{f106}"))))
  [f106])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r107
                  {r107}
                  "r107"
                  "{r107}"))))
  [r107])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f107
                  {f107}
                  "f107"
                  "{f107}"))))
  [f107])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r108
                  {r108}
                  "r108"
                  "{r108}"))))
  [r108])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f108
                  {f108}
                  "f108"
                  "{f108}"))))
  [f108])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r109
                  {r109}
                  "r109"
                  "{r109}"))))
  [r109])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f109
                  {f109}
                  "f109"
                  "{f109}"))))
  [f109])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r110
                  {r110}
                  "r110"
                  "{r110}"))))
  [r110])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f110
                  {f110}
                  "f110"
                  "{f110}"))))
  [f110])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r111
                  {r111}
                  "r111"
                  "{r111}"))))
  [r111])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f111
                  {f111}
                  "f111"
                  "{f111}"))))
  [f111])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r112
                  {r112}
                  "r112"
                  "{r112}"))))
  [r112])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f112
                  {f112}
                  "f112"
                  "{f112}"))))
  [f112])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r113
                  {r113}
                  "r113"
                  "{r113}"))))
  [r113])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f113
                  {f113}
                  "f113"
                  "{f113}"))))
  [f113])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r114
                  {r114}
                  "r114"
                  "{r114}"))))
  [r114])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f114
                  {f114}
                  "f114"
                  "{f114}"))))
  [f114])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r115
                  {r115}
                  "r115"
                  "{r115}"))))
  [r115])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f115
                  {f115}
                  "f115"
                  "{f115}"))))
  [f115])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r116
                  {r116}
                  "r116"
                  "{r116}"))))
  [r116])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f116
                  {f116}
                  "f116"
                  "{f116}"))))
  [f116])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r117
                  {r117}
                  "r117"
                  "{r117}"))))
  [r117])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f117
                  {f117}
                  "f117"
                  "{f117}"))))
  [f117])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r118
                  {r118}
                  "r118"
                  "{r118}"))))
  [r118])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f118
                  {f118}
                  "f118"
                  "{f118}"))))
  [f118])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r119
                  {r119}
                  "r119"
                  "{r119}"))))
  [r119])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f119
                  {f119}
                  "f119"
                  "{f119}"))))
  [f119])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r120
                  {r120}
                  "r120"
                  "{r120}"))))
  [r120])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f120
                  {f120}
                  "f120"
                  "{f120}"))))
  [f120])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r121
                  {r121}
                  "r121"
                  "{r121}"))))
  [r121])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f121
                  {f121}
                  "f121"
                  "{f121}"))))
  [f121])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r122
                  {r122}
                  "r122"
                  "{r122}"))))
  [r122])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f122
                  {f122}
                  "f122"
                  "{f122}"))))
  [f122])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r123
                  {r123}
                  "r123"
                  "{r123}"))))
  [r123])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f123
                  {f123}
                  "f123"
                  "{f123}"))))
  [f123])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r124
                  {r124}
                  "r124"
                  "{r124}"))))
  [r124])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f124
                  {f124}
                  "f124"
                  "{f124}"))))
  [f124])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r125
                  {r125}
                  "r125"
                  "{r125}"))))
  [r125])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f125
                  {f125}
                  "f125"
                  "{f125}"))))
  [f125])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r126
                  {r126}
                  "r126"
                  "{r126}"))))
  [r126])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f126
                  {f126}
                  "f126"
                  "{f126}"))))
  [f126])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  r127
                  {r127}
                  "r127"
                  "{r127}"))))
  [r127])
(defmethod translate-register
  ((?in LEXEME 
        (not (neq ?in
                  f127
                  {f127}
                  "f127"
                  "{f127}"))))
  [f127])
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         and
                         "and"))))
  A)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         or
                         "or"))))
  A)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.a
                         "nop.a"))))
  A)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         add
                         "add"))))
  A)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         mul
                         "mul"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         sub
                         "sub"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         cmp
                         "cmp"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         cmp.eq
                         "cmp.eq"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         cmp.gt
                         "cmp.gt"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         cmp.lt
                         "cmp.lt"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         cmp.neq
                         "cmp.neq"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         div
                         "div"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         shl
                         "shl"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         shr
                         "shr"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         sxt4
                         "sxt4"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         adds
                         "adds"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.i
                         "nop.i"))))
  I)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         st8
                         "st8"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         ld8
                         "ld8"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         ld4
                         "ld4"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         st4
                         "st4"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         mov
                         "mov"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         stf.spill
                         "stf.spill"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.m
                         "nop.m"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         alloc
                         "alloc"))))
  M)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br
                         "br"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.ret
                         "br.ret"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.call
                         "br.call"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.few
                         "br.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.many
                         "br.many"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.ret.sptk.many
                         "br.ret.sptk.many"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.ret.sptk.few
                         "br.ret.sptk.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.ret.dptk.many
                         "br.ret.dptk.many"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.ret.dptk.few
                         "br.ret.dptk.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.call.sptk.many
                         "br.call.sptk.many"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.call.sptk.few
                         "br.call.sptk.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.cond.dptk.few
                         "br.cond.dptk.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.call.dptk.many
                         "br.call.dptk.many"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         br.call.dptk.few
                         "br.call.dptk.few"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.b
                         "nop.b"))))
  B)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         ldfs
                         "ldfs"))))
  F)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         fma
                         "fma"))))
  F)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.f
                         "nop.f"))))
  F)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         none
                         "none"))))
  X)
(defmethod translate-operation 
  ((?op LEXEME (not (neq ?op
                         nop.x
                         "nop.x"))))
  X)


;------------------------------------------------------------------------------
; Templates
;------------------------------------------------------------------------------
(defmodule MAIN
           (export ?ALL))
(defrule MAIN::startup
         =>
         (focus schedule 
                schedule-update))

(defmodule schedule
           "identify instructions to schedule"
           (import MAIN 
                   ?ALL)
           (export deftemplate 
                   schedule-directive))

(deftemplate schedule::schedule-directive
             (slot target
                   (default ?NONE)))
(defmodule schedule-update
           "Update, and output, the contents of a packet"
           (import MAIN
                   ?ALL)
           (import schedule
                   deftemplate
                   schedule-directive))

;------------------------------------------------------------------------------
; Scheduling rules
;------------------------------------------------------------------------------
(defrule schedule::determine-scheduability
         "An object is able to be scheduled if it has no remaining producers"
         (object (is-a register)
                 (queue ?q $?))
         (test (send ?q
                     ready-to-schedule))
         =>
         (assert (schedule-directive (target ?q))))

(defrule schedule-update::update-producer-set
         ?f <- (schedule-directive (target ?q))
         =>
         (retract ?f)
         (assert (Restart Scheduling))
         (printout ?*output-router* 
                   (send ?q notify-scheduling) crlf))

(defrule schedule-update::restart-scheduling
         (declare (salience -1))
         ?f <- (Restart Scheduling)
         =>
         (retract ?f)
         (printout ?*output-router* ";;" crlf)
         (focus schedule))

(defrule schedule-update::insert-branch
         (declare (salience -2))
         (object (is-a Instruction)
                 (InstructionType B)
                 (name ?op))
         =>
         (printout ?*output-router* 
                   (send ?op
                         notify-scheduling) crlf
                   ";;" crlf))

