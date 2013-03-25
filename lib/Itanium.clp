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
;; Itanium.clp - Defines Itanium architecture features
;; Written by Joshua Scoggins 
;;Register Classes

(deffunction special-registers "Defines a list of special registers"
             (?Size $?Registers)
             (registers Special ?Size $?Registers))

(deffunction application-registers "Defines a list of application registers"
             (?Size $?Registers)
             (registers Application ?Size $?Registers))

(deffunction float-registers "Defines a list of floating point registers"
             (?Size $?Registers)
             (registers Float ?Size $?Registers))


(deffunction predicate-registers "Defines a list of floating point registers"
             (?Size $?Registers)
             (registers Predicate ?Size $?Registers))

(deffunction gpr-registers "Defines a list of floating point registers"
             (?Size $?Registers)
             (registers GPR ?Size $?Registers))

(deffunction br (?Predicate ?Target)
             (make-instruction ?Predicate br ?Target))

(deffunction br.cond (?Predicate ?Target)
             (make-instruction ?Predicate br.cond ?Target))

(deffunction br.few (?Predicate ?Target)
             (make-instruction ?Predicate br.few ?Target))

(deffunction br.many (?Predicate ?Target)
             (make-instruction ?Predicate br.many ?Target))

(deffunction br.cond.dptk.few (?Predicate ?Target)
             (make-instruction ?Predicate br.cond.dptk.few ?Target))

(deffunction br.cond.dptk.many (?Predicate ?Target)
             (make-instruction ?Predicate br.cond.dptk.many ?Target))


(deffunction br.cond.sptk.few (?Predicate ?Target)
             (make-instruction ?Predicate br.cond.sptk.few ?Target))

(deffunction br.cond.sptk.many (?Predicate ?Target)
             (make-instruction ?Predicate br.cond.sptk.many ?Target))

(deffunction br.ret.dptk.few (?Predicate ?Target)
             (make-instruction ?Predicate br.ret.dptk.few ?Target))

(deffunction br.ret.dptk.many (?Predicate ?Target)
             (make-instruction ?Predicate br.ret.dptk.many ?Target))


(deffunction br.ret.sptk.few (?Predicate ?Target)
             (make-instruction ?Predicate br.ret.sptk.few ?Target))

(deffunction br.ret.sptk.many (?Predicate ?Target)
             (make-instruction ?Predicate br.ret.sptk.many ?Target))

(deffunction br.call.dptk.few (?Predicate ?Target ?Source)
             (make-instruction ?Predicate br.call.dptk.few ?Target ?Source))

(deffunction br.call.dptk.many (?Predicate ?Target ?Source)
             (make-instruction ?Predicate br.call.dptk.many ?Target ?Source))

(deffunction br.call.sptk.few (?Predicate ?Target ?Source)
             (make-instruction ?Predicate br.call.sptk.few ?Target ?Source))

(deffunction br.call.sptk.many (?Predicate ?Target ?Source)
             (make-instruction ?Predicate br.call.sptk.many ?Target ?Source))

(deffunction adds (?Pred ?Dest ?Source0 ?Source1)
             (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction addl (?Pred ?Dest ?Source0 ?Source1)
             (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction add (?Pred ?Dest ?Source0 ?Source1)
             (make-instruction ?Pred add ?Dest ?Source0 ?Source1))

(deffunction sub (?P ?D ?S0 ?S1)
             (make-instruction ?P sub ?D ?S0 ?S1))

(deffunction and-inst (?Pred ?Dest ?Source0 ?Source1)
             (make-instruction ?Pred adds ?Dest ?Source0 ?Source1))

(deffunction xmpy.l (?Pred ?Dest ?Source0 ?Source1)
             (make-instruction ?Pred xmpy.l ?Dest ?Source0 ?Source1))

(deffunction mov (?Pred ?Dest ?Source0)
             (make-instruction ?Pred mov ?Dest ?Source0))

(deffunction mov.i (?Pred ?Dest ?Source0)
             (make-instruction ?Pred mov.i ?Dest ?Source0))

(deffunction sxt4 (?Pred ?Dest ?Source0)
             (make-instruction ?Pred sxt4 ?Dest ?Source0))

(deffunction stf.spill (?Pred ?Dest ?Source0)
             (make-instruction ?Pred stf.spill ?Dest ?Source0))

(deffunction ldfs (?P ?D ?S)
             (make-instruction ?P ldfs ?D ?S))

(deffunction nop.b (?P)
             (make-instruction ?P nop.b 0x0))

(deffunction nop.i (?P)
             (make-instruction ?P nop.i 0x0))

(deffunction nop.f (?P)
             (make-instruction ?P nop.f 0x0))

(deffunction nop.m (?P)
             (make-instruction ?P nop.m 0x0))

(deffunction nop.x (?P)
             (make-instruction ?P nop.x 0x0))

(deffunction cmp.eq (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp.eq ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.lt (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp.gt (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp.gt ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.eq (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp4.eq ?D0 ?D1 ?S0 ?S1))

(deffunction cmp4.lt (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp4.lt ?D0 ?D1 ?S0 ?S1))
(deffunction cmp4.gt (?Predicate ?D0 ?D1 ?S0 ?S1)
             (make-instruction ?Predicate cmp4.gt ?D0 ?D1 ?S0 ?S1))

(deffunction setf.sig (?P ?D ?S)
             (make-instruction ?P setf.sig ?D ?S))

(deffunction getf.sig (?P ?D ?S)
             (make-instruction ?P getf.sig ?D ?S))

(deffunction ld8 (?P ?D ?S)
             (make-instruction ?P ld8 ?D ?S))

(deffunction ld4 (?P ?D ?S)
             (make-instruction ?P ld4 ?D ?S))

(deffunction st8 (?P ?D ?S)
             (make-instruction ?P st8 ?D ?S))

(deffunction st4 (?P ?D ?S)
             (make-instruction ?P st4 ?D ?S))

(deffunction alloc (?P ?D ?I ?L ?O ?R)
             (make-instruction ?P alloc ?D (create$ ar.pfs ?I ?L ?O ?R)))

(deffunction init-ia64-machine ()
             (defop-range A 1 and or nop.a add)

             (defop-range I 1 mul sub cmp cmp.eq cmp.gt cmp.lt cmp.neq div shl 
                          shr sxt4 adds nop.i)

             (defop-range M 1 st8 ld8 ld4 st4 mov stf.spill nop.m alloc)

             (defop-range B 1 br br.ret br.call br.few br.many 
                          br.ret.sptk.many br.ret.sptk.few br.ret.dptk.many 
                          br.ret.dptk.few br.call.sptk.many br.call.sptk.few 
								  br.cond.dptk.few br.call.dptk.many br.call.dptk.few nop.b)
             (defop-range F 4 ldfs fma nop.f)
             (defop-range X 2 none nop.x)


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

