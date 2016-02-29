(defmethod defop 
  "Defines an operation"
  ((?Op LEXEME)
   (?Type SYMBOL))
  (format t 
          "(defmethod translate-operation 
             ((?op LEXEME (not (neq ?current-argument
                                    %s
                                    \"%s\"))))
             %s)%n"
          ?Op
          ?Op
          ?Type))


(defmethod defop-range
  ((?Type SYMBOL)
   (?Ops MULTIFIELD LEXEME))
  (progn$ (?op ?Ops)
          (defop ?op ?Type)))

(defmethod defop-range
  ((?Type SYMBOL)
   ($?Ops LEXEME (> (length$ $?Ops) 1)))
  (defop-range ?Type $?Ops))

(defmethod defop-range
  ((?Type SYMBOL)
   (?Op LEXEME))
  (defop ?Op ?Type))

(deffunction build-machine
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

(defmessage-handler register init after
                    ()
                    (bind ?title 
                          (instance-name ?self))
                    (format t
                            "(defmethod translate-register
                               ((?in LEXEME 
                                     (not (neq ?current-argument
                                               %s
                                               {%s}
                                               \"%s\"
                                               \"{%s}\"))))
                               [%s])%n"
                            ?title
                            ?title
                            ?title
                            ?title
                            ?title))
