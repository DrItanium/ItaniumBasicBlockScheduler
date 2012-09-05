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
(defclass List (is-a USER)
 (slot Name (default nil))
 (slot GUID (default-dynamic (gensym*)))
 (multislot Contents))

(defmessage-handler List .IsEmpty ()
 (eq (length ?self:Contents) 0))

(defmessage-handler List .Count ()
 (length ?self:Contents))

(defmessage-handler List .SetAdd (?element) 
 (if (not (send ?self .Contains ?element)) then
  (send ?self .Add ?element)))

(defmessage-handler List .Add (?element)
 (bind ?new-list (create$ (dynamic-get Contents) ?element))
 (dynamic-put Contents ?new-list))

(defmessage-handler List .Remove (?element)
 (dynamic-put Contents (delete-member$ (dynamic-get Contents) ?element)))

(defmessage-handler List .RemoveAt (?element)
 (dynamic-put Contents (delete$ ?self ?element ?element)))

(defmessage-handler List .AddRange ($?element)
 (dynamic-put Contents (create$ ?self:Contents ?element)))

(defmessage-handler List .ElementAt (?index)
 (nth$ ?index ?self:Contents))

(defmessage-handler List .Contains (?item)
 (not (eq (member$ ?item ?self:Contents) FALSE)))

(defmessage-handler List .ContainsSubset ($?subset)
 (foreach ?c ?subset
  (if (not (send ?self .Contains ?c)) then
   (return FALSE)))
 (return TRUE))

(defmessage-handler List .Unique () 
 "Return the unique elements of this list into a new list"
 (bind ?q (gensym*))
 (make-instance (symbol-to-instance-name ?q) of List (GUID ?q))
 (foreach ?i ?self:Contents
  (if (not (send (symbol-to-instance-name ?q) .Contains ?i)) then
	(send (symbol-to-instance-name ?q) .Add ?i)))
 (return ?q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass DependencyChain (is-a USER)
 (slot GUID (default-dynamic (gensym*)))
 (slot Producers (default-dynamic (make-instance (gensym*) of List)))
 (slot Consumers (default-dynamic (make-instance (gensym*) of List))))

(defmessage-handler DependencyChain .AddProducer (?p) 
 "Adds a producer to the list of producers"
 (send ?self:Producers .SetAdd ?p))
(defmessage-handler DependencyChain .AddConsumer (?p)
 "Adds a producer to the list of consumers"
 (send ?self:Consumers .SetAdd ?p))
(defmessage-handler DependencyChain .IsProducer (?p)
 (send ?self:Producers .Contains ?p))

(defmessage-handler DependencyChain .IsConsumer (?p)
 (send ?self:Consumers .Contains ?p))

(defmessage-handler DependencyChain .HasProducers ()
 (not (send ?self:Producers .IsEmpty)))

(defmessage-handler DependencyChain .HasConsumers ()
 (not (send ?self:Consumers .IsEmpty)))

(defmessage-handler DependencyChain .RemoveConsumer (?v)
  (send ?self:Consumers .Remove ?v))

(defmessage-handler DependencyChain .RemoveProducer (?v)
  (send ?self:Producers .Remove ?v))

(defmessage-handler DependencyChain .ProducersContainsSubset ($?v)
 (send ?self:Producers .ContainsSubset $?v))

(defmessage-handler DependencyChain .ConsumersContainsSubset ($?v)
 (send ?self:Consumers .ContainsSubset $?v))

(defmessage-handler DependencyChain .Producers () 
 "Returns the list of producers from the dependency information"
 (return (send ?self:Producers get-Contents)))

(defmessage-handler DependencyChain .Consumers () 
 "Returns the list of consumers from the dependency information"
 (return (send ?self:Consumers get-Contents)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Instruction (is-a USER)
 (slot GUID (default-dynamic (gensym*)))
 (slot Predicate (type SYMBOL))
 (slot Name (type SYMBOL))
 (slot TimeIndex (type NUMBER))
 (slot ExecutionLength (type NUMBER))
 (slot DestinationRegisters (default-dynamic (make-instance (gensym*) of List)))
 (slot SourceRegisters (default-dynamic (make-instance (gensym*) of List)))
 (slot InstructionType (type SYMBOL) (default-dynamic nil))
 (slot DependencyInformation (default-dynamic (make-instance (gensym*) of DependencyChain))))

(defmessage-handler Instruction .ToString ()
 "Creates a string representation of the given instruction"
 (bind ?dest (send ?self:DestinationRegisters get-Contents))
 (bind ?src (send ?self:SourceRegisters get-Contents))
 (if (send ?self:SourceRegisters .IsEmpty) then
  (return (str-cat "(" ?self:Predicate ") " ?self:Name " " (implode$ ?dest)))
  else
 (return (str-cat "(" ?self:Predicate ") " ?self:Name " " (implode$ ?dest) 
			 " = " (implode$ ?src)))))

(defmessage-handler Instruction .DestinationRegisters () 
 "Returns the list of destination registers"
 ?self:DestinationRegisters)

(defmessage-handler Instruction .SourceRegisters () 
 "Returns the list of source registers"
 ?self:SourceRegisters)
(defmessage-handler Instruction .DestinationRegisters.Count () 
 "Gets the number of destination registers"
 (send ?self:DestinationRegisters .Count))

(defmessage-handler Instruction .SourceRegisters.Count () 
 "Gets the number of source registers"
 (send ?self:SourceRegisters .Count))

(defmessage-handler Instruction .AddDestinationRegisters ($?dr)
 "Adds a list of destination registers to the destination
 registers list"
 (send ?self:DestinationRegisters .AddRange ?dr))

(defmessage-handler Instruction .AddSourceRegisters ($?dr)
 "Adds a list of destination registers to the destination
 registers list"
 (send ?self:SourceRegisters .AddRange ?dr))

(defmessage-handler Instruction .IsProducer (?p)
 (send ?self:DependencyInformation .IsProducer ?p))

(defmessage-handler Instruction .IsConsumer (?p)
 (send ?self:DependencyInformation .IsConsumer ?p))

(defmessage-handler Instruction .AddProducer (?p)
 (send ?self:DependencyInformation .AddProducer ?p))

(defmessage-handler Instruction .AddConsumer (?p)
 (send ?self:DependencyInformation .AddConsumer ?p))

(defmessage-handler Instruction .RemoveProducer (?p)
 (send ?self:DependencyInformation .RemoveProducer ?p))

(defmessage-handler Instruction .RemoveConsumer (?p)
 (send ?self:DependencyInformation .RemoveConsumer ?p))

(defmessage-handler Instruction .ContainsSourceRegister (?r)
 (send ?self:SourceRegisters .Contains ?r))

(defmessage-handler Instruction .ContainsSourceRegisters ($?r)
 (foreach ?reg ?r
  (if (send ?self .ContainsSourceRegister ?reg) then
	(return TRUE)))
 (return FALSE))

(defmessage-handler Instruction .ContainsDestinationRegister (?r)
 (send ?self:DestinationRegisters .Contains ?r))

(defmessage-handler Instruction .ContainsDestinationRegisters ($?r)
 (foreach ?reg ?r
  (if (send ?self .ContainsDestinationRegister ?reg) then
	(return TRUE)))
 (return FALSE))

(defmessage-handler Instruction .Producers () 
 "Returns the list of producers from the dependency information"
 (return (send (send ?self:DependencyInformation get-Producers) get-Contents)))

(defmessage-handler Instruction .Consumers () 
 "Returns the list of consumers from the dependency information"
 (return (send (send ?self:DependencyInformation get-Consumers) get-Contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Operation (is-a USER)
 (slot Name (type SYMBOL) (default ?DERIVE))
 (slot Class (type SYMBOL) (default ?DERIVE))
 (slot Length (type NUMBER) (range 0 ?VARIABLE) (default 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Register (is-a USER)
 (slot Name (type SYMBOL) (default ?DERIVE))
 (slot Class (type SYMBOL) (default ?DERIVE))
 (slot Length (type NUMBER) (range 0 ?VARIABLE) (default 1))
 (multislot OtherNames (type SYMBOL) (default ?DERIVE)))


(defmessage-handler Register .RegisterEquals (?sym) 
 "Checks to see if the register is equal to the given symbol.
 It also checks to see if the symbol is the memory access version of the given
 register."
 (return (and (symbolp ?sym) 
			 (or (eq ?sym ?self:Name) 
			  (not (eq (member$ ?sym ?self:OtherNames) 
					  FALSE))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass InstructionGroup (is-a USER)
 (slot Contents (default-dynamic (make-instance (gensym*) of List)))
 (slot TimeIndex (type NUMBER))
 (slot Printed (type SYMBOL) (default-dynamic FALSE)))

(defmessage-handler InstructionGroup .IsEmpty ()
 (send ?self:Contents .IsEmpty))

(defmessage-handler InstructionGroup .Count ()
 (send ?self:Contents .Count))

(defmessage-handler InstructionGroup .SetAdd (?element) 
 (send ?self:Contents .SetAdd ?element))

(defmessage-handler InstructionGroup .Add (?element)
 (send ?self:Contents .Add ?element))

(defmessage-handler InstructionGroup .Remove (?element)
 (send ?self:Contents .Remove ?element))

(defmessage-handler InstructionGroup .RemoveAt (?element)
 (send ?self:Contents .RemoveAt ?element))

(defmessage-handler InstructionGroup .AddRange ($?element)
 (send ?self:Contents .AddRange ?element))

(defmessage-handler InstructionGroup .ElementAt (?index)
 (send ?self:Contents .ElementAt ?index))

(defmessage-handler InstructionGroup .Contains (?item)
 (send ?self:Contents .Contains ?item))

(defmessage-handler InstructionGroup .ContainsSubset (?subset)
 (send ?self:Contents .ContainsSubset ?subset))

(defmessage-handler InstructionGroup .Unique () 
 "Return the unique elements of this list into a new list"
 (send ?self:Contents .Unique))
(defmessage-handler InstructionGroup .HasBeenPrinted ()
 (return (eq ?self:Printed TRUE)))
