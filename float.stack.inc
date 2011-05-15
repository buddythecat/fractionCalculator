; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
; prototypes for the fraction stack management function
; Copyright David B. Sher 2011
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
include fraction.inc


; push the numerator and denominator onto the stack
stackPush PROTO :real4
; get the numerator and denominator from the top of the stack
stackTop PROTO :PTR real4
; pop the stack
stackPop PROTO
; find out if the stack is empty
stackEmpty PROTO
; find out if the stack is full
stackFull PROTO
; Retrieves a fraction from the top of the stack, puts the numerator in ecx and the denominator in edx
topStack PROTO
; Print the entire stack out to the console
printStack PROTO