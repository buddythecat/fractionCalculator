; ¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤
; PROCs for manipulating a stack of fractions
; Copyright David B. Sher 2011
; ¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤=÷=¤

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
    include \masm32\include\masm32rt.inc
    include stack.inc
	include fraction.inc
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤




.data
MAXSIZE EQU 20  ; the maximum size of the stack
theStack DWORD MAXSIZE dup(?) ; maximum stack size is 20
floatStack DWORD MAXSIZE dup(?)
stackSize WORD 0  ; initially stack is empty
aFrac fraction <0,0> ; used to build fraction
dummyFrac fraction <0,0>


.code

; Pushes a fraction on top of the stack (in the last position of the array
stackPush PROC USES eax ebx esi edi, frac: fraction
	;mov edi,frac
    cmp stackSize,MAXSIZE
    jge pushError
    mov esi, OFFSET theStack
    movzx ebx,stackSize  ; set up index in ebx
    mov eax, frac
    mov fraction PTR [esi+4*ebx],eax   ; put eax fraction into stack
    inc stackSize ; one more item on stack
    ret
pushError:
    ccout "Tried to push into a full stack\n"
    ret   
stackPush ENDP
; Pushes a float on top of the stack
floatPush proc, float:real4
	local temp:fraction
    invoke float2frac,float,addr temp
	invoke stackPush,temp
    ret
floatPush endp


; Retrieves a fraction from the top of the stack
stackTop  PROC USES eax ebx esi edi, frac:ptr fraction
    cmp stackSize,0
    jle topError
    ; get fraction into eax
    mov esi, OFFSET theStack
	mov edi, frac
	
    movzx ebx, stackSize 
    dec ebx ; ebx now has the index of the top of the stack
	mov	eax, fraction PTR[esi+4*ebx] ; eax has the top of the stack
    mov fraction PTR [edi], eax ; puts fraction in specified location
    ret
topError:
    ccout "Tried to retrieve a fraction from an empty stack\n"
	mov (fraction ptr[edi]).numerator,0
	mov (fraction ptr[edi]).denominator,0
    ret
stackTop ENDP

floatTop proc, float:ptr real4
	local temp:fraction
	invoke stackTop,addr temp
	invoke frac2Float,temp,float
	ret
floatTop endp

; Retrieves a fraction from the top of the stack, puts the struct in ecx
topStack  PROC USES eax ebx esi edi   
    cmp stackSize,0
    jle topError2
    ; get fraction into eax
    mov esi, OFFSET theStack
    movzx ebx, stackSize 
    dec ebx ; ebx now has the index of the top of the stack
    mov ecx, fraction PTR [esi+4*ebx] ; eax has the top of the stack
    ret
topError2:
    ccout "Tried to retrieve a fraction from an empty stack\n"
	mov ecx,dummyFrac
    ret
topStack ENDP

topFloat proc
	local temp:fraction, float:real4
	call topStack
	mov temp,ecx
	invoke frac2Float,temp, addr float
	mov ecx,float
	ret
topFloat endp
; Removes the top element of an not empty stack
stackPop PROC
    cmp stackSize,0
    jle popError
    dec stackSize
    ret
popError:
    ccout "Tried to pop empty stack\n"
    ret
stackPop ENDP  

; TRUE if stack is empty
stackEmpty PROC
    cmp stackSize,0
    je  emptyStack
    mov eax,FALSE
    ret
emptyStack:
    mov eax,TRUE
    ret
stackEmpty ENDP

; TRUE if stack is full
stackFull PROC
    cmp stackSize,MAXSIZE
    je  fullStack
    mov eax,FALSE
    ret
fullStack:
    mov eax,TRUE
    ret
stackFull ENDP

printStack proc uses eax ebx ecx edi
local temp:fraction
	ccout "\n=========\nSTACK:   \n---------\n"
	mov edi,offset theStack
	mov ecx,0
	stackLoop:
		cmp cx,stackSize
		jge stackLoopDone
		movsx eax,(fraction ptr[edi+4*ecx]).numerator
		push ecx
			ccout str$(eax)
			ccout "/"
		pop ecx
			movsx eax,(fraction ptr[edi+4*ecx]).denominator
		push ecx
			ccout str$(eax)
			ccout "\n---------\n"
		pop ecx
		inc ecx
		jmp stackLoop
	stackLoopDone:
		ccout "TOP\n=========	\n"
		ret
printStack endp

fracs2Reals proc

fracs2Reals endp
END ; end of stack functions