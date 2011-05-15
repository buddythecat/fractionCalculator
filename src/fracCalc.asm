; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««
; Main file for fraction calculator
; defines the user interface
; Original Code copyright: David B. Sher 2011
; Modifications: Rich "Dances With Caterpillars" 2011
; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
		include \masm32\include\masm32rt.inc
		include stack.inc
		include calcDisplay.inc
		include fraction.inc
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
.data
		hDlg				DWORD		0
		hInstance			DWORD		0
		hEditReal			DWORD		0
		hEditNum			DWORD		0
		hEditDen			DWORD		0
		bFracMode			DWORD		0
		bRealMode			DWORD		0
		numBuffer			BYTE		15 dup(0)
		denBuffer			BYTE		15 dup(0)
		realBuffer			BYTE		32 dup(0)
		isRealMode			BYTE		FALSE
		frac 				fraction	<?,?>	;fraction who should hold input from the main window
		fTemp 				fraction 	<?,?> 	;temporary fraction for retriving from the stack
		result 				fraction 	<?,?>	;fraction to store results from any arithmatic computations
		tFloat 				real4		0.0
.code
CalculatorManager proc hWin			:DWORD,
						uMsg		:DWORD,
						wParam		:DWORD,
						lParam		:DWORD
		cmp uMsg, WM_INITDIALOG
		jne		checkForCommand
		push	hWin
		pop		hDlg	;save handle in a global variable
						;this avoids having to pass it as a
						;parameter whenever needed outside this proc
		invoke 	SendDlgItemMessage,hDlg,FRACCALC,EM_SETLIMITTEXT,1024,0
		invoke 	SetDlgItemText,hDlg,NUMERATOR,chr$("0") ; initial value for numerator
		invoke 	SetDlgItemText,hDlg,DENOMINATOR,chr$("1") ; initial value for numerator
		invoke 	GetDlgItem,hDlg,REALEDITOR
		mov 	hEditReal,	eax
		invoke 	GetDlgItem,hDlg,NUMERATOR
		mov 	hEditNum,	eax
		invoke 	GetDlgItem,hDlg,DENOMINATOR
		mov 	hEditDen,	eax
		invoke 	GetDlgItem,hDlg,FRACMODE
		mov 	bFracMode,	eax
		invoke 	GetDlgItem,hDlg,REALMODE
		mov 	bRealMode,	eax
		return TRUE
	checkForCommand:
		cmp 	uMsg,	WM_COMMAND
		jne 	checkForClose
		mov		eax,wParam
		and		eax,0ffffh
	checkForClose:
		cmp		uMsg,WM_CLOSE
		jne		checkForENTER
		invoke	EndDialog,hDlg,0
	checkForENTER:           ; clicked the Enter button
		cmp 	eax,ENTERBUTTON
		jne 	checkForPOP
		; Push fraction on stack
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		fractionPush
		realPush:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			ccout 	"pushing float: "
			ccout 	real4$(tFloat)
			ccout 	" to stack\n"
			invoke 	float2frac,tFloat,offset frac
			jmp 	enterPush
		fractionPush:
			cls
			ccout 	"Pushing:  "
			invoke 	GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			ccout 	OFFSET numBuffer
			ccout 	"/"
			invoke 	GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			ccout 	OFFSET denBuffer
			ccout 	"\n"
			invoke 	stackFull ; don't push if stack is full
			cmp 	eax,TRUE
			je 		fullEnter
			mov 	eax,sval(OFFSET numBuffer)
			mov 	frac.numerator,ax
			mov 	eax,sval(OFFSET denBuffer)
			mov 	frac.denominator,ax
		enterPush:
			invoke 	SetDlgItemText,hDlg,REALEDITOR,chr$("0.0")
			invoke 	printToConsole,frac
			invoke 	stackPush,frac
			invoke 	printStack
			return 	TRUE
		fullEnter:
			ccout 	"	Can't enter because stack is full\n"       
			return 		TRUE
	checkForPOP:
		cmp 	eax,POPBUTTON
		jne 	checkForADD
		;check calculator mode
		cls
		ccout 	"Popping: "
		cmp 	eax,TRUE
		je 		popEmpty
		invoke 	topStack
		mov 	frac,ecx
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		fractionPop
		realPop:
			mov ax,frac.denominator
			cmp ax,0
			jne @F
			mov frac.denominator,1
			mov frac.numerator,0
			@@:
			invoke 	frac2Float,frac,offset tFloat
			invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
			jmp 	popComplete
		fractionPop:
			; Pop the fraction on the screen
			ccout 	str$(frac.numerator)
			ccout 	"/"
			ccout 	str$(frac.denominator)
			ccout 	"\n"
			invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(frac.numerator)
			invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(frac.denominator)
		popComplete:
			invoke 	stackPop
			invoke 	printStack
			return 	TRUE
		popEmpty:
			invoke 	SetDlgItemText,hDlg,REALEDITOR,chr$("0.0")
			invoke 	SetDlgItemText,hDlg,NUMERATOR,chr$("0") ; initial value for numerator
			invoke 	SetDlgItemText,hDlg,DENOMINATOR,chr$("1") ; initial value for numerator
			ccout 	"Nothing on stack to pop"
			return 	TRUE
	checkForADD:
		cmp 	eax,ADDBUTTON
		jne 	checkForSUBTRACT
		;if add was pressed:
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		addFraction
		realAdd:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			invoke 	float2frac,tFloat,offset frac
			jmp 	readyToAdd
		addFraction:
			;1) Pull numerator and denom into a temp fraction
			invoke 	GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			invoke 	GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			mov 	eax,sval(OFFSET numBuffer)
			push 	eax
			mov 	eax,sval(OFFSET denBuffer)
			mov 	frac.denominator,ax
			pop 	eax
			mov 	frac.numerator,ax
		readyToAdd:
			;2) Make sure there is a fraction in the stack to ADD TO
			;	if the stack is empty: just push the current fraction
			mov eax,0
			invoke 	stackEmpty
			cmp		eax,TRUE
			jne 	canAdd
			;3) Pop top into a temp
			invoke 	stackPush,frac
			jmp 	doneAdd
		canAdd:
			invoke 	stackTop,offset fTemp
			cls
			invoke 	printToConsole,fTemp
			ccout 	" + "
			invoke 	printToConsole,frac
			ccout 	" = "
			invoke 	stackPop
			;4) Add new input to stack top
			invoke 	addFrac,fTemp,frac,addr result
			invoke 	printToConsole,result
			ccout 	"\n"
			;5) Push the new top b ack onto the stack
			invoke 	stackPush,result		;should not be full, being we just popped before we added
			;move the new value into the window
			invoke frac2Float,result,offset tFloat
			invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
			invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(result.numerator) ; initial value for numerator
			invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(result.denominator) ; initial value for numerator
			doneAdd:
				invoke printStack
		return TRUE
	checkForSUBTRACT:
		cmp 	eax,SUBTRACTBUTTON
		jne 	checkForMULTIPLY
		; Subtract the fraction from the top of the stack
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		subFraction
		subReal:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			invoke 	float2frac,tFloat,offset frac
			jmp 	readyToSub
		subFraction:
			;1) Pull numerator and denom into a temp fraction
			invoke GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			invoke GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			mov eax,sval(OFFSET numBuffer)
			push eax
			mov eax,sval(OFFSET denBuffer)

			mov frac.denominator,ax
			pop eax
			mov frac.numerator,ax
		readyToSub:
			;2) Make sure there is a fraction in the stack to ADD TO
			;if the stack is empty: subtract the value from 0 (negate it)
			invoke stackEmpty
			cmp eax,TRUE
			jne @F
			mov fTemp.numerator,0
			mov fTemp.denominator,0
			jmp subThem
			@@:
				;3) Pop top into a temp
				invoke stackTop,offset fTemp
				cls
				invoke printToConsole,fTemp
				ccout " - "
				invoke printToConsole,frac
				ccout " = "
				invoke stackPop
			subThem:
				;4) Add new input to stack top
				invoke subFrac,fTemp,frac,addr result
				invoke printToConsole,result
				ccout "\n"
			;5) Push the new top b ack onto the stack
			invoke stackPush,result		;should not be full, being we just popped before we added
		invoke printStack
		invoke frac2Float,result,offset tFloat
		invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
		invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(result.numerator) ; initial value for numerator
		invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(result.denominator) ; initial value for numerator
		return TRUE
	checkForMULTIPLY:
		cmp eax,MULTIPLYBUTTON
		jne checkForDIVIDE
		; Multiply the fraction with the top of the stack
		;check the mode
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		mulFraction
		mulReals:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			invoke 	float2frac,tFloat,offset frac
			jmp 	readyToMul
		mulFraction:
			;1) Pull numerator and denom into a temp fraction
			invoke GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			invoke GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			mov eax,sval(OFFSET numBuffer)
			push eax
			mov eax,sval(OFFSET denBuffer)
			mov frac.denominator,ax
			pop eax
			mov frac.numerator,ax
		readyToMul:
			;2) Make sure there is a fraction in the stack to multiply TO
			;	if the stack is empty: just push the current fraction
			invoke stackEmpty
			cmp eax,TRUE			;if the stack is empty, jump over the stackTop call
			je @F
			;3) Pop top into a temp
			invoke stackTop,offset fTemp
			jmp doMult
			@@:
				mov fTemp.numerator,1
				mov fTemp.denominator,1
			doMult:
				cls
				invoke printToConsole,fTemp
				ccout " * "
				invoke printToConsole,frac
				ccout " = "
				invoke stackPop
				;4) Add new input to stack top
				invoke mulFrac,fTemp,frac,addr result
			invoke printToConsole,result
			ccout "\n"
			;5) Push the new top b ack onto the stack
			invoke stackPush,result		;should not be full, being we just popped before we added	   
		invoke printStack
		invoke frac2Float,result,offset tFloat
		invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
		invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(result.numerator) ; initial value for numerator
		invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(result.denominator) ; initial value for numerator
		return TRUE
	checkForDIVIDE:
		cmp eax,DIVIDEBUTTON
		jne checkForSQRTBUTTON
		; Divide the fraction from the top of the stack
		mov 	al,isRealMode
		cmp 	al,FALSE
		je 		divFraction
		divReal:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			invoke 	float2frac,tFloat,offset frac
			jmp 	readyToDiv
		divFraction:
			;1) Pull numerator and denom into a temp fraction
			invoke GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			invoke GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			mov eax,sval(OFFSET numBuffer)
			push eax
			mov eax,sval(OFFSET denBuffer)

			mov frac.denominator,ax
			pop eax
			mov frac.numerator,ax
		readyToDiv:
			;2) Make sure there is a fraction in the stack to ADD TO
			;	if the stack is empty: just push the current fraction
			invoke stackEmpty
			cmp eax,TRUE
			je @F
			;3) Pop top into a temp
			invoke stackTop,offset fTemp
			cls
			invoke printToConsole,fTemp
			ccout " / "
			invoke printToConsole,frac
			ccout " = "
			invoke stackPop
			jmp doDivision
		@@:
			mov fTemp.numerator,1
			mov fTemp.denominator,1
		doDivision:
			;4) Add new input to stack top
			invoke divFrac,fTemp,frac,addr result
			invoke printToConsole,result
			ccout "\n"
			;5) Push the new top b ack onto the stack
		invoke stackPush,result		;should not be full, being we just popped before we added
		invoke printStack
		invoke frac2Float,result,offset tFloat
		invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
		invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(result.numerator) ; initial value for numerator
		invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(result.denominator) ; initial value for numerator
		return TRUE 
	checkForSQRTBUTTON:
		cmp eax,SQRTBUTTON
		jne checkForREALMODE
		ccout "here"
		mov al,isRealMode
		cmp al,FALSE
		je sqrtFracLbl
		sqrtReal:
			invoke 	GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
			mov 	edi,a2r4(addr realBuffer)
			mov 	eax,real4 ptr[edi]
			mov 	tFloat,eax
			invoke 	float2frac,tFloat,offset frac
			jmp 	readyToSqrt
		sqrtFracLbl:
			invoke GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
			invoke GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
			mov eax,sval(OFFSET numBuffer)
			push eax
			mov eax,sval(OFFSET denBuffer)

			mov frac.denominator,ax
			pop eax
			mov frac.numerator,ax
		readyToSqrt:
			invoke sqrtRealFrac,frac,addr result
			invoke frac2Float,result,offset tFloat
			invoke 	SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
			invoke 	SetDlgItemText,hDlg,NUMERATOR,str$(result.numerator) ; initial value for numerator
			invoke 	SetDlgItemText,hDlg,DENOMINATOR,str$(result.denominator) ; initial value for numerator
		return TRUE
	checkForREALMODE:
		cmp eax,REALMODE
		jne checkForFRACMODE
		mov isRealMode,TRUE
		invoke EnableWindow,hEditReal, TRUE
		invoke ShowWindow,hEditReal,TRUE
		invoke EnableWindow,hEditNum, FALSE
		invoke ShowWindow,hEditNum,FALSE
		invoke EnableWindow,hEditDen, FALSE
		invoke ShowWindow,hEditDen,FALSE
		invoke ShowWindow,bFracMode,TRUE
		invoke EnableWindow,bFracMode,TRUE
		invoke ShowWindow,bRealMode,FALSE
		invoke EnableWindow,bRealMode,FALSE
		invoke GetDlgItemText,hDlg,NUMERATOR,ADDR numBuffer,10
		invoke GetDlgItemText,hDlg,DENOMINATOR,ADDR denBuffer,10
		mov eax,sval(OFFSET numBuffer)
		push eax
		mov eax,sval(OFFSET denBuffer)
		mov frac.denominator,ax
		pop eax
		mov frac.numerator,ax
		
		mov ax,frac.denominator
		cmp ax,0
		jne @F
		mov frac.denominator,1
		mov frac.numerator,0
		@@:
		invoke frac2Float,frac,offset tFloat
		invoke SetDlgItemText,hDlg,REALEDITOR,real4$(tFloat)
		return TRUE
	checkForFRACMODE:
		cmp eax,FRACMODE
		jne otherMessage
		mov isRealMode,FALSE
		invoke EnableWindow,hEditReal, FALSE
		invoke ShowWindow,hEditReal,FALSE
		invoke EnableWindow,hEditNum, TRUE
		invoke ShowWindow,hEditNum,TRUE
		invoke EnableWindow,hEditDen, TRUE
		invoke ShowWindow,hEditDen,TRUE
		invoke ShowWindow,bFracMode,FALSE
		invoke EnableWindow,bFracMode,FALSE
		invoke ShowWindow,bRealMode,TRUE
		invoke EnableWindow,bRealMode,TRUE
		invoke GetDlgItemText,hDlg,REALEDITOR,ADDR realBuffer,10
		mov edi,a2r4(addr realBuffer)
		mov eax,real4 ptr[edi]
		mov tFloat,eax
		invoke float2frac,tFloat,offset frac
		invoke SetDlgItemText,hDlg,NUMERATOR,str$(frac.numerator)
		invoke SetDlgItemText,hDlg,DENOMINATOR,str$(frac.denominator)
		return TRUE
	otherMessage:
		return FALSE        ;use Windows defaults to handle other messages
CalculatorManager endp
start:
	invoke GetModuleHandle,NULL
	mov    hInstance,eax
	invoke InitCommonControls
	invoke DialogBoxParam,hInstance,FRACCALC,NULL,ADDR CalculatorManager,NULL
	invoke ExitProcess,eax
end start