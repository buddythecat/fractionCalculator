;[Rich Tufano][2011.spring][NCC]													
title test module for fraction structure and prot/proc			(fraction.tester.asm)
include \masm32\include\masm32rt.inc
include fraction.32.inc
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
.data
	frac1 fraction32 <?,?>
	frac2 fraction32 <?,?>
	epsilon fraction32 <1,10>
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	code
;	_procs
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
.code
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	_getFracFromUser,:ptr fraction
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
getFracFromUser proc uses eax ebx ecx edx theFrac:ptr fraction32
	mov esi,theFrac
	mov eax,sval(input("Enter the numerator: "))
	mov (fraction32 ptr [esi]).numerator, eax
	mov eax,sval(input("Enter the denominator:"))
	mov (fraction32 ptr [esi]).denominator, eax
	ccout "------------------------\n"
	ret
getFracFromUser endp
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	_main
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
main proc
	local result:fraction32
	init:
		invoke getFracFromUser,offset frac1
		;invoke getFracFromUser,offset frac2
		;test printing
		;invoke printToConsole,frac1
		;ccout " & "
		;invoke printToConsole,frac2
	ccout "\n---------------\n"
	;test adding
		;
		;ccout "\nanswer: "
		
		invoke sqrtFrac32,frac1,epsilon,offset frac2
		invoke printToConsole32,frac2
	ret
main endp

start:
	call main
	inkey
	exit
end start
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	end code
;	end listing file
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл