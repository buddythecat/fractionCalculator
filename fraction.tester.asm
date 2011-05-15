;[Rich Tufano][2011.spring][NCC]													
title test module for fraction structure and prot/proc			(fraction.tester.asm)
include \masm32\include\masm32rt.inc
include fraction.inc
.data
	frac1 fraction <?,?>
	frac2 fraction <?,?>
	epsilon fraction <1,20>
	real real4 3.14
.code

main proc
	mov esi,a2r4(input("enter a floating point number:"))
	mov eax,real4 ptr[esi]
	mov real,eax
	ccout "converting to fraction...\n"
	ccout real4$(real)
	ccout "\n____\n"
	invoke float2frac,real,offset frac1
	invoke printToConsole,frac1
	ccout "\nGoing back to float:\n"
	invoke frac2Float,frac1,offset real
	ccout "result: "
	ccout real4$(real)
	ccout "\n"
	ret
main endp

start:
	call main
	inkey
	exit
end start