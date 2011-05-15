;[Rich Tufano][2011.spring][NCC]									
title 					(fraction32.asm)
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
include \masm32\include\masm32rt.inc				;[masm32 runtime libs,funcs,macros]
include fraction.32.inc
.code
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	addFrac32 proc uses eax ebx esi frac1:fraction32, frac2:fraction32, result:ptr fraction32
	;	addFrac,fraction321,fraction322,result ptr
	;	-adds two fraction32s together (fraction321 and fraction322) and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction321 - fraction32 structure
	;			fraction322 - fraction32 structure
	;			result - pointer to address of resulting fraction32 structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load the address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction32)]
		;calculate the new numerator[result.numer = (num1.numer*num2.denom)+(num2.numer*num1.denom)
		mov eax,frac1.numerator						;ax = frac1.numerator
		imul eax,frac2.denominator						;ax = frac1.numer*frac2.denom
		;ax ready to add
		
		mov ebx,frac2.numerator						;bx = frac2.numerator
		imul ebx,frac1.denominator					;bx = frac2.numer*frac1.denom
		;bx ready to add
		
		;hold the summed numerator in ax, ax=ax+bx
		add eax,ebx						;ax = frac1.num+frac2.num
		
		;since esi holds a 64bit fraction32 structure, we cast the pointer in esi as a 
		;fraction32, then access it's numerator using fraction32.numerator, and move ax in
		mov (fraction32 ptr [esi]).numerator,eax		;result.numerator = num1.numerator+num2.numerator
		
		;clear registers
		mov eax,0
		mov ebx,0
		
		;_set result's denominator [result.denom = (frac1.denom*frac2.denom)
		mov eax,frac1.denominator
		imul eax,frac2.denominator					;ax=(frac1.denom*frac2.denom)
		;move into result
		mov (fraction32 ptr [esi]).denominator,eax
		;!!!result now has the appropriate values for the numerator and denominator (though vulgar) -> cleanup
		invoke toLowestTerms32,result
		ret
	addFrac32 endp
	
	
	
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	subFrac32 proc uses eax ebx esi frac1:fraction32, frac2:fraction32, result:ptr fraction32
	;	subFrac,fraction321,fraction322,result ptr
	;	-subtracts fraction322 from fraction321 and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction321 - fraction32 structure
	;			fraction322 - fraction32 structure
	;			result - pointer to address of resulting fraction32 structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction32)]
		;calculate the new numerator[result.numer = (num1.numer*num2.denom)-(num2.numer*num1.denom)
		mov eax,frac1.numerator						;ax = frac1.numerator
		imul eax,frac2.denominator						;ax = frac1.numer*frac2.denom
		;ax ready to subtract
		
		mov ebx,frac2.numerator						;bx = frac2.numerator
		imul ebx,frac1.denominator					;bx = frac2.numer*frac1.denom
		;bx ready to subtract
		
		;hold the summed numerator in ax, ax=ax-bx
		sub eax,ebx						;ax = frac1.num-frac2.num
		
		;since esi holds a 64bit fraction32 structure, we cast the pointer in esi as a 
		;fraction32, then access it's numerator using fraction32.numerator, and move ax in
		mov (fraction32 ptr [esi]).numerator,eax		;result.numerator = num1.numerator+num2.numerator
		
		;clear registers
		mov eax,0
		mov ebx,0
		
		;_set result's denominator [result.denom = (frac1.denom*frac2.denom)
		mov eax,frac1.denominator
		imul eax,frac2.denominator					;ax=(frac1.denom*frac2.denom)
		;move into result
		mov (fraction32 ptr [esi]).denominator,eax
		;!!!result now has the appropriate values for the numerator and denominator (though vulgar)
		invoke toLowestTerms32,result
		ret
	subFrac32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	mulFrac32 proc uses eax esi frac1:fraction32, frac2:fraction32, result:ptr fraction32
	;	mulFrac,fraction321,fraction322,result ptr
	;	-multiplies fraction321 and fraction322 and stores the
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction321 - fraction32 structure
	;			fraction322 - fraction32 structure
	;			result - pointer to address of resulting fraction32 structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction32)]
		;arithmatic here is fairly simple. 
		;result.numerator=frac1.num*frac2.num AND result.denominator=frac1.den*frac2.den
		;_numerator
		mov eax,frac1.numerator						;ax=frac1.numer
		imul eax,frac2.numerator						;ax=frac1.numer*frac2.numer
		;result's numerator is ready
		mov (fraction32 ptr [esi]).numerator,eax
		;clear ax
		mov eax,0
		;_denominator
		mov eax,frac1.denominator					;ax=frac1.denom
		imul eax,frac2.denominator					;ax=frac1.denom*frac2.denom
		;results denominator is ready:
		mov (fraction32 ptr [esi]).denominator,eax
		;!!result now has the appropriate values for the numerator and denom, though not reduced
		invoke toLowestTerms32,result
		ret
	mulFrac32 endp
	
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	divFrac32 proc uses eax esi frac1:fraction32, frac2:fraction32, result:ptr fraction32
	;	divFrac,fraction321,fraction322,result ptr
	;	-divides fraction322 into fraction321 and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction321 - fraction32 structure
	;			fraction322 - fraction32 structure
	;			result - pointer to address of resulting fraction32 structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result fraction32 into esi reg.
		mov esi, result
		;arithmatic isn't too difficult: num1 * reciprocal(num2) -> (num1.numer/num1.denom)*(num2.denom/num2.numer)
		;result.numerator = (num1.numer*num2.denom) AND result.denom = (num1.denom*num2.numer)
		;@note to self: perhaps in these arith functions the subFrac and divFrac procs can be emptied and implemented using addFrac/mulFrac?
		;	-what about the pointer on esi?
		;_numerator
		mov eax,frac1.numerator
		imul eax,frac2.denominator
		;result's numer is ready to go
		mov (fraction32 ptr [esi]).numerator,eax
		
		;clear ax
		mov eax,0
		;_denominators
		mov eax,frac1.denominator
		imul eax,frac2.numerator
		;result's denom is ready to go:
		mov (fraction32 ptr [esi]).denominator, eax
		invoke toLowestTerms32,result
		ret
	divFrac32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	sqrtFrac32 proc frac1:fraction32, eps:fraction32, result:ptr fraction32
		local temp:fraction32, alpha:fraction32, two:fraction32, diff:fraction32
	;	sqrtFrac,fraction321,ptr result
	;	- This method is prone to OVERFLOW ERRORS, due to storing all of it's values
	;	in 16 bit registers, and the fact that these irrational fraction32s
	;	can get very large, very fast!
	;	I recommend an epsilon no larger then 1/10 or 1/20!
	;	- finds the square root of the fraction32 using the babylonian
	;	method.
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл	
		finit
		
		invoke toLowestTerms32,addr frac1
		mov eax,frac1.numerator
		cmp eax,0
		jle failure
		
		mov alpha.numerator,0
		mov alpha.denominator,0
		mov diff.numerator,0
		mov diff.denominator,0
		mov temp.numerator,1
		mov temp.denominator,1
		mov two.numerator,2
		mov two.denominator,1
		
		sqrtLoop:
			pushad
				invoke printToConsole32,temp
				ccout "  "
			popad

			mov eax,0
			mov ebx,0
			invoke divFrac32,frac1,temp,addr alpha			;alpha=n/r
			invoke addFrac32,alpha,temp,addr alpha			;apha=(n/r)+r
			invoke divFrac32,alpha,two,addr alpha				;alpha=(n/r)+r/2
			pushad
				invoke printToConsole32,alpha
				ccout "\n"
			popad
			
			invoke subFrac32,alpha,temp,addr diff			;diff = alpha-r
			mov ecx,diff.numerator
			cmp ecx,0
			jg @F
			neg diff.numerator
			@@:
						
			pushad
				invoke printToConsole32,diff
				ccout "\n"
			popad
			

			invoke subFrac32,diff,eps,addr diff				;diff=[(alpha-r)-epsilon] -> if diff <0, alpha-r<0
			
			mov eax,diff.numerator							;sign is always stored in numerator, therefor, check numerator
			cmp eax,0								
			jle done									;diff<=eps
			reLoop:
				mov ebx,alpha.numerator
				mov temp.numerator,ebx
				mov ebx,0
				mov ebx,alpha.denominator
				mov temp.denominator,ebx
				mov diff.numerator,0
				mov diff.denominator,0
				jmp sqrtLoop
			
		done:
			mov esi,result
			mov ebx,alpha.numerator
			mov (fraction32 ptr[esi]).numerator, ebx
			
			mov ebx,0
			
			mov ebx,alpha.denominator
			mov (fraction32 ptr[esi]).denominator, ebx
			invoke toLowestTerms32,esi
			ret
		failure:
			ccout "dont get here"
			mov (fraction32 ptr[esi]).numerator, 0
			mov (fraction32 ptr[esi]).denominator,0
		ret
	sqrtFrac32 endp
	
	truncate32 proc frac:ptr fraction32
		mov esi,frac
		mov eax,(fraction32 ptr[esi]).numerator
		cmp eax,4095
		jl @F
		sar eax,1
		and al,1100b
		mov (fraction32 ptr[esi]).numerator,eax
		@@:
		mov eax,(fraction32 ptr[esi]).denominator
		cmp eax,4095
		jl @F
		sar eax,1
		and al,1100b
		mov eax,(fraction32 ptr[esi]).denominator
		@@:
		ret
	truncate32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	cmpFrac32 proc frac1:fraction32, frac2:fraction32
		local temp1:fraction32, temp2:fraction32, diff:sdword
	;	this version can ONlY DO 16 BIT ARITH! BEWARE OF OVERFLOWS!
	;	compares two fraction32s, and returns the result in eax.
	;	if frac1>frac2, eax>0
	;	if frac1<frac2, eax<0
	;	if frac1=frac2, eax=0
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov eax,0
		mov eax,frac1.numerator							;ax = frac1.numerator
		imul eax,frac2.denominator						;ax = frac1.numer*frac2.denom
		cmp eax,0
		jge continue
		neg eax
		;ax ready to subtract
		continue:
		mov ebx,frac2.numerator							;bx = frac2.numerator
		imul ebx,frac1.denominator						;bx = frac2.numer*frac1.denom
		cmp ebx,0
		jge finish
		neg ebx
		;bx ready to subtract
		finish:
		sub eax,ebx										;ax = frac1.num-frac2.num
		;if ax is positive, frac1>frac2
		;if ax is negative, frac1<frac2
		;if ax is zero, frac1=frac2
		mov diff,eax
		;in ax -> difference between frac1 and frac2
		mov eax,diff
		ret
	cmpFrac32 endp
;:::[fraction32:utilities]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;	(_(general arithmatic procedures for the fraction32 structure)_)
;		I.	findFactor,num1:sword,num2:sword,sign:ptr sword
;		II.	signBalance,frac:fraction32,sign:word,factor:sword
;		III.reBalance,frac:ptr fraction32, sign:word
;		II.	toLowestTerms,frac:fraction32
;		III.printToConsole,frac:fraction32
;	(_(notes)_)
;		.result is a pointer to the fraction32 variable that will store the resulting
;		fraction32.
;		.none of these methods return anything.
;:::[END|fraction32:utilities]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	findFactor32 proc uses eax ebx edx esi num1:sdword, num2:sdword, factor:ptr sdword
	;	findFactor - accepts two swords and finds the lcm of them and
	;	stores it by using the factor pointer.
	;	-algorithim finds the lcm using the general design:
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	;	void findFactor(int a, int b, *factor){
	;		int n=0;
	;		while(b>0){
	;			if(b>=0){
	;				if(a>b)
	;					a=a-b;
	;				else{
	;					n=a;
	;					a=b;
	;					b=n%a
	;				}
	;			}
	;			else
	;				factor=a;
	;		}
	;	}
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	;		num1:numerator[a]
	;		num2:denominator[b]
	;		factor:ptr sdword[pointer to loc of factor memory]
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov eax,num1
		cmp eax,0
			jge @F
			neg eax
		
		@@:
		mov ebx,num2
		cmp ebx,0
			jge @F
			neg ebx
		
		@@:
		mov ecx,0										;ecx -> represents n
		whileLoop:										;while(b>0){
			cmp ebx,0
			jle done
			;if(a>b)
				cmp eax,ebx
				jg @F
			;else
				mov edx,0

				cdq
				idiv ebx

				mov eax,ebx
				mov ebx,edx
				jmp whileLoop
			@@:
				sub eax,ebx
				jmp whileLoop
		done:
			mov esi,factor
			mov (sdword ptr[esi]),eax
		ret
	findFactor32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	signBalance32 proc uses eax bx esi frac:ptr fraction32, sign:ptr word, factor:sdword
	local temp:sword
	;	signBalance - balances the sign of the fraction32, and adjusts the numerator
	;	and denominator of the fraction32 by the factor
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov esi,frac										;move pointer to this fraction32 into esi reg
		mov bx,0											;clear bx (will store the sign)
		numer:												;check sign of numerator
			mov eax,(fraction32 ptr [esi]).numerator			;move numerator into ax
			cmp eax,0										;if numerator is negative
			jl negateN										;negate the numerator
			jmp calcNumer									;if not skip to calculation
			
		negateN:											;_negate numerator
			;neg ax
			neg (fraction32 ptr [esi]).numerator				;numerator*-1
			inc bx											;increment sign
			jmp calcNumer									;calculate numerator
			
		calcNumer:											;_negate numerator
			mov eax,(fraction32 ptr [esi]).numerator			;move the numerator to ax
			cdq												;extend ax to dx
			idiv factor										;divide ax by dx (numerator/factor) -> new numerator
			mov (fraction32 ptr [esi]).numerator,eax			;move new numerator into the fraction32's numerator
			jmp denom										;do denominator
		
		denom:												;_denominator
			mov eax,(fraction32 ptr [esi]).denominator		;move denominator to ax
			cmp eax,0										;see if denominator is negative
			jl negateD										;if it is, negate the denominator
			jmp calcDenom									;if not, calculate the denominator
			
		negateD:											;_negate denominator
			neg eax											;denominator*-1
			mov (fraction32 ptr [esi]).denominator,eax		;move denominator into ax
			inc bx											;inc sign
			
		
		calcDenom:
			mov eax,(fraction32 ptr [esi]).denominator		;move denominator to ax
			cdq												;extend dneominator to ax:dx
			idiv factor										;denominator/factor
			mov (fraction32 ptr [esi]).denominator,eax		;move the new denominator into the fraction32
			jmp done										;goto done
		
		done:												;finish up
			mov esi,sign									;move sign into esi
			mov (word ptr [esi]),bx							;move sign value into pointer
		ret
	signBalance32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	reBalance32 proc uses eax bx esi frac:ptr fraction32, sign:ptr word
	;	reBalance - change fraction32 back to it's designated sign
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov esi,sign										;move pointer to esi register
		mov bx,(word ptr [esi])								;move actual sign into bx
		cmp bx,1											;is sign zero?
		jne done											;if it's 0, no need to negate
			mov esi,frac									;move frac pointer onto esi
			mov eax,(fraction32 ptr [esi]).numerator			;move the numerator into eax
			neg eax											;negate the numerator
			mov (fraction32 ptr [esi]).numerator, eax			;move the new numerator into the fraction32
		done:
			ret
	reBalance32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	toLowestTerms32 proc uses eax edx frac:ptr fraction32
		local factor:sdword, sign:word
	;	toLowestTerms,fraction32
	;	-uses euclidian/gcf algorithm (euclidianGCF) to find the gcf/lcm
	;	then calculates the new values for the numerator and denominator
	;	of THIS fraction32 and stores them.
	;	-uses euclidianGCF
	;		params:
	;			fraction32 - this fraction32
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;invoke euclidianGCF,frac.numerator,frac.denominator,addr factor
		mov esi,frac
		invoke findFactor32,(fraction32 ptr [esi]).numerator,(fraction32 ptr [esi]).denominator,addr factor
		invoke signBalance32,frac,addr sign,factor
		invoke reBalance32,frac,addr sign
		ret
	toLowestTerms32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	printToConsole32 proc uses eax ebx ecx edx frac:fraction32
	;	printFrac,fraction32
	;	-uses ccout to print out the fraction32 to the console in form:
	;		>>(numerator)/(denominator)
	;	-protects all general registers due to ccout calls possible registry
	;	thrashing.
	;		params:
	;			fraction32 - THIS fraction32 structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		ccout str$(frac.numerator)
		ccout "/"
		ccout str$(frac.denominator)
		ret
	printToConsole32 endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	
end
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	end code
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл