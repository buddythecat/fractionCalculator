;[Rich Tufano][2011.spring][NCC]									
title 					(fraction.asm)
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
include \masm32\include\masm32rt.inc				;[masm32 runtime libs,funcs,macros]
;	.model 486, stdcall
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;::fraction.inc -> protocol and structure definitions
;:::[fraction:structure]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
include fraction.inc
;		fraction<numerator, denominator> -
;		-> creates a fraction with a set numerator and denominator.
;		data:
;			numerator[sword] - fraction's numerator
;			denominator[sword] - fraction's denominator
;		structure:
;			32-bits wide
;			===================================================================
;			|   fraction.numerator:sword     |  fraction.denominator:sword    |
;			===================================================================
;			|<-[0]                           |<-[15]                          |<-[31]
;		notes:
;			after being reduced to a non-vulgar form, the fraction's sign should 
;			be stored in the first bit of the fraction -- only the numerator should
;			store sign.
;:::[END|fraction:structure]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

.code

;:::[fraction:arithmatic]:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;	(_(general arithmatic procedures for the fraction structure)_)
;		I	addFrac,frac1:fraction,frac2:fraction,result:ptr fraction
;		II 	subFrac,frac1:fraction,frac2:fraction,result:ptr fraction
;		III	mulFrac,frac1:fraction,frac2:fraction,result:ptr fraction
;		IV	divFrac,frac1:fraction,frac2:fraction,result:ptr fraction
;	(_(notes)_)
;		.result is a pointer to the fraction variable that will store the resulting
;		fraction.
;		.none of these methods return anything.
;:::[END|fraction:arithmatic]:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	addFrac proc uses ax bx esi frac1:fraction, frac2:fraction, result:ptr fraction
	;	addFrac,fraction1,fraction2,result ptr
	;	-adds two fractions together (fraction1 and fraction2) and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction1 - fraction structure
	;			fraction2 - fraction structure
	;			result - pointer to address of resulting fraction structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load the address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction)]
		;calculate the new numerator[result.numer = (num1.numer*num2.denom)+(num2.numer*num1.denom)
		mov ax,frac1.numerator						;ax = frac1.numerator
		imul ax,frac2.denominator						;ax = frac1.numer*frac2.denom
		;ax ready to add
		
		mov bx,frac2.numerator						;bx = frac2.numerator
		imul bx,frac1.denominator					;bx = frac2.numer*frac1.denom
		;bx ready to add
		
		;hold the summed numerator in ax, ax=ax+bx
		add ax,bx						;ax = frac1.num+frac2.num
		
		;since esi holds a 64bit fraction structure, we cast the pointer in esi as a 
		;fraction, then access it's numerator using fraction.numerator, and move ax in
		mov (fraction ptr [esi]).numerator,ax		;result.numerator = num1.numerator+num2.numerator
		
		;clear registers
		mov ax,0
		mov bx,0
		
		;_set result's denominator [result.denom = (frac1.denom*frac2.denom)
		mov ax,frac1.denominator
		imul ax,frac2.denominator					;ax=(frac1.denom*frac2.denom)
		;move into result
		mov (fraction ptr [esi]).denominator,ax
		;!!!result now has the appropriate values for the numerator and denominator (though vulgar) -> cleanup
		invoke toLowestTerms,result
		ret
	addFrac endp
	
	
	
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	subFrac proc uses ax bx esi frac1:fraction, frac2:fraction, result:ptr fraction
	;	subFrac,fraction1,fraction2,result ptr
	;	-subtracts fraction2 from fraction1 and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction1 - fraction structure
	;			fraction2 - fraction structure
	;			result - pointer to address of resulting fraction structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction)]
		;calculate the new numerator[result.numer = (num1.numer*num2.denom)-(num2.numer*num1.denom)
		mov ax,frac1.numerator						;ax = frac1.numerator
		imul ax,frac2.denominator						;ax = frac1.numer*frac2.denom
		;ax ready to subtract
		
		mov bx,frac2.numerator						;bx = frac2.numerator
		imul bx,frac1.denominator					;bx = frac2.numer*frac1.denom
		;bx ready to subtract
		
		;hold the summed numerator in ax, ax=ax-bx
		sub ax,bx						;ax = frac1.num-frac2.num
		
		;since esi holds a 64bit fraction structure, we cast the pointer in esi as a 
		;fraction, then access it's numerator using fraction.numerator, and move ax in
		mov (fraction ptr [esi]).numerator,ax		;result.numerator = num1.numerator+num2.numerator
		
		;clear registers
		mov ax,0
		mov bx,0
		
		;_set result's denominator [result.denom = (frac1.denom*frac2.denom)
		mov ax,frac1.denominator
		imul ax,frac2.denominator					;ax=(frac1.denom*frac2.denom)
		;move into result
		mov (fraction ptr [esi]).denominator,ax
		;!!!result now has the appropriate values for the numerator and denominator (though vulgar)
		invoke toLowestTerms,result
		ret
	subFrac endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	mulFrac proc uses ax esi frac1:fraction, frac2:fraction, result:ptr fraction
	;	mulFrac,fraction1,fraction2,result ptr
	;	-multiplies fraction1 and fraction2 and stores the
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction1 - fraction structure
	;			fraction2 - fraction structure
	;			result - pointer to address of resulting fraction structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result into the esi register
		mov esi,result								;ebi = PTR[result(_fraction)]
		;arithmatic here is fairly simple. 
		;result.numerator=frac1.num*frac2.num AND result.denominator=frac1.den*frac2.den
		;_numerator
		mov ax,frac1.numerator						;ax=frac1.numer
		imul ax,frac2.numerator						;ax=frac1.numer*frac2.numer
		;result's numerator is ready
		mov (fraction ptr [esi]).numerator,ax
		;clear ax
		mov ax,0
		;_denominator
		mov ax,frac1.denominator					;ax=frac1.denom
		imul ax,frac2.denominator					;ax=frac1.denom*frac2.denom
		;results denominator is ready:
		mov (fraction ptr [esi]).denominator,ax
		;!!result now has the appropriate values for the numerator and denom, though not reduced
		invoke toLowestTerms,result
		ret
	mulFrac endp
	
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	divFrac proc uses ax esi frac1:fraction, frac2:fraction, result:ptr fraction
	;	divFrac,fraction1,fraction2,result ptr
	;	-divides fraction2 into fraction1 and stores
	;	the result at the address specified in the result pointer.
	;		params:
	;			fraction1 - fraction structure
	;			fraction2 - fraction structure
	;			result - pointer to address of resulting fraction structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;load address of result fraction into esi reg.
		mov esi, result
		;arithmatic isn't too difficult: num1 * reciprocal(num2) -> (num1.numer/num1.denom)*(num2.denom/num2.numer)
		;result.numerator = (num1.numer*num2.denom) AND result.denom = (num1.denom*num2.numer)
		;@note to self: perhaps in these arith functions the subFrac and divFrac procs can be emptied and implemented using addFrac/mulFrac?
		;	-what about the pointer on esi?
		;_numerator
		mov ax,frac1.numerator
		imul ax,frac2.denominator
		;result's numer is ready to go
		mov (fraction ptr [esi]).numerator,ax
		
		;clear ax
		mov ax,0
		;_denominators
		mov ax,frac1.denominator
		imul ax,frac2.numerator
		;result's denom is ready to go:
		mov (fraction ptr [esi]).denominator, ax
		invoke toLowestTerms,result
		ret
	divFrac endp
	sqrtRealFrac proc frac:fraction, result:ptr fraction
		local alpha:real4, aPrime:real4, n:real4, diff:real4, two:real4, temp:real4
	.data
		epsReal real4 0.001
	.code
		invoke frac2Float,frac,addr n
		mov ecx,0
		finit
		;n = original fraction in a real format
		fld1
		fstp alpha
		fldz
		fstp aPrime
		fld1
		fld1
		fadd
		fstp two
		calculatePrime:
			inc ecx
			fld alpha
			fld n
			;ST0 = n, ST1=alpha 
			fdiv alpha
			;ST0=n/alpha ST1=alpha 
			fadd
			;ST0=alpha-n/alpha 
			fld two
			fdiv
			;ST0=alpha-n/alpha / 2
			fst aPrime
			pushad
				ccout real4$(aPrime)
				ccout " "
				ccout real4$(alpha)
				ccout "\n"
			popad
			compareToEps:
				fld aPrime
				fsub alpha
				;ST0 = aPrime-alpha
				fabs
				;ST0 = |aPrime-alpa|
				fstp temp
				pushad
					ccout real4$(temp)
					ccout " |difference| \n"
					ccout real4$(epsReal)
					ccout " epsilon\n"
				popad
				mov ax,0
				fld temp
				fcomp epsReal	;ST0=null
				fnstsw ax
				sahf		;if|aPrime-alpha|<eps -> finished
				jnae finished
				cmp ecx,10
				je finished
				;set alpha to aPrime, and aPrime to zero
				fld aPrime
				fstp alpha
				fldz
				fstp aPrime
			jmp calculatePrime
		finished:
			invoke float2frac,aPrime,result
		ret
	sqrtRealFrac endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	sqrtFrac proc frac1:fraction, eps:fraction, result:ptr fraction
		local temp:fraction, alpha:fraction, two:fraction, diff:fraction
	;	sqrtFrac,fraction1,ptr result
	;	- This method is prone to OVERFLOW ERRORS, due to storing all of it's values
	;	in 16 bit registers, and the fact that these irrational fractions
	;	can get very large, very fast!
	;	I recommend an epsilon no larger then 1/10 or 1/20!
	;	- finds the square root of the fraction using the babylonian
	;	method.
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл	
		finit
		mov esi,result
		mov temp.numerator,1
		mov temp.denominator,1
		mov two.numerator,2
		mov two.denominator,1
		
		sqrtLoop:
			mov ax,0
			mov bx,0
			invoke divFrac,frac1,temp,addr alpha			;alpha=n/r
			invoke addFrac,alpha,temp,addr alpha			;apha=(n/r)+r
			invoke divFrac,alpha,two,addr alpha				;alpha=(n/r)+r/2
			invoke subFrac,alpha,temp,addr diff			;diff = alpha-r
			invoke cmpFrac,diff,eps						;diff ? eps
			cmp ax,0								
			jle done									;diff<=eps
			reLoop:
				mov bx,alpha.numerator
				mov temp.numerator,bx
				mov bx,0
				mov bx,alpha.denominator
				mov temp.denominator,bx
				mov diff.numerator,0
				mov diff.denominator,0
				jmp sqrtLoop
			
		done:
			mov bx,alpha.numerator
			mov (fraction ptr[esi]).numerator, bx
			
			mov bx,0
			
			mov bx,alpha.denominator
			mov (fraction ptr[esi]).denominator, bx
			invoke toLowestTerms,result
		ret
	sqrtFrac endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	cmpFrac proc frac1:fraction, frac2:fraction
		local temp1:fraction, temp2:fraction, diff:sword
	;	this version can ONlY DO 16 BIT ARITH! BEWARE OF OVERFLOWS!
	;	compares two fractions, and returns the result in eax.
	;	if frac1>frac2, eax>0
	;	if frac1<frac2, eax<0
	;	if frac1=frac2, eax=0
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov eax,0
		mov ax,frac1.numerator							;ax = frac1.numerator
		imul ax,frac2.denominator						;ax = frac1.numer*frac2.denom
		cmp ax,0
		jg continue
		neg ax
		;ax ready to subtract
		continue:
		mov bx,frac2.numerator							;bx = frac2.numerator
		imul bx,frac1.denominator						;bx = frac2.numer*frac1.denom
		cmp bx,0
		jg finish
		neg bx
		;bx ready to subtract
		finish:
		sub ax,bx										;ax = frac1.num-frac2.num
		;if ax is positive, frac1>frac2
		;if ax is negative, frac1<frac2
		;if ax is zero, frac1=frac2
		mov diff,ax
		;in ax -> difference between frac1 and frac2
		mov ax,diff
		ret
	cmpFrac endp
;:::[fraction:utilities]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;	(_(general arithmatic procedures for the fraction structure)_)
;		I.	findFactor,num1:sword,num2:sword,sign:ptr sword
;		II.	signBalance,frac:fraction,sign:word,factor:sword
;		III.reBalance,frac:ptr fraction, sign:word
;		II.	toLowestTerms,frac:fraction
;		III.printToConsole,frac:fraction
;	(_(notes)_)
;		.result is a pointer to the fraction variable that will store the resulting
;		fraction.
;		.none of these methods return anything.
;:::[END|fraction:utilities]::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	findFactor proc uses eax ebx edx esi num1:sword, num2:sword, factor:ptr sword
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
	;		factor:ptr sword[pointer to loc of factor memory]
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov esi,factor									;store pointer in esi
		mov ax,num1										;num1 in ax
		mov bx,num2										;num2 in bx
		mov dx,0										;clear dx for division
		cmp ax,0										;check numerator for sign
		jg denominator									;skip if positive
			neg ax										;negate numerator(make positive)
		denominator:									;skipped-to?
		cmp bx,0										;is denominator positive?
		jg do											;if it is, go strait to loop
			neg bx										;if not, negate
		;ready to factor
		do:												;while(b>=0)
			cmp bx,0									;bx[b]vs[0]
			jle done									;ax = b(on last execution this is the lowest factor)
			cmp ax,bx									;if(a>b)
			jg reduce									;jump to reduction label
			;else (modulous ops)
			mov dx,0									;clear dx to get divisor
			idiv bx										;dx = a%b'ax = a' bx = b
			mov ax,bx									;ax=b
			mov bx,dx									;bx=a%b
			jmp do										;DO[WHILE]
		reduce:											;directive to reduce a by b
			sub ax,bx									;ax=ax-bx[a=a-b]
			jmp do										;DO[WHILE]
		done:
			mov (sword ptr [esi]),ax						;store a in [factor]
		ret
	findFactor endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	signBalance proc uses ax bx esi frac:ptr fraction, sign:ptr word, factor:sword
	local temp:sword
	;	signBalance - balances the sign of the fraction, and adjusts the numerator
	;	and denominator of the fraction by the factor
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov esi,frac										;move pointer to this fraction into esi reg
		mov bx,0											;clear bx (will store the sign)
		numer:												;check sign of numerator
			mov ax,(fraction ptr [esi]).numerator			;move numerator into ax
			cmp ax,0										;if numerator is negative
			jl negateN										;negate the numerator
			jmp calcNumer									;if not skip to calculation
			
		negateN:											;_negate numerator
			;neg ax
			neg (fraction ptr [esi]).numerator				;numerator*-1
			inc bx											;increment sign
			jmp calcNumer									;calculate numerator
			
		calcNumer:											;_negate numerator
			mov ax,(fraction ptr [esi]).numerator			;move the numerator to ax
			cwd												;extend ax to dx
			idiv factor										;divide ax by dx (numerator/factor) -> new numerator
			mov (fraction ptr [esi]).numerator,ax			;move new numerator into the fraction's numerator
			jmp denom										;do denominator
		
		denom:												;_denominator
			mov ax,(fraction ptr [esi]).denominator			;move denominator to ax
			cmp ax,0										;see if denominator is negative
			jl negateD										;if it is, negate the denominator
			jmp calcDenom									;if not, calculate the denominator
			
		negateD:											;_negate denominator
			neg ax											;denominator*-1
			mov (fraction ptr [esi]).denominator,ax			;move denominator into ax
			inc bx											;inc bx
			
		
		calcDenom:
			mov ax,(fraction ptr [esi]).denominator			;move denominator to ax
;			mov temp,ax
;			ccout str$(temp)
;			ccout str$(factor)
			cwd												;extend dneominator to ax:dx
			idiv factor										;denominator/factor
			mov (fraction ptr [esi]).denominator,ax			;move the new denominator into the fraction
			jmp done										;goto done
		
		done:												;finish up
			mov esi,sign									;move sign into esi
			mov (word ptr [esi]),bx							;move sign value into pointer
		ret
	signBalance endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	reBalance proc uses ax bx esi frac:ptr fraction, sign:ptr word
	;	reBalance - change fraction back to it's designated sign
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		mov esi,sign										;move pointer to esi register
		mov bx,(word ptr [esi])								;move actual sign into bx
		cmp bx,1											;is sign zero?
		jne done											;if it's 0, no need to negate
			mov esi,frac									;move frac pointer onto esi
			mov ax,(fraction ptr [esi]).numerator			;move the numerator into eax
			neg ax											;negate the numerator
			mov (fraction ptr [esi]).numerator, ax			;move the new numerator into the fraction
		done:
			ret
	reBalance endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	toLowestTerms proc uses ax dx frac:ptr fraction
		local factor:sword, sign:word
	;	toLowestTerms,fraction
	;	-uses euclidian/gcf algorithm (euclidianGCF) to find the gcf/lcm
	;	then calculates the new values for the numerator and denominator
	;	of THIS fraction and stores them.
	;	-uses euclidianGCF
	;		params:
	;			fraction - this fraction
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		;invoke euclidianGCF,frac.numerator,frac.denominator,addr factor
		mov esi,frac
		invoke findFactor,(fraction ptr [esi]).numerator,(fraction ptr [esi]).denominator,addr factor
		invoke signBalance, frac,addr sign,factor
		invoke reBalance,frac,addr sign
		ret
	toLowestTerms endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	printToConsole proc uses eax ebx ecx edx frac:fraction
	;	printFrac,fraction
	;	-uses ccout to print out the fraction to the console in form:
	;		>>(numerator)/(denominator)
	;	-protects all general registers due to ccout calls possible registry
	;	thrashing.
	;		params:
	;			fraction - THIS fraction structure
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		ccout str$(frac.numerator)
		ccout "/"
		ccout str$(frac.denominator)
		ret
	printToConsole endp
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	frac2Float proc uses eax frac:fraction, fResult:ptr real4
	;	toFloat-
	;		returns this fraction represented as a float.
	;		the value is returned on the top of the floating point stack
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		finit
		fild frac.numerator
		fidiv frac.denominator
		mov esi,fResult
		fstp (real4 ptr[esi])
		ret
	frac2Float endp
	
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	float2frac proc float:real4, aReturn:ptr fraction
	;
		local temp:fraction, alpha:fraction, one:fraction, aResult:fraction, d:real4, cwords:word, index:dword
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	.data
		delta real4 0.001
	.code
		finit
		;set the control words of the FPU
		;specifically, the RC field (bit 11 and 10) to 11 [truncate]
		fstcw cwords
		mov ax,cwords
		or    ax,0C00h			;0000 1100 0000 0000
		mov cwords,ax
		fldcw cwords
		;done setting cwords
		
		fld float
		fstp d
		mov ecx,0
		mov temp.numerator,0
		mov temp.denominator,1
		mov edx,temp
		push edx
		mov one.numerator,1
		mov one.denominator,1
		checkCounter:
			cmp ecx,10
			jge finished
		makeFractions:
			inc ecx
			fld d
			fist temp.numerator
			mov temp.denominator,1
			mov edx,temp
			push edx
			;edx has a fraction of n/1
		calcNewD:
			fisub temp.numerator
			fstp d
		checkFloat:
			push eax
			fld d
			fcomp delta
			fnstsw ax
			sahf
			;if d<delta -> basecase
			jna finished
			pop eax
		flipNewD:
			fld1
			fdiv d
			fstp d
			;ST(0)=1/(d-n)
			jmp checkCounter
		finished:
			mov index,ecx
			;clear result
			mov aResult.numerator,0
			mov aResult.denominator,1
			pop edx
			;condition if already finished..
			cmp ecx,1
			je done
			
			pop edx
			mov temp,edx
			invoke divFrac,one,temp,addr aResult
			;aResult = 1/N_n
			dec ecx
			mov index,ecx
			sumLoop:
				cmp ecx,1
				jle done
				pop edx
				mov alpha,edx
				;alpha = N_n-1/1
				invoke addFrac,alpha,aResult,addr temp
				;temp = (N[n-1]/1)+(1/N[n])
				invoke divFrac,one,temp,addr aResult
				;aResult = 1/(N[n-1])+(1/N[n])
				dec ecx
				mov index,ecx
				jmp sumLoop
		done:
			pop edx
			mov temp,edx
			invoke addFrac,temp,aResult,addr alpha
			mov edx,alpha
			mov esi,aReturn
			mov (fraction ptr[esi]),edx
		ret
	float2frac endp
end
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	end code
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл