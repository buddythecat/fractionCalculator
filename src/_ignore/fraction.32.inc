;[Rich Tufano][2011.spring][NCC]									
title fraction32 procedure listing					(fraction32.inc)
;nclude \masm32\include\masm32rt.inc
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	-protocol listing for the fraction32 module:
;	_arithmatic_
;		-addFrac
;		-subFrac
;		-mulFrac
;		-divFrac
;	_manipulation_
;		-euclidianGCF
;		-toLowestTerms
;	ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	;	__structure__
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	fraction32 struc
		numerator sdword ?
		denominator sdword ?
	fraction32 ends
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	;	__arithmatic__
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	addFrac32 proto :fraction32, :fraction32, :ptr fraction32
	subFrac32 proto :fraction32, :fraction32, :ptr fraction32
	mulFrac32 proto :fraction32, :fraction32, :ptr fraction32
	divFrac32 proto :fraction32, :fraction32, :ptr fraction32
	sqrtFrac32 proto :fraction32, :fraction32, :ptr fraction32
	truncate32 proto :ptr fraction32
	cmpFrac32 proto :fraction32, :fraction32
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
	;	utilities
	;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	euclidianGCF32 proto :sdword, :sdword, :ptr sdword
	toLowestTerms32 proto :ptr fraction32
	printToConsole32 proto :fraction32
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;	end listing
;ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл