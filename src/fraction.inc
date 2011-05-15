;[Rich Tufano][2011.spring][NCC]									
title fraction procedure listing					(fraction.inc)
;nclude \masm32\include\masm32rt.inc
;�������������������������������������������������������������������
;	-protocol listing for the fraction module:
;	_arithmatic_
;		-addFrac
;		-subFrac
;		-mulFrac
;		-divFrac
;	_manipulation_
;		-euclidianGCF
;		-toLowestTerms
;	�������������������������������������������������������������������
	;	__structure__
	;�������������������������������������������������������������������
	fraction struc
		numerator sword ?
		denominator sword ?
	fraction ends
	;�������������������������������������������������������������������
	;	__arithmatic__
	;�������������������������������������������������������������������
	addFrac proto :fraction, :fraction, :ptr fraction
	subFrac proto :fraction, :fraction, :ptr fraction
	mulFrac proto :fraction, :fraction, :ptr fraction
	divFrac proto :fraction, :fraction, :ptr fraction
	sqrtFrac proto :fraction, :fraction, :ptr fraction
	sqrtRealFrac proto :fraction, :ptr fraction
	cmpFrac proto :fraction, :fraction
	frac2Float proto :fraction, :ptr real4
	float2frac proto :real4, :ptr fraction
	;�������������������������������������������������������������������
	;	utilities
	;�������������������������������������������������������������������
;	euclidianGCF proto :sdword, :sdword, :ptr sdword
	toLowestTerms proto :ptr fraction
	printToConsole proto :fraction
;�������������������������������������������������������������������
;	end listing
;�������������������������������������������������������������������