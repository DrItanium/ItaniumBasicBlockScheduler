
	;;	alloc r35=ar.pfs,25,23,0
	;;	mov r34=r12
	;;	adds r12=-176,r12
	;;      mov r37=r1
	;;	mov r36=b0
	;;	mov r38=r32
	;;	mov r39=r33
	;;	sxt4 r40=r38
	;;	adds r41=-48,r34
	;;	st8 [r41]=r40
	;;	adds r42=-40,r34
	;;	st8 [r42]=r39
	;;	adds r43=-48,r34
	;;	ld4 r44=[r43]
	;;	sxt4 r45=r44
	;;	      adds r46=-160,r34;;
	;;	st8 [r46]=r45
	;;	adds r47=-160,r34;;
	;;	ld8 r48=[r47];;
	;;	adds r49=-1,r48
	;;	adds r50=-152,r34;;
	;;	st8 [r50]=r49
	;;	adds r51=-160,r34
	;;	ld8 r52=[r51];;
	;;	setf.sig f6=r52
	;;	mov r53=4;;
	;;	setf.sig f7=r53
	;;	xmpy.l f32=f6,f7
	;;	getf.sig r54=f32
	;;	adds r20=-112,r34
	;;	st8 [r20]=r54
	;;	adds r20=-128,r34
	;;	st8 [r20]=r37
	;;	adds r20=-112,r34
	;;	ld8 r37=[r20];;
	;;	adds r38=15,r37
	;;	and r39=-16,r38;;
	;;	sub r12=r12,r39
	;;	mov r40=r12
	;;	adds r20=-104,r34
	;;	st8 [r20]=r40
	;;	adds r37=16,r12
	;;	adds r20=-104,r34;;
	;;	st8 [r20]=r37
	;;	adds r37=-144,r34
	;;	adds r20=-104,r34;;
	;;	ld8 r38=[r20];;
	;;	st8 [r37]=r38
	;;	adds r39=-136,r34;;
	;;	st4 [r39]=r0
	;;	adds r37=-136,r34;;
	;;	ld4 r38=[r37]
	;;	adds r39=-48,r34;;
	;;	ld4 r40=[r39];;
	;;	cmp4.lt p0,p8=r38,r40
	;;	mov r19=0;;
	;;	(p08) mov r19=1
	;;	adds r20=-96,r34
	;;	st8 [r20]=r19
	;;	(p08) br.cond.dptk.few 4000000000000bf0 <main+0x330>;;
	;;	adds r37=-40,r34;;
	;;	ld8 r38=[r37]
	;;	adds r39=-136,r34;;
	;;	ld4 r40=[r39];;
	;;	sxt4 r41=r40;;
	;;	setf.sig f6=r41
	;;	mov r42=8
	;;	setf.sig f7=r42
	;;	xmpy.l f32=f6,f7
	;;	getf.sig r43=f32;;
	;;	add r44=r38,r43
	;;	ld8 r45=[r44];;
	;;	mov r55=r45
	;;	br.call.sptk.few b0=4000000000000640 <_init+0x120>;;
	;;	adds r20=-128,r34;;
	;;	ld8 r37=[r20]
	;;	mov r1=r37
	;;	mov r38=r8
	;;	adds r39=-144,r34;;
	;;	ld8 r40=[r39]
	;;	adds r41=-136,r34;;
	;;	ld4 r42=[r41];;
	;;	sxt4 r43=r42;;
	;;	setf.sig f6=r43
	;;	mov r44=4
	;;	setf.sig f7=r44
	;;	xmpy.l f32=f6,f7
	;;	getf.sig r45=f32;;
	;;	add r46=r40,r45
	;;	st4 [r46]=r38
	;;	adds r47=-136,r34
	;;	ld4 r48=[r47];;
	;;	adds r49=1,r48
	;;	adds r50=-136,r34;;
	;;	st4 [r50]=r49
	;;	br.few 4000000000000a40 <main+0x180>;;
	;;	adds r37=-136,r34;;
	;;	ld4 r38=[r37]
	;;	adds r39=-48,r34;;
	;;	ld4 r40=[r39];;
	;;	cmp4.lt p8,p0=r38,r40
	;;	mov r19=0;;
	;;	(p08) mov r19=1
	;;	adds r20=-88,r34
	;;	st8 [r20]=r19
	;;	(p08) br.cond.dptk.few 4000000000000c10 <main+0x350>
	;;	br.few 4000000000000cc0 <main+0x400>;;
	;;	adds r37=-136,r34;;
	;;	st4 [r37]=r0
	;;	br.few 4000000000000bb0 <main+0x2f0>;;
	;;	adds r37=-132,r34;;
	;;	ld4 r38=[r37]
	;;	adds r39=-144,r34;;
	;;	ld8 r40=[r39]
	;;	adds r41=-136,r34
	;;	ld4 r42=[r41];;
	;;	sxt4 r43=r42;;
	;;	setf.sig f6=r43
	;;	mov r44=4
	;;	setf.sig f7=r44
	;;	xmpy.l f32=f6,f7
	;;	getf.sig r45=f32;;
	;;	add r46=r40,r45
	;;	ld4 r47=[r46];;
	;;	add r48=r38,r47
	;;	adds r49=-132,r34;;
	;;	st4 [r49]=r48
	;;	adds r50=-136,r34
	;;	ld4 r51=[r50];;
	;;	adds r52=1,r51
	;;	adds r53=-136,r34;;
	;;	st4 [r53]=r52
	;;	br.few 4000000000000bb0 <main+0x2f0>;;
	;;	addl r37=388,r1
	;;	adds r38=-132,r34
;	(ld4 p0 r39 [r38])
;	(mov p0 r66 r37)
;	(mov p0 r56 r39)
;	(br.call.sptk.few p0 b0 40000000000005e0)
;	(adds p0 r20 -128 r34)
;	(ld8 p0 r37 [r20])
;	(mov p0 r1 r37)
;	(adds p0 r38 -144 r34)
;	(ld8 p0 r39 [r38])
;	(adds p0 r40 -160 r34)
;	(ld8 p0 r41 [r40])
;	(setf.sig p0 f6 r41)
;	(mov p0 r42 4)
;	(setf.sig p0 f7 r42)
;	(xmpy.l p0 f32 f6 f7)
;	(getf.sig p0 r43 f32)
;	(adds p0 r44 15 r43)
;	(and-inst p0 r45 -16 r44)
;	(add p0 r12 r12 r45)
;	(mov p0 r37 r0)
;	(mov p0 b0 r36)
;	(mov p0 ar.pfs r35)
;	(mov p0 r8 r37)
;	(mov p0 r12 r34)
;	(br.ret.sptk.few p0 b0)

	;; Figure out how to load these operations into a single basic block

