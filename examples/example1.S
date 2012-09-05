<main>:
[MII]       alloc r35=ar.pfs,25,23,0
            mov r34=r12
            adds r12=-176,r12
[MMB]       mov r37=r1
            nop.m 0x0
            nop.b 0x0;;
[MII]       nop.m 0x0
            mov r36=b0
            mov r38=r32
[MMB]       mov r39=r33
            nop.m 0x0
            nop.b 0x0;;
[MMI]       nop.m 0x0
            nop.m 0x0
            sxt4 r40=r38
[MMI]       adds r41=-48,r34;;
            st8 [r41]=r40
            adds r42=-40,r34;;
[MIB]       st8 [r42]=r39
            adds r43=-48,r34
            nop.b 0x0;;
[MII]       ld4 r44=[r43]
            nop.i 0x0;;
            sxt4 r45=r44
[MMI]       adds r46=-160,r34;;
            st8 [r46]=r45
            adds r47=-160,r34;;
[MMI]       ld8 r48=[r47];;
            adds r49=-1,r48
            adds r50=-152,r34;;
[MIB]       st8 [r50]=r49
            adds r51=-160,r34
            nop.b 0x0;;
[MMI]       ld8 r52=[r51];;
            setf.sig f6=r52
            mov r53=4;;
[MIB]       setf.sig f7=r53
            nop.i 0x0
            nop.b 0x0;;
[MFI]       nop.m 0x0
            xmpy.l f32=f6,f7
            nop.i 0x0;;
[MIB]       getf.sig r54=f32
            adds r20=-112,r34
            nop.b 0x0;;
[MIB]       st8 [r20]=r54
            adds r20=-128,r34
            nop.b 0x0;;
[MIB]       st8 [r20]=r37
            adds r20=-112,r34
            nop.b 0x0;;
[MMI]       ld8 r37=[r20];;
            adds r38=15,r37
            nop.i 0x0;;
[MMI]       and r39=-16,r38;;
            sub r12=r12,r39
            nop.i 0x0;;
[MIB]       mov r40=r12
            adds r20=-104,r34
            nop.b 0x0;;
[MII]       st8 [r20]=r40
            adds r37=16,r12
            adds r20=-104,r34;;
[MII]       st8 [r20]=r37
            adds r37=-144,r34
            adds r20=-104,r34;;
[MMI]       ld8 r38=[r20];;
            st8 [r37]=r38
            adds r39=-136,r34;;
[MIB]       st4 [r39]=r0
            nop.i 0x0
            nop.b 0x0;;
[MMI]       adds r37=-136,r34;;
            ld4 r38=[r37]
            adds r39=-48,r34;;
[MMI]       ld4 r40=[r39];;
            cmp4.lt p0,p8=r38,r40
            mov r19=0;;
[MIB] (p08) mov r19=1
            adds r20=-96,r34
            nop.b 0x0;;
[MIB]       st8 [r20]=r19
            nop.i 0x0
      (p08) br.cond.dptk.few 4000000000000bf0 <main+0x330>;;
[MMI]       adds r37=-40,r34;;
            ld8 r38=[r37]
            adds r39=-136,r34;;
[MMI]       ld4 r40=[r39];;
            nop.m 0x0
            sxt4 r41=r40;;
[MIB]       setf.sig f6=r41
            mov r42=8
            nop.b 0x0;;
[MMI]       setf.sig f7=r42
            nop.m 0x0
            nop.i 0x0;;
[MFI]       nop.m 0x0
            xmpy.l f32=f6,f7
            nop.i 0x0;;
[MMI]       getf.sig r43=f32;;
            add r44=r38,r43
            nop.i 0x0;;
[MMI]       ld8 r45=[r44];;
            mov r55=r45
            nop.i 0x0
[MIB]       nop.m 0x0
            nop.i 0x0
            br.call.sptk.few b0=4000000000000640 <_init+0x120>;;
[MMI]       adds r20=-128,r34;;
            ld8 r37=[r20]
            nop.i 0x0;;
[MII]       mov r1=r37
            mov r38=r8
            nop.i 0x0
[MMI]       adds r39=-144,r34;;
            ld8 r40=[r39]
            adds r41=-136,r34;;
[MMI]       ld4 r42=[r41];;
            nop.m 0x0
            sxt4 r43=r42;;
[MIB]       setf.sig f6=r43
            mov r44=4
            nop.b 0x0;;
[MMI]       setf.sig f7=r44
            nop.m 0x0
            nop.i 0x0;;
[MFI]       nop.m 0x0
            xmpy.l f32=f6,f7
            nop.i 0x0;;
[MMI]       getf.sig r45=f32;;
            add r46=r40,r45
            nop.i 0x0;;
[MIB]       st4 [r46]=r38
            adds r47=-136,r34
            nop.b 0x0;;
[MMI]       ld4 r48=[r47];;
            adds r49=1,r48
            adds r50=-136,r34;;
[MIB]       st4 [r50]=r49
            nop.i 0x0
            br.few 4000000000000a40 <main+0x180>;;
[MMI]       adds r37=-136,r34;;
            ld4 r38=[r37]
            adds r39=-48,r34;;
[MMI]       ld4 r40=[r39];;
            cmp4.lt p8,p0=r38,r40
            mov r19=0;;
[MIB] (p08) mov r19=1
            adds r20=-88,r34
            nop.b 0x0;;
[MBB]       st8 [r20]=r19
      (p08) br.cond.dptk.few 4000000000000c10 <main+0x350>
            br.few 4000000000000cc0 <main+0x400>;;
[MMI]       adds r37=-136,r34;;
            st4 [r37]=r0
            nop.i 0x0
[MIB]       nop.m 0x0
            nop.i 0x0
            br.few 4000000000000bb0 <main+0x2f0>;;
[MMI]       adds r37=-132,r34;;
            ld4 r38=[r37]
            adds r39=-144,r34;;
[MIB]       ld8 r40=[r39]
            adds r41=-136,r34
            nop.b 0x0;;
[MMI]       ld4 r42=[r41];;
            nop.m 0x0
            sxt4 r43=r42;;
[MIB]       setf.sig f6=r43
            mov r44=4
            nop.b 0x0;;
[MMI]       setf.sig f7=r44
            nop.m 0x0
            nop.i 0x0;;
[MFI]       nop.m 0x0
            xmpy.l f32=f6,f7
            nop.i 0x0;;
[MMI]       getf.sig r45=f32;;
            add r46=r40,r45
            nop.i 0x0;;
[MMI]       ld4 r47=[r46];;
            add r48=r38,r47
            adds r49=-132,r34;;
[MIB]       st4 [r49]=r48
            adds r50=-136,r34
            nop.b 0x0;;
[MMI]       ld4 r51=[r50];;
            adds r52=1,r51
            adds r53=-136,r34;;
[MIB]       st4 [r53]=r52
            nop.i 0x0
            br.few 4000000000000bb0 <main+0x2f0>;;
[MIB]       addl r37=388,r1
            adds r38=-132,r34
            nop.b 0x0;;
[MIB]       ld4 r39=[r38]
            mov r55=r37
            nop.b 0x0;;
[MIB]       mov r56=r39
            nop.i 0x0
            br.call.sptk.few b0=40000000000005e0 <_init+0xc0>;;
[MMI]       adds r20=-128,r34;;
            ld8 r37=[r20]
            nop.i 0x0;;
[MIB]       mov r1=r37
            adds r38=-144,r34
            nop.b 0x0;;
[MIB]       ld8 r39=[r38]
            adds r40=-160,r34
            nop.b 0x0;;
[MMI]       ld8 r41=[r40];;
            setf.sig f6=r41
            mov r42=4;;
[MIB]       setf.sig f7=r42
            nop.i 0x0
            nop.b 0x0;;
[MFI]       nop.m 0x0
            xmpy.l f32=f6,f7
            nop.i 0x0;;
[MMI]       getf.sig r43=f32;;
            adds r44=15,r43
            nop.i 0x0;;
[MMI]       and r45=-16,r44;;
            add r12=r12,r45
            mov r37=r0;;
[MIB]       nop.m 0x0
            mov b0=r36
            nop.b 0x0;;
[MII]       nop.m 0x0
            mov.i ar.pfs=r35
            mov r8=r37
[MMB]       mov r12=r34
            nop.m 0x0
            br.ret.sptk.few b0;;

