(adds p0 r37 -132 r34)
(ld4 p0 r38 {r37})
(adds p0 r39 -144 r34)
(ld8 p0 r40 {r39})
(adds p0 r41 -136 r34)
(ld4 p0 r42 {r41})
(sxt4 p0 r43 r42)
(setf.sig p0 f6 r43)
(mov p0 r44 4)
(setf.sig p0 f7 r44)
(xmpy.l p0 f32 f6 f7)
(getf.sig p0 r45 f32)
(add p0 r46 r40 r45)
(ld4 p0 r47 {r46})
(add p0 r48 r38 r47)
(adds p0 r49 -132 r34)
(st4 p0 {r49} r48)
(adds p0 r50 -136 r34)
(ld4 p0 r51 {r50})
(adds p0 r52 1 r51)
(adds p0 r53 -136 r34)
(st4 p0 {r53} r52)

