(adds p0 r37 -40 r34)
(ld8 p0 r38 {r37})
(adds p0 r39 -136 r34)
(ld4 p0 r40 {r39})
(sxt4 p0 r41 r40)
(setf.sig p0 f6 r41)
(mov p0 r42 8)
(setf.sig p0 f7 r42)
(xmpy.l p0 f32 f6 f7)
(getf.sig p0 r43 f32)
(add p0 r44 r38 r43)
(ld8 p0 r45 {r44})
(mov p0 r55 r45)
