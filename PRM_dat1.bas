! /////////////////////////////////////// 
! 
!           Permutation Matrix check
! 
!       (c) 2023 by Dietmar G. Schrausser
! 

INCLUDE "data1.prm"
in:
nmax=14
INPUT "n1", n1, 2
INPUT "n2", n2, 2
IF n1+n2 > nmax
 msg$="nmax="+INT$(nmax)
 DIALOG.MESSAGE "Input",msg$, msg
 GOTO in
ENDIF
IF n1>n2
 n0=n1:n1=n2:n2=n0 %// to n1<n2
ENDIF
!// P vectors start position
IF n1=1 & n2=2 THEN st=0
IF n1=1 & n2=3 THEN st=9
IF n1=2 & n2=2 THEN st=25
IF n1=1 & n2=4 THEN st=49
IF n1=2 & n2=3 THEN st=74
IF n1=1 & n2=5 THEN st=124
IF n1=2 & n2=4 THEN st=160
IF n1=3 & n2=3 THEN st=250
IF n1=1 & n2=6 THEN st=370
IF n1=2 & n2=5 THEN st=419
IF n1=3 & n2=4 THEN st=566
IF n1=1 & n2=7 THEN st=811
IF n1=2 & n2=6 THEN st=875
IF n1=3 & n2=5 THEN st=1099
IF n1=4 & n2=4 THEN st=1547
IF n1=1 & n2=8 THEN st=2107
IF n1=2 & n2=7 THEN st=2188
IF n1=3 & n2=6 THEN st=2512
IF n1=4 & n2=5 THEN st=3268
IF n1=1 & n2=9 THEN st=4402
IF n1=2 & n2=8 THEN st=4502
IF n1=3 & n2=7 THEN st=4952
IF n1=4 & n2=6 THEN st=6152
IF n1=5 & n2=5 THEN st=8252
IF n1=1 & n2=10 THEN st=10772
IF n1=2 & n2=9 THEN st=10893
IF n1=3 & n2=8 THEN st=11498
IF n1=4 & n2=7 THEN st=13313
IF n1=5 & n2=6 THEN st=16943
IF n1=1 & n2=11 THEN st=22025
IF n1=2 & n2=10 THEN st=22169
IF n1=3 & n2=9 THEN st=22961
!! 
IF n1=4 & n2=8 THEN st=25601
IF n1=5 & n2=7 THEN st=31541
IF n1=6 & n2=6 THEN st=41045
IF n1=1 & n2=12 THEN st=52133
IF n1=2 & n2=11 THEN st=52302
IF n1=3 & n2=10 THEN st=53316
IF n1=4 & n2=9 THEN st=57034
IF n1=5 & n2=8 THEN st=66329
IF n1=6 & n2=7 THEN st=83060
IF n1=1 & n2=13 THEN st=105368
IF n1=2 & n2=12 THEN st=105564
IF n1=3 & n2=11 THEN st=
IF n1=4 & n2=10 THEN st=
IF n1=5 & n2=9 THEN st=
IF n1=6 & n2=8 THEN st=
IF n1=7 & n2=7 THEN st=
!! 
pn=1:pn1=1:pn2=1
for i=1 to n1+n2: pn=pn*i :next
for i=1 to n1: pn1=pn1*i :next
for i=1 to n2: pn2=pn2*i:next
m=pn/(pn1*pn2)

DIM d[n1+n2]
READ.FROM st+1
forj=1 to m

 FOR i=1 TO n1+n2
  READ.NEXT d[i] %// values read in
 NEXT
 FOR i =1 TO n1
  PRINT INT$(d[i]);" "; %// values out
 NEXT
 PRINT " ";
 FOR i =n1+1 TO n1+n2
  PRINT INT$(d[i]);" "; %// values out
 NEXT
 PRINT j
next
END
