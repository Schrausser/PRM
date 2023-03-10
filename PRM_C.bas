! /////////////////////////////////////// 
! 
!         Permutation Matrix Generator
! 
!       (c) 2023 by Dietmar G. Schrausser
! 
INPUT "n", n_, 6
INPUT " m", m_, 3
dat$="READ.DATA "
theta =1
zlr=0
sw=0
DIM vi[100]
DIM c_wert[n_]
DIM c1_wert[n_,2]
TEXT.OPEN w, dat, "datP.txt"

FOR i= 1 TO n_
 c_wert[i]=i
NEXT

FOR i= 1 TO 100
 vi[i]=1
NEXT
vi[1]=0

st:
! // general index generation 
FOR i=1 TO  m_
 IF i=1 THEN vi[1]= vi[1]+1
 IF i=m_ & vi[i] > n_ THEN GOTO brk
 IF vi[i] > n_
  vi[i]  = 1
  vi[i+1] = vi[i+1] +1
 ENDIF
NEXT i

! // endcriterion
IF i =m_ & vi[i] > n_ THEN GOTO brk

! // element repeat test at C, V, P, wP
IF theta =1 | theta=3 | theta=5 | theta=6
 FOR i=1 TO m_
  FOR j=i+1 TO m_
   IF vi[i]=vi[j]  THEN sw=1
  NEXT j
 NEXT i
ENDIF

! // arrangement repeat test at C, wC, wP
IF sw=0 & (theta=1 | theta=2 | theta=6)
 FOR i=1 TO m_
  FOR j = i+1 TO m_
   IF vi[i]>vi[j] THEN sw=1
  NEXT j
 NEXT i
ENDIF
! // line vector output to file
IF sw=0 
 !// C, wC, V, wV, P
 IF theta<6 
  FOR i =1 TO m_
   ! // PRINT INT$(c_wert[vi[i]]);CHR$(9);
   c1_wert[i,1]=c_wert[vi[i]]
   c1_wert[i,2]=1
  NEXT

  zl1=1
  FOR i=1 TO n_
   swv=0
   FOR j=1 TO m_
    IF vi[j]=i
     swv=1
    ENDIF
   NEXT
   IF swv=0
    c1_wert[m_+zl1,1]=c_wert[i]
    c1_wert[m_+zl1,2]=2
    zl1=zl1+1
   ENDIF
  NEXT

  PRINT INT$(zlr+1);CHR$(9); CHR$(9);
  FOR i=1 TO m_
   PRINT INT$(c1_wert[i,1]);CHR$(9);
   dat$=dat$+ INT$(c1_wert[i,1])+","
  NEXT
  PRINT CHR$(9);
  FOR i= 1 TO n_-m_
   PRINT INT$(c1_wert[m_+i,1]);CHR$(9);
   dat$=dat$+ INT$(c1_wert[m_+i,1])
   IF i < n_- m_ THEN dat$=dat$+","
  NEXT
  PRINT CHR$(10) % lf
  TEXT.WRITELN dat, dat$
  dat$="READ.DATA "
 ENDIF                                                  
 ! // wP
 IF theta=6
  FOR i=1 TO m_
   PRINT  c_wert[vi[i]]
   c1_wert[i]=c_wert[vi[i]]
  NEXT      
  PRINT " " % // k1
  FOR i=1 TO  n_
   FOR j=1 TO m_
    IF vi[j]=i  THEN sw=1 %// k1 k2 element comparison
   NEXT
   IF sw=0 THEN PRINT c_wert[i] % //k2
   sw=0
  NEXT
 ENDIF
 zlr = zlr+1
ENDIF
sw=0
GOTO st
brk:
wp=1: FOR i=1 TO n_:wp=wp*i:NEXT
wp1=1:FOR i=1 TO m_:wp1=wp1*i:NEXT
wp2=1:FOR i=1 TO n_-m_:wp2=wp2*i:NEXT
wp=wp/(wp1*wp2)
PRINT "WP=C"+INT$(n_)+"("+INT$(m_)+")="INT$(wp)
pmin=1/wp
pmin2=pmin*2
p2$= FORMAT$("(%.###",pmin2)
PRINT FORMAT$("pmin=%.###",pmin)+p2$;")"
TEXT.CLOSE dat
END
