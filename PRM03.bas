! ////////////////////////////////////////////////////////
! //
! //                  SCHRAUSSER-MAT:
! //           Permutation methods calculator
! //
! //                    PRM v0.0.5
! //             
! //                        by 
! //               Dietmar G. Schrausser 
! //                    © 2022-23
! //

INCLUDE "strg.inc"
INCLUDE "data1.prm"
INCLUDE "data2.prm"
! INCLUDE "data3.prm"

GR.OPEN 255,150,150,150,0,1
GR.SCREEN sx,sy

! // INI file /////////////////////
FILE.EXISTS f_ini, "prm.ini"
IF f_ini
 TEXT.OPEN r, ini, "prm.ini"
 TEXT.READLN ini, ini$:inf=VAL(ini$)
 TEXT.READLN ini, ini$:pth$=ini$
 TEXT.READLN ini, ini$:p_b=VAL(ini$)
 TEXT.READLN ini, ini$:rp=VAL(ini$)
 TEXT.READLN ini, ini$:psg=VAL(ini$)
 TEXT.READLN ini, ini$:rnsw=VAL(ini$)
 TEXT.READLN ini, ini$:sig=VAL(ini$)
 TEXT.CLOSE ini
ELSE
 ! // Default
 inf=1 % startinfo
 pth$="../../PRM/" % input-output path
 p_b=0.5 % Binomialdesign p
 rp=0.5 % mid p
 psg=0.05 % crit niv
 rnsw=1 % rnd mode 1=system 0=sigma
 sig=2 %tailed
ENDIF

IF inf=1 %Startinfo
 ! DIALOG.MESSAGE ,"Permutation methods calculator PRM Copyright © 2022-23 by Dietmar G. SCHRAUSSER + Veritas in materia principii +",m
ENDIF

st:

GOSUB dlgmode % //// data input ////////////////
param:
GOSUB dlgmethod % // p calculation  method /////

! // set max mv=1000 for distribution vector d //
IF m<=2000
 mv=m
ELSE 
 mv=2000
ENDIF
DIM pvt[mv] % /// Distribution vector d /////////

! // Dialog Seed //////////////////////
INPUT "Seed (0 = sec timevalue)",seed,0
IF seed=0
 GOSUB systime
 seed=INT(SQR(VAL(sec$))*1000) % // timeseed ////
ENDIF

seed0=seed
sgm=34/45

! ///////////////////////////////////////////
! //                      t-value calculation
! //
am1=s1/n1 % arithmetic mean x1
am2=s2/n2 % arithmetic mean x2
q0=am1-am2 % mean diff 0

s21=0
FOR i=1 TO n1
 s21=s21+(n0_1[i]-am1)^2
NEXT
s21=s21/n1 %sample variance s21
s22=0
FOR i=1 TO n2
 s22=s22+(n0_2[i]-am2)^2
NEXT
s22=s22/n2 %sample variance s22
! // t value /////////////////////////////////
twer=am1-am2
twer=twer/(SQR((s21*n1+s22*n2)/(n1+n2-2))*SQR(1/n1+1/n2))
q11=0
GOSUB tp % /////t/probability//////////
GOSUB binom %///Binomial/probability///
IF meth <3 % exact %//wP/Matrices//////
 GOSUB exact % start position st
 READ.FROM st+1
ENDIF

! ////////////////////////////////////////////////////////
! ////////////////////////////////////////////////////////
! //              Permutations, Simulations over cycles M
! //

FOR j=1 TO m %///////over/M////////

 IF meth >= 5 % // Bt, Btr ////////Bootstrap////////
  FOR i=1 TO n1
   IF rnsw=0
    GOSUB rand %Randomization function sigma
    r=INT((n1+n2)* nx )+1 % Bt x1
   ELSE
    r=INT((n1+n2)* RND() )+1 % Bt x1
   ENDIF
   s1m=s1m+ n0_12[r,1] % sum1
  NEXT
  FOR i=1 TO n2
   IF rnsw=0
    GOSUB rand %Randomization function sigma
    r=INT((n1+n2)* nx )+1 % Bt x2
   ELSE
    r=INT((n1+n2)* RND() )+1 % Bt x2
   ENDIF
   s2m=s2m+ n0_12[r,1] % sum2
  NEXT
 ENDIF
 IF meth=3 | meth=4 % // mP, mPr ///randomized/P/////
  ARRAY.SHUFFLE mP_12[] % mP
  FOR i=1 TO n1
   s1m=s1m+ mP_12[i,1] % sum1 m
  NEXT
  FOR i=1 TO n2
   s2m=s2m+ mP_12[i+n1,1] % sum2 m
  NEXT
 ENDIF
 IF meth=1 | meth=2 % // P, Pr //////Exact/p/////////
  FOR i=1 TO n1
   READ.NEXT rnp % P
   s1m=s1m+ n0_12[rnp,1] % sum1
  NEXT
  FOR i=1 TO n2
   READ.NEXT rnp % P
   s2m=s2m+ n0_12[rnp,1] % sum2
  NEXT
 ENDIF

 ! ///////////////////////////////////
 ! //   mean diff m, q1 calculation
 ! // 
 q1=s1m/n1-s2m/n2 %// one-tailed /////
 IF sig=2         %// two-tailed /////
  q0=ABS(q0)
  q1=ABS(q1) 
 ENDIF

 ! ///////////////////////////////////
 ! //   probability calculation, sum
 ! // 
 IF q0=q1 THEN pe=pe+1
 IF q0>q1 THEN pg=pg+1
 IF q0<q1 THEN ps=ps+1
 IF  j=m &(meth<>2 | meth<>4 | meth<>6) % //, p=->p
  IF pg<ps 
   pg=pg+pe
  ELSE
   ps=ps+pe
  ENDIF
 ENDIF

 IF  j=m &(meth = 2 | meth=4 | meth=6)  % // randomized p
  pg=pg+ pe * ((1-rp)/2)
  ps=ps+ pe * ((1-rp)/2)
  pe=pe * rp
 ENDIF
 sgpg$="":sgps$=""
 IF pg/j<=0.1 THEN sgpg$="+" 
 IF pg/j<=0.05 THEN sgpg$="*" 
 IF pg/j<=0.01 THEN sgpg$="**"
 IF ps/j<=0.1 THEN sgps$="+" 
 IF ps/j<=0.05 THEN sgps$="*" 
 IF ps/j<=0.01 THEN sgps$="**"


 s1m=0: s2m=0

 ! /////////////////////////////////////////////////
 ! //                            Screen graph output
 ! //
 GR.CLS 
 ! ////////////////////////////////////////
 pvt[j]=q1 % Simulation distribution vector
 DIM pvt1[j] % tmp vector for graphics
 FOR i=1 TO j
  pvt1[i] =pvt[i]
 NEXT
 ARRAY.SORT pvt1[] % // sort tmp graphics vector 
 ARRAY.MIN minp, pvt1[] % min value

 IF sig=1 % // 1-tailed negativ corr
  FOR i=1 TO j
   pvt1[i]=pvt1[i]+ABS(minp) 
  NEXT
 ENDIF

 xx=(sx-sx/20)-sx/20 % // window width
 yy=(sy/2+sy/20)-(sy-sy/20) % // window hight

 dmin=pvt1[1] % // min value
 dmax=pvt1[j] % // max value
 IF dmax=0 THEN dmax=1 % // division by 0
 ! /////////////////////////////////////////
 ! // permutation, btr distribution graph //
 FOR i=1 TO j % //
  pvt_=pvt1[i]
  yy1=yy-(yy*(pvt_/dmax)) % // actual val/max val
  xx0=xx*((i-1)/j) %  percentage width st
  xx1=xx*((j-i)/j ) % percentage width end

  !! //////x/render/logic///////////
  0 0
  0 0.5  0.5  1
  0 0.33 0.33 0.66 0.66 1
  0 0.25 0.25 0.5  0.5  0.75 0.75 1
  :
  !! ///////////////////////////////

  ! //////////// windows ///////////////////////////////
  GR.COLOR 255,200,200,200,0
  GR.RECT rec, sx/20,sy-sy/20,sx-sx/20,sy/2+sy/20
  GR.RECT rec, sx/2+sx/5,sy/2-sy/8,sx-sx/20,sy/2-sy/2.7
  GR.LINE ln, sx/20,sy-sy/20+yy/2,sx-sx/20,sy-sy/20+yy/2
  GR.LINE ln, sx/20+xx/2,sy-sy/20,sx/20+xx/2,sy/2+sy/20

  ! //////bar/colors/and/style//////////////////////
  IF pvt_<q0 THEN GR.COLOR 255,20,20,20,0  % // q1<q0
  IF pvt_>q0 THEN GR.COLOR 255,120,120,120,0 % // q1>q0
  IF pvt_=q0 THEN GR.COLOR 255,20,20,20,1 % // q1=q0
  ! //////p/distribution/graph/rendering/////////////////////////////////
  GR.RECT rec,(sx/20) +xx0, (sy-sy/20), (sx-sx/20)-xx1, (sy/2+sy/20)-yy1
 NEXT
 ARRAY.DELETE pvt1[] % // delete tmp vector //
 ! ////////////////////////////////////////
 ! // Text rendering 
 GR.COLOR 255,200,200,200,1
 GR.TEXT.BOLD 0
 GR.TEXT.ALIGN 3
 GR.TEXT.SIZE sx/20
 GR.COLOR 255,220,220,220,1
 IF j < m
  GR.TEXT.DRAW tx,sx-sx/13,sy/30, "M="+INT$(m-j) 
 ELSE
  GR.TEXT.DRAW tx,sx-sx/13,sy/30, "M="+INT$(m)
 ENDIF

 GR.TEXT.ALIGN 1
 IF ps>0
  GR.TEXT.DRAW tx, sx/11.7,1.8*(sy/25), " p<:"+FORMAT$("%.#####", ROUND(ps/j,5))+sgps$
 ENDIF
 GR.COLOR 255,200,200,200,1
 IF pe>0
  IF meth = 2 | meth=4 | meth=6
   GR.TEXT.DRAW tx,sx/11.7,2.5*(sy/25), " p=:"+FORMAT$("%.#####", ROUND(pe/j,5))+FORMAT$(" [%.#",rp)+"]"
  ELSE
   GR.TEXT.DRAW tx,sx/11.7,2.5*(sy/25), " p=:"+FORMAT$("%.#####", ROUND(pe/j,5))
  ENDIF
 ENDIF
 GR.COLOR 255,220,220,220,1
 IF pg>0
  GR.TEXT.DRAW tx,sx/11.7,3.2*(sy/25), " p>:"+FORMAT$("%.#####",round (pg/j,5))+sgpg$
 ENDIF
 GR.TEXT.SIZE sx/12
 GR.TEXT.ALIGN 1

 GR.TEXT.DRAW tx,sx/200,sy/25, met$
 GR.COLOR 255,200,200,200,1
 GR.TEXT.SIZE sx/20
 IF 1/m >0.00001 % pmin!//////
  GR.TEXT.DRAW tx,sx/6.5,sy/25, FORMAT$("[ %.#####",1/m)+" ]" 
 ENDIF

 GR.TEXT.ALIGN 3
 GR.TEXT.DRAW tx,sx-sx/13,2.8*(sy/40), "diff[0]: "+STR$(round (ABS(q0),2))
 GR.TEXT.DRAW tx,sx-sx/13,4*(sy/40), INT$(sig) +"-tailed" 
 GR.TEXT.SIZE sx/15
 GR.TEXT.ALIGN 1
 GR.TEXT.DRAW tx,5,sy-sy/100, "PRM: "+des$
 GR.TEXT.ALIGN 2
 GR.TEXT.SIZE sx/5
 GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2-sy/6.5, "A"
 IF (pg/j<=0.05 | ps/j<=0.05) 
  IF sig=1
   IF am1>am2
    GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, ">"
   ELSE
    GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, "<"
   ENDIF
  ELSE
   GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, CHR$(8800)
  ENDIF
 ENDIF

 GR.TEXT.DRAW tx,sx/2+sx/6.5-sx/7,sy/2-sy/6.5, "B"
 GR.TEXT.ALIGN 2
 GR.TEXT.SIZE sx/8
 GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2-sy/14.5, INT$(n1)
 GR.TEXT.DRAW tx,sx/2+sx/6.5-sx/7,sy/2-sy/14.5, INT$(n2)
 GR.TEXT.SIZE sx/20
 GR.TEXT.ALIGN 2

 GR.TEXT.SIZE sx/12
 GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2,  STR$(ROUND(am1,2))
 GR.TEXT.DRAW tx,sx/2+sx/6.5-sx/7,sy/2,  STR$(ROUND(am2,2))
 GR.TEXT.SIZE sx/20
 GR.TEXT.ALIGN 3

 GR.TEXT.DRAW tx,sx-sx/13,sy/2-sy/14.5, FORMAT$("p: %.###",bnp)
 GR.TEXT.DRAW tx,sx-sx/13,sy/2,    FORMAT$("t:  %.###",twer)
 GR.TEXT.ALIGN 1
 GR.COLOR 255,220,220,220,1
 GR.TEXT.DRAW tx,sx/2+sx/4.5,sy/2+sy/40, FORMAT$("p: %.###",pval)+sgpv$
 !endif
 GOSUB systime
 GR.TEXT.SIZE sx/25
 GR.TEXT.ALIGN 3
 GR.COLOR 255,200,200,200,1
 GR.TEXT.DRAW tx,sx-sx/150,sy-sy/100, Y$+"."+M$+"."+D$+"/"+h$+":"+min$+":"+sec$

 GR.RENDER

 GR.TOUCH tc,tx,ty
 IF tc 
  GOTO en
 ENDIF

NEXT j % // over cycles M
! /////////////////////////////////////////////////////////
! ////////////////////////////////////////////////////////

GOSUB dwass

! //////////////////////////////////////////////////
! //                                    File output
! //
! // Distribution vector file out
ARRAY.SORT pvt[]
fileot$=pth$+"prm_pvt.txt"
TEXT.OPEN A, out, fileot$
GOSUB systime
GOSUB head
FOR i=1 TO mv
 IF pvt[i] <= q0
  TEXT.WRITELN out, pvt[i];_tb$;"*" 
 ELSE
  TEXT.WRITELN out, pvt[i]
 ENDIF
NEXT
GOSUB lin
GOSUB bottom
TEXT.CLOSE out
! ///////////////////////////////////////////
! // log file out 
! //
ms$=" M:"+INT$(m)
pmn$=FORMAT$("pmin:%.###",1/m)+FORMAT$("(%.###",(1/m)*2)+")"
pb$=FORMAT$("pb:%.###",bnp)+" ["+STR$(p_b)+"]"
sgi=(ps/m)*2
GOSUB sigbr
ps$=FORMAT$("p<:%.###",ps/m)+FORMAT$("(%.###",sgi)+brs$
pe$=FORMAT$("p=:%.###",pe/m)
pg$=FORMAT$("p>:%.###",pg/m)
IF pg<ps
 sgi=(pg/m)*2
 GOSUB sigbr
 ps$=FORMAT$("p<:%.###",ps/m)
 pg$=FORMAT$("p>:%.###",pg/m)+FORMAT$("(%.###",sgi)+brs$
ENDIF
tw$=FORMAT$("t: %.###",ABS(twer))
sgi=pval % // *2
GOSUB sigbr
pt$=FORMAT$("pt:%.###",pval)+FORMAT$("(%.###",pval*2)+brs$
edp$=FORMAT$("eDp:%.###",edp)
!eds$=FORMAT$("]:%.###",edsg)
!eds$=FORMAT$("eD[%.###",psg)+eds$
eds$=" eD["+STR$(ROUND(psg,2))+"]: "+STR$(ROUND(edsg,3))
sed$=" seed:"+INT$(seed0)
fileot$=pth$+"prm_out.txt"
TEXT.OPEN A, out, fileot$
GOSUB systime
GOSUB head
GOSUB lin
ng=n1
IF n2 > ng THEN ng=n2
TEXT.WRITELN out," ";_tb$;"A";_tb$;"B"
TEXT.WRITELN out," n";_tb$;INT$(n1);_tb$;INT$(n2)
GOSUB lin
TEXT.WRITELN out, pb$ 
GOSUB lin
FOR i=1 TO ng
 IF i <= n1 & i <= n2
  TEXT.WRITELN out," "; INT$(i);_tb$;STR$(n0_1[i]);_tb$;STR$(n0_2[i])
 ENDIF
 IF i > n1 
  TEXT.WRITELN out," "; INT$(i);_tb$;_tb$;STR$(n0_2[i])
 ENDIF
 IF i > n2
  TEXT.WRITELN out," "; INT$(i);_tb$;STR$(n0_1[i])
 ENDIF
NEXT
GOSUB lin
TEXT.WRITELN out," mean";_tb$;STR$(ROUND(am1,2));_tb$;STR$(ROUND(am2,2))
TEXT.WRITELN out," sd";_tb$;STR$(ROUND(SQR(s21),2));_tb$;STR$(ROUND(SQR(s22),2))
!TEXT.WRITELN out," sd'";_tb$;STR$(ROUND(SQR(s21)*(SQR((n1)/(n1-1))),2));_tb$;STR$(ROUND(SQR(s22) *(SQR((n2)/(n2-1))) ,2))
TEXT.WRITELN out," diff";_tb$;STR$(ABS(ROUND(q0,2)))
GOSUB lin
TEXT.WRITELN out, tw$
TEXT.WRITELN out, pt$
GOSUB lin
TEXT.WRITELN out, ps$
IF meth = 2 | meth=4 | meth=6
 TEXT.WRITELN out, pe$ ;FORMAT$("[%.#",rp);"]"
ELSE
 TEXT.WRITELN out, pe$
ENDIF
TEXT.WRITELN out, pg$
!GOSUB lin
!TEXT.WRITELN out, edp$
GOSUB lin
TEXT.WRITELN out, " Method: ";met$
TEXT.WRITELN out, sed$
TEXT.WRITELN out, ms$
TEXT.WRITELN out, pmn$
TEXT.WRITELN out, eds$
TEXT.WRITELN out, "" 
GOSUB lin
GOSUB bottom
TEXT.CLOSE out
! ///////////////////////////////////////////
! // dat file out 
! //
fileot$=pth$+"prm_outdat.txt"
TEXT.OPEN A, out, fileot$
GOSUB systime
GOSUB head
!!
FOR j=1 TO n1+n2
 IF sw1g=1
  TEXT.WRITELN out, INT$(n0_12[j,1])
 ENDIF
 IF sw2x2=0 & sw1g=0
 TEXT.WRITELN out, INT$(n0_12[j,1])+" "+INT$(n0_12[j,2])
 ENDIF
 IF sw2x2=1 & sw1g=0
  TEXT.WRITELN out, INT$(n0_12[j,1])+" "+INT$(n0_12[j,2]) +" "+INT$(n0_12[j,3])
 ENDIF
NEXT
!!
FOR i=1 TO n1
 TEXT.WRITELN out, STR$(n0_1[i])+" 1"
NEXT
FOR i=1 TO n2
 TEXT.WRITELN out, STR$(n0_2[i])+" 2"
NEXT
GOSUB lin
GOSUB bottom
TEXT.CLOSE out

en: % // wait... 
FORi=1 TO 1000000
NEXT

DO % // wait
 GR.TOUCH tc1,tx,ty
 IF tc1
  GOTO st
 ENDIF
UNTIL 0

ONBACKKEY:
GOSUB fin
END

! ////////////////////////////////////////////////////////
! //                                         Subroutines
! //
tp: 
! // t probability p(t,n) /////////////////
n=n1+n2
nn2=1
IF n/2 <> FLOOR(n/2)
 n=n+1
ENDIF
n1_ = n
n = 1
w4 = twer
twer =POW(twer,2)
w1 = n1_/(n1_+1*twer)
w2 = SQR(1-w1)
nn1 =2*FLOOR(n1_/2)-n1_+2
pval = 1-w2
w3 = w1*w2/2
FOR i = nn1 TO n1_ STEP 2
 ij = i
 IF n1_ <= i | ABS(2/i*w3)<0.00001*pval
  GOTO e
 ENDIF
 pval = pval-2/i*w3
 w3 = w3*w1*(nn2/i+1)
NEXT
e:
pval = pval/2
n = n1_
tx = w4
IF twer < 0
 pval = 1-pval
ENDIF
xx = pval
yy = -5
IF xx = 0 THEN GOTO g
fx = (ABS(xx))
fx = LOG10(fx)
yy = fx + yy
IF fx >= 0 
 fx = FLOOR(fx)
ELSE
 fx = -1*(FLOOR(ABS(fx)))
ENDIF
zz = fx
fx = yy
IF fx >= 0
 fx = FLOOR(fx)
ELSE
 fx = -1*(FLOOR(ABS(fx)))
ENDIF
yy = zz-fx-1
IF yy >= 19 THEN GOTO g
IF yy < 0
 xx = 0
 GOTO g
ENDIF
IF zz >= 90
 xx = 10^-20
 fx = zz
 zz = zz-20
ENDIF
ww = ABS(xx*POW(10,-zz ))
ww = FLOOR(ww*POW(10,yy)+0.5)
ww = ww*(POW(10,zz))*(POW(10,-yy))
IF fx >= 90
 ww = 10^20
ENDIF
IF xx >= 0
 xx = ww
ELSE
 xx = ww * -1
ENDIF
g:
pval = xx
pval =ABS(pval)*sig % tail
sgpv$="" 
IF pval<=0.1 THEN sgpv$="+" 
IF pval<=0.05 THEN sgpv$="*" 
IF pval<=0.01 THEN sgpv$="**"
twer=ABS(w4)
RETURN

binom:
! // Binomial probability p /////////////////
q_b=1-p_b
bnp=0
fa_n=1
FOR i=1 TO n1+n2
 fa_n=fa_n*i
NEXT
FOR i=0 TO n1
 fa_i=1
 FOR j=1 TO i
  fa_i=fa_i*j
 NEXT
 fa_ni=1
 FOR j=1 TO n1+n2-i
  fa_ni=fa_ni*j
 NEXT
 bnp=bnp+(fa_n/(fa_i*fa_ni))*p_b^i*q_b^(n1+n2-i)
NEXT
RETURN

dwass:
!// Dwass efficiency ///////////////////////
pdw=pg/m
IF pdw>ps/m THEN pdw=ps/m
IF pdw>0 & psg>0
 edp=  1-SQR((1-pdw)/pdw)*(1/SQR(m*2*PI()))
 edsg= 1-SQR((1-psg)/psg)*(1/SQR(m*2*PI()))
ENDIF
RETURN

rand:
!// Random number n[0,1] /////////////////
!// by randomization function sigma,
!// Schrausser (2009)
rn=seed^sgm
nx=10*(rn-FLOOR(rn))-FLOOR(10*(rn-FLOOR(rn)))
seed=nx
RETURN

perm:
!// n of permutations  wP
p_n=1: p_n1=1: p_n2=1
FOR i=1 TO n1+n2:p_n=p_n*i:NEXT
FOR i=1 TO n1:p_n1=p_n1*i:NEXT
FOR i=1 TO n2:p_n2=p_n2*i:NEXT
perm=p_n/(p_n1*p_n2) 
RETURN

exact:
!// P vectors start position //////////////////
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
!! 
RETURN

! menue ////////////////////////////////

dlgmode:
! /////////////////////////////////////////////////
! ////////////  Raw data input mode ///////////////
! //
ARRAY.LOAD mod$[], "Input","File", "exit" 
DIALOG.SELECT mode,mod$[], "Select Mode..."
SW.BEGIN mode 
  ! ////////////////////////////////////////////////
 SW.CASE 1 % //                         direct input       
  sw1g=0: sw2g=0: sw2x2=0 % design switches ini
  ARRAY.LOAD dsgn$[], "1 Group","2 Groups", "%2x2"
  DIALOG.SELECT dsgn, dsgn$[], "Select design..."
  SW.BEGIN dsgn
   SW.CASE 1
    sw1g=1 % //// 1 group design /////////////
    des$= "(x)"
    SW.BREAK
   SW.CASE 2
    sw2g=1 % ///// 2 groups design ///////////
    des$="(x|g)
    SW.BREAK
   SW.CASE 3
    sw2x2=1 % ///// 2x2 design ///////////////
    des$="(x|g1|g2)
    SW.BREAK
  SW.END

  GOSUB dlgtail % // tail

  IF sw2g=1 | sw1g=1
   INPUT "n(1)", n1, 5 % n1   % // input n1 ////
   IF sw2g=1
    INPUT "n(2)", n2, 5 % n2  % // input n2 ////
   ENDIF
   IF sw2x2=1
    ! /////2x2/design/////////
   ENDIF
  ENDIF

  ! ////////////////////////////////////////////
  ! //                             Direct input
  IF sw1g=1
   n2=n1
  ENDIF
  ng=n1
  IF n2> ng THEN ng=n2 % max group n switch
  DIM n0_1[ng] % Vector a
  DIM n0_2[ng] % Vector b
  DIM n0_12[n1+n2,2] % Raw data matrix X
  ! ///////////////////////////////////////
  ! //      Raw data input, summation
  ! //
  s1=0
  FOR i=1 TO n1
   inp$="x(1,"+INT$(i)+")"
   INPUT inp$, n0_1[i]
   s1=s1+n0_1[i] % sum1
  NEXT
  s2=0
  IF sw2g=1
   FOR i=1 TO n2
    inp$="x(2,"+INT$(i)+")"
    INPUT inp$, n0_2[i]
    s2=s2+n0_2[i] % sum2
   NEXT
  ENDIF

  IF sw1g=1
   am1=s1/n1
   intx$= "am1="+FORMAT$("#.##",am1)+", diff..."
   INPUT intx$, adif, 1 % // crit diff
   am0=am1-adif % // crit mean
   FOR i=1 TO n2
    n0_2[i]=(n0_1[i]-am1)+am0 % // x1 transf to x2 with am0
    s2=s2+n0_2[i] % sum2
   NEXT
  ENDIF
  ! ////////////////////////////////////////
  ! // vectors x1, x2 to (x|g) /////////////
  n=1
  FOR i=1 TO n1+n2
   IF i <=n1
    n0_12[i,1]=n0_1[i]
    n0_12[i,2]=n
   ELSE
    n0_12[i,1]=n0_2[i-n1]
    n0_12[i,2]=n
   ENDIF
   n=n+1
  NEXT
  GOTO param
  SW.BREAK
  ! /////////////////////////////////////////////////
 SW.CASE 2 % //                           File input 
  sw2x2=1 % design switches
  sw1g=0  %

  INPUT "File", fi$, "prm_indat.txt" %input file//
  GOSUB dlgtail % // tail
  filedt$=pth$+fi$
  DIM cin[2,2] % n counter ini
  FOR i=1 TO 2: cin[i,1]=0: cin[i,2]=0: NEXT
  DIM cin2[2,2]
  FOR i=1 TO 2: cin2[i,1]=0: cin2[i,2]=0: NEXT
  n_=-1 % // n total count
  TEXT.OPEN R, dat, filedt$
  DO
   TEXT.READLN dat, in$
   n_=n_+1 % n counter
  UNTIL in$="EOF"
  TEXT.CLOSE dat
  ! // Raw data matrix X
  DIM n0_12[n_,3] 
  TEXT.OPEN R, dat, filedt$ % // Raw data read from file
  FOR j=1 TO n_
   TEXT.READLN dat, in$
   in1$="" % ini
   in2$="" %
   in3$="" %
   leng= LEN(in$)  % string lenght of row
   sw=0
   FOR i= 1 TO leng
    lin$= MID$(in$,i,1)
    IF lin$=" " 
     sw=sw+1 % data position switch
    ENDIF
    IF sw=0 % //    Column n=1, 1g design
     in1$=in1$+lin$
     des$="(x)
    ENDIF
    IF sw=1 % //    Column n=2, 2g design
     in2$=in2$+lin$
     des$="(x|g)
    ENDIF
    IF sw=2 % //    Column n=3, 2x2 design
     in3$=in3$+lin$
     des$="(x|g1,g2)
    ENDIF
   NEXT i
   ! //  Data conversion and readin
   IF in$<>"EOF" 
    n0_12[j,1]= VAL(in1$)
    IF in2$<>"" 
     n0_12[j,2]= VAL(in2$)
    ELSE
     sw1g=1 % 1g design
    ENDIF
    IF in3$<>"" 
     n0_12[j,3]= VAL(in3$)
    ELSE
     sw2x2=0 % 2g design
    ENDIF

    y= n0_12[j,2]
    z= n0_12[j,3]
    ! //counter read in
    IF y=1 THEN cin[1,1]=cin[1,1]+1
    IF y=2 THEN cin[2,1]=cin[2,1]+1
    IF z=1 THEN cin[1,2]=cin[1,2]+1
    IF z=2 THEN cin[2,2]=cin[2,2]+1
    IF y=1 & z=1 THEN cin2[1,1]=cin2[1,1]+1
    IF y=2 & z=1 THEN cin2[2,1]=cin2[2,1]+1
    IF y=1 & z=2 THEN cin2[1,2]=cin2[1,2]+1
    IF y=2 & z=2 THEN cin2[2,2]=cin2[2,2]+1
   NEXT j
   TEXT.CLOSE dat
   ! /////////////////////////////////////////////
   ! //         File input, summation
   n1=cin[1,1] % n1
   n2=cin[2,1] % n2
   s1=0:s2=0
   i1=1:i2=1
   DIM n0_1[n1]
   DIM n0_2[n2]
   FOR i=1 TO n1+n2
    IF n0_12[i,2]=1
     s1=s1+n0_12[i,1]  % sum1
     n0_1[i1]=n0_12[i,1] % vector x1
     i1=i1+1
    ENDIF
    IF n0_12[i,2]=2
     s2=s2+n0_12[i,1]  % sum2
     n0_2[i2]=n0_12[i,1] % vector x2
     i2=i2+1
    ENDIF
   NEXT
   SW.BREAK
  SW.CASE 3 % //
   GOSUB fin
   END
   SW.BREAK
 SW.END
 RETURN

 dlgmethod:
 ! ////////////////////////////////////////////////
 ! //                                Dialog method
 ! param:
 ARRAY.LOAD meth$[], "P","Pr","mP","mPr", "Bt", "Btr"
 DIALOG.SELECT meth,meth$[], "Select Method..."
 SW.BEGIN meth
  SW.CASE 1
   met$="P"
   SW.BREAK
  SW.CASE 2
   met$="Pr"
   SW.BREAK
  SW.CASE 3
   met$="mP"
   SW.BREAK
  SW.CASE 4
   met$="mPr"
   SW.BREAK
  SW.CASE 5
   met$="Bt"
   SW.BREAK
  SW.CASE 6
   met$="Btr"
   SW.BREAK
 SW.END

 IF meth=3 | meth =4
  DIM mP_12[n1+n2,2]
  FOR i=1 TO n1+n2
   mP_12[i,1]=n0_12[i,1]
   mP_12[i,2]=n0_12[i,2]
  NEXT
 ENDIF
 s1m=0 % Simulation sums ini ////////////////
 s2m=0 %
 pe=0 % p values ini ////////////////////////
 pg=0 %
 ps=0 %

 IF meth=2|meth=4|meth=6 % /// dialog ///////
  INPUT "midp=" , rp, 0.5 % // midp /////////
 ENDIF

 IF meth>2 % // Dialog M /////////////////////////
  INPUT "cycles M", m, 20000 % Simulation cycles M
 ELSE
  GOSUB perm % ///// n of permutations /////
  m=perm
 ENDIF
 RETURN

 dlgtail:
 DO
  INPUT "tailed=", sig, 1 % // input tail //////
 UNTIL sig=1 | sig=2
 RETURN

 ! //////////////////////////////////////
 ! /// outputfile routines ///////////////
 sigbr:
 brs$=")"
 IF sgi < 0.1 THEN brs$="+)"
 IF sgi < 0.05 THEN brs$="*)"
 IF sgi < 0.01 THEN brs$="**)"
 IF sgi < 0.001 THEN brs$="***)"
 RETURN

 systime:
 TIME Y$, M$, D$, h$, min$, sec$
 RETURN

 head:
 TEXT.WRITELN out,"PRM/"+Y$+"."+M$+"."+D$+"/"+h$+":"+min$+":"+sec$+"/"
 RETURN

 lin:
 ln$=""
 FOR i= 1 TO 23: ln$=ln$+"-":NEXT
 TEXT.WRITELN out, ln$
 RETURN

 bottom:
 TEXT.WRITELN out,"SCHRAUSSER -"
 RETURN

 fin:
 ! // write ini //////////////
 TEXT.OPEN w, f_ini, "prm.ini"
 TEXT.WRITELN f_ini, inf
 TEXT.WRITELN f_ini, pth$
 TEXT.WRITELN f_ini, p_b
 TEXT.WRITELN f_ini, rp
 TEXT.WRITELN f_ini, psg
 TEXT.WRITELN f_ini, rnsw
 TEXT.WRITELN f_ini, sig
 TEXT.CLOSE f_ini
 CONSOLE.TITLE "PRM"
 PRINT"© 2022-23 by Dietmar G. Schrausser"
 RETURN
 ! END //
