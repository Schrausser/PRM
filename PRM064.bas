! //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
! //
! //                               SCHRAUSSER-MAT:
! //                        Permutation methods calculator
! //
! //                                   P R M
! //             
! //                                    by 
! //                          Dietmar Gerald Schrausser 
! //                                  © 2023
! //
_name$="PRM"
_ver$="v1.2"
! /////////////////////////////////////////////////////////
INCLUDE strg.inc
INCLUDE data1.prm
INCLUDE data2.prm
INCLUDE data3.prm
! //                                              INI file
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
 TEXT.READLN ini, ini$:seed=VAL(ini$)
 TEXT.READLN ini, ini$:meth=VAL(ini$)
 TEXT.READLN ini, ini$:met$=ini$
 TEXT.READLN ini, ini$:m0=VAL(ini$)
 TEXT.READLN ini, ini$:fi$=ini$
 TEXT.READLN ini, ini$:mode=VAL(ini$)
 TEXT.READLN ini, ini$:thta$=ini$
 TEXT.READLN ini, ini$:thmode=VAL(ini$)
 TEXT.READLN ini, ini$:rnsw$=ini$
 TEXT.READLN ini, ini$:fiout$=ini$
 TEXT.READLN ini, ini$:fidt$=ini$
 TEXT.READLN ini, ini$:fids$=ini$
 TEXT.READLN ini, ini$:grsw=VAL(ini$)
 TEXT.READLN ini, ini$:pvsw=VAL(ini$)
 TEXT.READLN ini, ini$:msw=VAL(ini$)
 TEXT.READLN ini, ini$:mvd=VAL(ini$)
 TEXT.READLN ini, ini$:inpt=VAL(ini$)
 TEXT.READLN ini, ini$:swout=VAL(ini$)
 TEXT.READLN ini, ini$:swdal=VAL(ini$)
 TEXT.READLN ini, ini$:swvec=VAL(ini$)
 TEXT.READLN ini, ini$:m1=VAL(ini$)
 TEXT.READLN ini, ini$:fo1$=ini$
 TEXT.READLN ini, ini$:fo2$=ini$
 TEXT.READLN ini, ini$:fo3$=ini$
 TEXT.READLN ini, ini$:win1$=ini$
 TEXT.READLN ini, ini$:win2$=ini$
 TEXT.READLN ini, ini$:win3$=ini$
 TEXT.READLN ini, ini$:scm$=ini$
 TEXT.READLN ini, ini$:scm=VAL(ini$)
 TEXT.READLN ini, ini$:outsw1=VAL(ini$)
 TEXT.READLN ini, ini$:outsw2=VAL(ini$)
 TEXT.READLN ini, ini$:outsw3=VAL(ini$)
 TEXT.READLN ini, ini$:outsw4=VAL(ini$)
 TEXT.CLOSE ini
ELSE
 ! // Default //
 inf=1                        % // startinfo sw          //
 pth$="../../PRM/"            % // input-output path     //
 p_b=0.5                      % // Binomialdesign p      //
 rp=0.5                       % // mid p                 //
 psg=0.05                     % // crit niv              //
 rnsw=1                       % // rnd -1=sys 1=sigma    //
 sig=1                        % // tailed                //
 seed=0                       % // seed, 0= systime      //
 meth=1                       % // method switch P       //
 met$="P"                     % // method switch label   //
 m0=10000                     % // initial random cycles //
 fi$=""                       % // default input file    // 
 mode=1                       % // default input mode    //
 thta$="AM Difference [dAM]"  % // default Theta label   //
 thmode=1                     % // default Theta         //
 rnsw$="Sigma"                % // rnd label             //
 fiout$="prm_out.txt"         % // default log file      // 
 fidt$="prm_outdat.txt"       % // default datlog file   // 
 fids$="prm_pvt.txt"          % // default distr file    //
 grsw=1                       % // dynamic graph sw      //
 pvsw=1                       % // dyn p value sw        //
 msw=1                        % // dyn m counter sw      //
 mvd=10000                    % // dist vect max         //
 inpt=2                       % // input mode            //
 swout=1                      % // output log on sw      //
 swdal=1                      % // output dat on sw      //
 swvec=1                      % // output vec on sw      //
 m1=10000                     % // default random cycles //
 fo1$=" [Out] "               % // output log label      //
 fo2$=" [Dat] "               % // output dat label      //
 fo3$=" [Dist] "              % // output dist label     //
 win1$=" Dist~ "              % // window graph label    //
 win2$=" p~ "                 % // window p val label    //
 win3$=" m~ "                 % // window m label        //
 scm$=" Default"              % // scheme label          //
 scm=1                        % // scheme sw             //  
 outsw1=1                     % // outpt option sw1      //
 outsw2=1                     % // outpt option sw2      //
 outsw3=1                     % // outpt option sw3      //
 outsw4=1                     % // outpt option sw4      //
ENDIF
m=m0                          % // default m reset       //
IF seed=0
GOSUB dlgseed:ENDIF           % // seed value
!seed1=seed
__sd_=seed
sgm=34/45                     % // sigma                 //
scsw=1                        % // scheme sw ini         //
dlsw=1                        % // dlgmain sw ini        //
swtl=1                        % // taild switch          //
smq$=CHR$(9654)               % // menue symbol >        //
IF sig=2: swtl=-1: ENDIF

st0:                          % // start0                //

IF scm=1:GR.OPEN 255,150,150,150,0,1:ENDIF % // Default  //
IF scm=2:GR.OPEN 255,80,30,30,0,1:ENDIF    % // Red      //
IF scm=3:GR.OPEN 255,30,80,30,0,1:ENDIF    % // Green    //
IF scm=4:GR.OPEN 255,30,30,30,0,1:ENDIF    % // Inverted //
IF scm=5:GR.OPEN 255,255,255,255,0,1:ENDIF % // RGB1     //
IF scm=6:GR.OPEN 255,200,200,200,0,1:ENDIF % // RGB2     //

GR.SCREEN sx,sy

IF inf=1 %Startinfo
 !DIALOG.MESSAGE ,_name$+ "Permutation methods calculator "+_ver$+" Copyright © 2023 by Dietmar G. SCHRAUSSER + Veritas in materia principii +",msg
ENDIF

st:                           % // start                 //      

IF mode=2                     % // initial file input    //
GOSUB filinp: ENDIF

s1m=0                         % // Simulation sums ini   //
s2m=0                         %
pe=0                          % // p values ini          //
pg=0                          % //
ps=0                          % //
seedsw=0                      % // rnd reset             //

IF rnsw=-1
 rdz=RANDOMIZE(seed)          % // system rnd seed       //
ENDIF

IF dlsw=1 THEN GOSUB dlgmain      % // main menue          //
IF scsw=0
 scsw=1:dlsw=0:GR.CLOSE:GOTO st0  % // scheme sw //  
ENDIF                  
dlsw=1                            % // dlgmain sw reset            //

IF meth=3 | meth =4               % // mP, mPr             //
 DIM mP_12[n1+n2,2]
 FOR i=1 TO n1+n2
  mP_12[i,1]=n0_12[i,1]
  mP_12[i,2]=n0_12[i,2]
 NEXT
ENDIF

GOSUB tval                    % // q0, t value           //
IF thmode=1                   % // dAM                   //
 GOSUB tp                     % // t probability         //
ENDIF
GOSUB binom                   % // Binomial probability  //
IF meth <3                    % // exact % wP/Matrices   //
 GOSUB perm
 m=perm
 GOSUB exact                  % // start position st     //
 READ.FROM st+1
ENDIF

IF m<=mvd                     % // max mv for distr vector d //
 mv=m: ELSE: mv=mvd
ENDIF
DIM pvt[mv]                   % // Distribution vector d //

! /////////////////////////////////////////////////////////
! //                             Simulations over cycles M

IF meth>2:m_=m-1:ELSE:m_=m:ENDIF
FOR j=1 TO m_  % // over m //
 DIM q11[n1]
 DIM q12[n2]
 ! //
 IF meth >= 5 % // Bt, Btr         / ///Bootstrap/// /
  FOR i=1 TO n1
   IF rnsw=1
    GOSUB rand % // Randomization function sigma //
    r=INT((n1+n2)* n__ )+1    % // Bt x1      //
   ELSE
    r=INT((n1+n2)* RND() )+1 % // Bt x1      //
   ENDIF
   s1m=s1m+ n0_12[r,1]       % // sum1       //
   q11[i]=  n0_12[r,1]       % // vector q11 //
  NEXT
  FOR i=1 TO n2
   IF rnsw=1
    GOSUB rand % // Randomization function sigma //
    r=INT((n1+n2)* n__ )+1    % // Bt x2      //
   ELSE
    r=INT((n1+n2)* RND() )+1 % // Bt x2      //
   ENDIF
   s2m=s2m+ n0_12[r,1]       % // sum2       //
   q12[i]=  n0_12[r,1]       % // vector q12 //
  NEXT
 ENDIF
 ! //
 IF meth=3 | meth=4 % // mP, mPr / ///randomized/P/// /

  DIM perm [n1+n2]
  n2_sw=1
  DO                              % // n2_sw loop //
   FOR i=1 TO n1+n2                                   
    DO                            % // n_sw loop //
     n_sw=0
     IF rnsw=-1 % // Randomization function system // 
      r=ROUND((RND()*((n1+n2)-1))+1)  % // random index i[1,n1+n2] //
     ELSE       % // Randomization function sigma //
      GOSUB rand
      r=ROUND((n__*((n1+n2)-1))+1)  % // random index i[1,n1+n2] //
     ENDIF
     IF i>1
      FOR k=1 TO i-1
       IF perm[k]=r:n_sw=1:ENDIF % // permutation switch n_sw://
      NEXT                        % // no multiple elements    //
     ENDIF
    UNTIL n_sw=0                  % // n_sw
    perm[i]=r
   NEXT
   FOR i=1 TO n1                  % // class permutation n2_sw   //
    IF perm[i]>n1:n2_sw=0         % // no intraclass permutation //
    ENDIF
   NEXT
  UNTIL n2_sw=0                   % // n2_sw //

  FOR i=1 TO n1
   s1m=s1m+ mP_12[perm[i],1]       % // sum1 m     //
   q11[i]=  mP_12[perm[i],1]       % // vector q11 //
  NEXT
  FOR i=1 TO n2
   s2m=s2m+ mP_12[perm[i+n1],1]    % // sum2 m     //
   q12[i]=  mP_12[perm[i+n1],1]    % // vector q12 //
  NEXT

  ARRAY.DELETE perm[] % // delete tmp vector //

 ENDIF
 ! //
 IF meth=1 | meth=2 % // P, Pr      / ///Exact/p/// /
  FOR i=1 TO n1
   READ.NEXT rnp % P
   s1m=s1m+ n0_12[rnp,1]     % // sum1       //
   q11[i]=  n0_12[rnp,1]     % // vector q11 //
  NEXT
  FOR i=1 TO n2
   READ.NEXT rnp % P
   s2m=s2m+ n0_12[rnp,1]     % // sum2       //
   q12[i]=  n0_12[rnp,1]     % // vector q12 //
  NEXT
 ENDIF

 ! ////////////////////////////////////////////////////////
 ! //                                             Theta, q1
 IF thmode=1 % //dAM //
  q1=s1m/n1-s2m/n2      % // one-tailed //
  IF sig=2         
   q0=ABS(q0)           % // two-tailed //
   q1=ABS(q1) 
  ENDIF
 ENDIF

 IF thmode=2 % //dSD //
  s211=0:s221=0
  FOR i=1 TO n1
   s211=s211+(q11[i]-s1m/n1)^2 
  NEXT
  s211=SQR(s211/n1)

  FOR i=1 TO n2
   s221=s221+(q12[i]-s2m/n2)^2 
  NEXT
  s221=SQR(s221/n2)

  q1=s211-s221          % // one-tailed //
  IF sig=2         
   q0=ABS(q0)           % // two-tailed //
   q1=ABS(q1) 
  ENDIF
 ENDIF

 ARRAY.DELETE q11[]
 ARRAY.DELETE q12[]
 ! ////////////////////////////////////////////////////////
 ! //                                sum, p/btr probability
 IF j=1 & meth>2:pe=1:ENDIF
 IF q0=q1 THEN pe=pe+1
 IF q0>q1 THEN pg=pg+1
 IF q0<q1 THEN ps=ps+1
 IF  j=m_ &(meth=1 | meth=3 | meth=5) % // p=->p //
  IF pg<=ps:pg=pg+pe: ELSE: ps=ps+pe
  ENDIF
 ENDIF
 ! //
 IF  j=m_ &(meth = 2 | meth=4 | meth=6) % // randomized p //
  IF pg<=ps
   pg=pg+ pe * rp
   ps=ps+ pe * (1-rp) 
  ELSE
   pg=pg+ pe * (1-rp) 
   ps=ps+ pe * rp
  ENDIF
 ENDIF
 ! // 
 sgpg$="":sgps$=""              % // sig marks  //
 IF pg/j<=0.1 THEN sgpg$="+" 
 IF pg/j<=0.05 THEN sgpg$="*" 
 IF pg/j<=0.01 THEN sgpg$="**"
 IF pg/j<=0.001 THEN sgpg$="***"
 IF ps/j<=0.1 THEN sgps$="+" 
 IF ps/j<=0.05 THEN sgps$="*" 
 IF ps/j<=0.01 THEN sgps$="**"
 IF ps/j<=0.001 THEN sgps$="***"

 s1m=0: s2m=0                   % // sum scores //

 ! ////////////////////////////////////////////////////////
 ! //                                  Screen graph output
 xx=(sx-sx/20)-sx/20        % // window width
 yy=(sy/2+sy/20)-(sy-sy/20) % // window hight

 pvt[j]=q1   % // Simulation distribution vector //

 GR.CLS

 IF grsw=1 | (grsw=-1 &j>m_-2) % // graph out sw //

  DIM pvt1[j] % // tmp vector for graphics
  DIM pvt2[j] % // tmp vector for vector

  FOR i=1 TO j
   pvt1[i] =pvt[i]
   pvt2[i]=pvt[i]
  NEXT
  ARRAY.SORT pvt1[]      % // sort tmp graphics vector 
  IF j=m_
   DIM pval2[m_] % // pvalue vector //
   FOR k=1 TO m_:pval2[k]=k/m_:NEXT
   ARRAY.SORT pvt2[]      % // sort tmp vector vector 
  ENDIF
  ARRAY.MIN minp, pvt1[] % // min value

  IF sig=1               % // 1-tailed negativ corr
   FOR i=1 TO j
    pvt1[i]=pvt1[i]+ABS(minp) 
   NEXT
  ENDIF

  dmin=pvt1[1]           % // min value
  dmax=pvt1[j]           % // max value
  IF dmax=0 THEN dmax=1  % // division by 0

  ! ////////////////////////////////////////////////////////
  ! //                   permutation, btr distribution graph
  FOR i=1 TO j % //
   pvt_=pvt1[i]
   yy1=yy-(yy*((pvt_/dmax))*0.9) % // actual val/max val
   xx0=xx*((i-1)/j) %  percentage width st
   xx1=xx*((j-i)/j ) % percentage width end

   ! ///////////////////////////////////////////////////////
   ! //                                bar colors and style
   SW.BEGIN scm
    SW.CASE 1          % // default //
     cr01s= 20:cg01s= 20:cb01s= 20
     cr01g=120:cg01g=120:cb01g=120
     cr01e= 20:cg01e= 20:cb01e= 20
     cr0s=  20:cg0s=  20:cb0s=  20
     cr0g= 120:cg0g= 120:cb0g= 120
     cr0e=  20:cg0e=  20:cb0e=  20
     sc1=0
     SW.BREAK
    SW.CASE 2          % // red //
     cr01s=255:cg01s= 30:cb01s= 30
     cr01g=185:cg01g= 30:cb01g= 30
     cr01e=255:cg01e= 30:cb01e= 30
     cr0s= 255:cg0s=  30:cb0s=  30
     cr0g= 185:cg0g=  30:cb0g=  30
     cr0e= 255:cg0e=  30:cb0e=  30
     sc1=1
     SW.BREAK
     SW.BREAK
    SW.CASE 3          % // green //
     cg01s=255:cr01s= 30:cb01s= 30
     cg01g=185:cr01g= 30:cb01g= 30
     cg01e=255:cr01e= 30:cb01e= 30
     cg0s= 255:cr0s=  30:cb0s=  30
     cg0g= 185:cr0g=  30:cb0g=  30
     cg0e= 255:cr0e=  30:cb0e=  30
     sc1=1
     SW.BREAK
    SW.CASE 4          % // inverted //
     cr01s=150:cg01s=150:cb01s=150
     cr01g= 90:cg01g= 90:cb01g= 90
     cr01e=150:cg01e=150:cb01e=150
     cr0s= 150:cg0s= 150:cb0s= 150
     cr0g=  90:cg0g=  90:cb0g=  90
     cr0e= 150:cg0e= 150:cb0e= 150
     sc1=0
     SW.BREAK
    SW.CASE 5          % // RGB1 //
     cr01s=100:cg01s=220:cb01s=100
     cr01g=150:cg01g=150:cb01g=150
     cr01e=100:cg01e=220:cb01e=100
     cr0s= 100:cg0s= 220:cb0s= 100
     cr0g= 150:cg0g= 150:cb0g= 150
     cr0e= 100:cg0e= 220:cb0e= 100
     sc1=0
     SW.BREAK
    SW.CASE 6          % // RGB2 //
     cr01s=255:cg01s=  0:cb01s=  0
     cr01g=100:cg01g=100:cb01g=100
     cr01e=255:cg01e=  0:cb01e=  0
     cr0s= 255:cg0s=   0:cb0s=   0
     cr0g= 100:cg0g= 100:cb0g= 100
     cr0e= 255:cg0e=   0:cb0e=   0
     sc1=0
     SW.BREAK
   SW.END

   IF sig=1
    q01=q0+ABS(minp)
    IF pvt_ =q01 THEN GR.COLOR 255,cr01e,cg01e,cb01e,1 % // q1=q0
    IF ps/j>0.5&pg/j<0.5
     IF pvt_ <q01 THEN GR.COLOR 255,cr01s,cg01s,cb01s,sc1 % // q1<q0
     IF pvt_ >q01 THEN GR.COLOR 255,cr01g,cg01g,cb01g,0 % // q1>q0
    ELSE
     IF pvt_ <q01 THEN GR.COLOR 255,cr01g,cg01g,cb01g,0 % // q1<q0
     IF pvt_ >q01 THEN GR.COLOR 255,cr01s,cg01s,cb01s,sc1 % // q1>q0
    ENDIF

   ENDIF
   IF sig=2
    IF pvt_ =q0 THEN GR.COLOR 255,cr0e,cg0e,cb0e,1 % // q1=q0
    IF ps/j>0.5&pg/j<0.5
     IF pvt_ <q0 THEN GR.COLOR 255,cr0s,cg0s,cb0s,sc1  % // q1<q0
     IF pvt_ >q0 THEN GR.COLOR 255,cr0g,cg0g,cb0g,0 % // q1>q0
    ELSE
     IF pvt_ <q0 THEN GR.COLOR 255,cr0g,cg0g,cb0g,0  % // q1<q0
     IF pvt_ >q0 THEN GR.COLOR 255,cr0s,cg0s,cb0s,sc1 % // q1>q0
    ENDIF
   ENDIF

   ! ///////distribution/graph/rendering/////////////////////////////////
   GR.RECT rec,(sx/20) +xx0, (sy-sy/20), (sx-sx/20)-xx1, (sy/2+sy/20)-yy1
  NEXT

  ! ///////// distribution vector /////////////////////////////////////
  GR.TEXT.SIZE sx/35
  GR.TEXT.ALIGN 1
  SW.BEGIN scm
   SW.CASE 1:vcrs=  120:vcgs=  120:vcbs=  120:SW.BREAK
   SW.CASE 2:vcrs=  255:vcgs=   30:vcbs=   30:SW.BREAK
   SW.CASE 3:vcrs=   30:vcgs=  255:vcbs=   30:SW.BREAK
   SW.CASE 4:vcrs=   80:vcgs=   80:vcbs=   80:SW.BREAK
   SW.CASE 5:vcrs=  160:vcgs=  160:vcbs=  160:SW.BREAK
   SW.CASE 6:vcrs=  130:vcgs=  130:vcbs=  130:SW.BREAK
  SW.END

  GR.COLOR 255,vcrs,vcgs,vcbs,1

  stv=sy/23
  IF j<16
   FOR v=1 TO j
    GR.TEXT.ALIGN 1
    sgv_$=""
    IF pvt2[v]=q0 THEN sgv_$="  >"
    GR.TEXT.DRAW vec,sx/2+sx/5+sx/100,sy/2-sy/2.7+stv, STR$(ROUND(pvt2[v],2))+sgv_$

    IF j=m_
     GR.TEXT.ALIGN 3
     GR.TEXT.DRAW vec,sx-sx/15,sy/2-sy/2.7+stv, FORMAT$("%.###",pval2[v])
    ENDIF
    stv=stv+sy/77
   NEXT
  ELSE
   IF j<m_
    FOR v=1 TO 16
     GR.TEXT.DRAW vec,sx/2+sx/5+sx/100,sy/2-sy/2.7+stv, STR$(ROUND(pvt2[j-v+1],2))
     stv=stv+sy/77
    NEXT
   ELSE % // last vector M //
    swpvv=1
    FOR k=1 TO m_ % // q0 position //
     IF swpvv=1&pvt2[k]>=q0:swpvv=0:pvv=k:ENDIF
    NEXT
    pvv1=8 % // q0 in mid position  of output //
    IF pvv <=8 THEN pvv1=7-(8-pvv) % // k<8 //
    pvv=pvv-pvv1
    pmk=pvv+15
    IF m_-pvv <15 THEN pmk=pvv+(m_-pvv) % // k+8>M //
    FOR v=pvv TO pmk
     GR.TEXT.ALIGN 1
     IF v=pvv+pvv1 THEN sgv_$="  >" % // q0 mark //
     GR.TEXT.DRAW vec,sx/2+sx/5+sx/100,sy/2-sy/2.7+stv, STR$(ROUND(pvt2[v],2))+sgv_$
     sgv_$=""
     GR.TEXT.ALIGN 3
     GR.TEXT.DRAW vec,sx-sx/15,sy/2-sy/2.7+stv, FORMAT$("%.###",pval2[v])
     stv=stv+sy/77
    NEXT
   ENDIF
  ENDIF

  ARRAY.DELETE pvt1[]    % // delete tmp vector //
  !ARRAY.DELETE pvt2[]    % // delete tmp vector //
 ENDIF                   % // graph out sw      //

 ! //
 ! //////////// windows /////////////////////////////////////////////
 ! // head //
 IF scm=1:GR.COLOR 255,130,130,130,1:ENDIF % // default //
 IF scm=2:GR.COLOR 255, 80, 30, 30,1:ENDIF % // red     //
 IF scm=3:GR.COLOR 255, 30, 80, 30,1:ENDIF % // green   //
 IF scm=4:GR.COLOR 255,100,100,100,1:ENDIF % // inverted//
 IF scm=5:GR.COLOR 255,120,150,255,1:ENDIF % // RGB1    //
 IF scm=6:GR.COLOR 255, 50, 50,255,1:ENDIF % // RGB2    //
 GR.RECT rec, sx/20,sy/2+sy/12,sx-sx/20,sy/2+sy/20 %
 GR.RECT rec, sx/2+sx/5, sy/2-sy/2.95, sx-sx/20, sy/2-sy/2.7
 IF scm=1:GR.COLOR 255,180,180,180,1:ENDIF % // default //
 IF scm=2:GR.COLOR 255,255, 30, 30,1:ENDIF % // red     //
 IF scm=3:GR.COLOR 255, 30,255, 30,1:ENDIF % // green   //
 IF scm=4:GR.COLOR 255, 30, 30, 30,1:ENDIF % // inverted//
 IF scm=5:GR.COLOR 255,255,255,255,1:ENDIF % // RGB1    //
 IF scm=6:GR.COLOR 255,255,255,255,1:ENDIF % // RGB2    //

 GR.TEXT.ALIGN 1
 GR.TEXT.SIZE sx/30
 GR.TEXT.DRAW tx,sx/20+sx/100,sy/2+sy/14, met$+" - Distribution" 
 GR.TEXT.DRAW tx,sx/2+sx/5+sx/100,sy/2-sy/2.86, met$+" - Vector" 
 IF mode=2
  GR.TEXT.ALIGN 3
  GR.TEXT.DRAW tx,sx-sx/20-sx/100,sy/2+sy/14, fi$
 ENDIF
 ! // window frame //
 IF scm=1:GR.COLOR 255,165,165,165, 0:ENDIF % // default //
 IF scm=2:GR.COLOR 255,255, 30, 30, 0:ENDIF % // red //
 IF scm=3:GR.COLOR 255, 30,255, 30, 0:ENDIF % // green //
 IF scm=4:GR.COLOR 255, 80, 80, 80, 0:ENDIF % // inverted //
 IF scm=5:GR.COLOR 255, 100, 100, 100, 0:ENDIF % // RGB1 //
 IF scm=6:GR.COLOR 80, 230, 230, 230, 1:ENDIF % // RGB2 //

 GR.LINE ln, sx/20,sy-sy/20+yy/2+sy/45,sx-(sx/20),sy-sy/20+yy/2+sy/45 % // hor
 GR.LINE ln, sx/20+xx/2,sy-sy/20,sx/20+xx/2,sy/2+sy/12 % // vert
 IF scm=1:GR.COLOR 255,200,200,200,0:ENDIF % // default //
 GR.LINE ln, sx/20,sy/2+sy/12,sx-(sx/20),sy/2+sy/12 %
 GR.RECT ln, sx/2+sx/5, sy/2-sy/2.95, sx-sx/20, sy/2-sy/2.95
 GR.RECT rec, sx/20,sy-sy/20,sx-sx/20,sy/2+sy/20 %// bottom
 GR.RECT rec, sx/2+sx/5,sy/2-sy/8,sx-sx/20,sy/2-sy/2.7 %// top


 ! ////////////////////////////////////////////////////////
 ! //                                        Text rendering 
 IF (pvsw=1|msw=1)|((pvsw=-1&pvsw=-1)&(j=1|j>m_-1)) 
  !!
  IF scm=1:GR.COLOR 255,200,200,200,1:ENDIF % // default //
  !!
  GR.TEXT.BOLD 0
  GR.TEXT.ALIGN 3
  GR.TEXT.SIZE sx/20
  ! // meth, p<>, M //
  IF scm=1:GR.COLOR 255,220,220,220,1:ENDIF % // default //
  IF scm=2:GR.COLOR 255,255, 30, 30,1:ENDIF % // red     //
  IF scm=3:GR.COLOR 255, 30,255, 30,1:ENDIF % // green //
  IF scm=4:GR.COLOR 255,100,100,100,1:ENDIF % // inverted //
  IF scm=5:GR.COLOR 255,100,220,100,1:ENDIF % // RGB1 //
  IF scm=6:GR.COLOR 255,10,10,10,1:ENDIF % // RGB2 //

  IF (pvsw=-1) % // window sw //
   IF j=1
    GR.TEXT.DRAW tx,sx-sx/13,sy/30, "~m "+INT$(m)
   ENDIF
   IF j>m_-1
    GR.TEXT.DRAW tx,sx-sx/13,sy/30, "M="+INT$(m)
   ENDIF
  ENDIF
  IF (pvsw=1|msw=1)
   IF j < m_
    GR.TEXT.DRAW tx,sx-sx/13,sy/30, "m="+INT$(m-j) 
   ELSE
    GR.TEXT.DRAW tx,sx-sx/13,sy/30, "M="+INT$(m)
   ENDIF
  ENDIF
  GR.TEXT.ALIGN 1
  IF (pvsw=1|msw=1) |(pvsw=-1 & j>m_-1) % // window sw //
   IF ps>0
    GR.TEXT.DRAW tx, sx/11.7,1.8*(sy/25), " p>:"+FORMAT$("%.#####", ROUND(ps/j,5))+sgps$
   ENDIF
   IF pg>0
    GR.TEXT.DRAW tx,sx/11.7,3.2*(sy/25), " p<:"+FORMAT$("%.#####",round (pg/j,5))+sgpg$
   ENDIF
  ENDIF
  GR.TEXT.SIZE sx/12
  GR.TEXT.DRAW tx,-sx/108,sy/25, met$
  ! // main text, p= //
  IF scm=1:GR.COLOR 255,200,200,200,1:ENDIF % // default //
  IF scm=2:GR.COLOR 255,255, 30, 30,1:ENDIF % // red     //
  IF scm=3:GR.COLOR 255, 30,255, 30,1:ENDIF % // green   //
  IF scm=4:GR.COLOR 255, 80, 80, 80,1:ENDIF % // inverted//
  IF scm=5:GR.COLOR 255,90,90,90,1:ENDIF % // RGB1 //
  IF scm=6:GR.COLOR 255,120,120,120,1:ENDIF % // RGB2 //

  GR.TEXT.SIZE sx/20
  IF (pvsw=1|msw=1) |(pvsw=-1 & j>m_-1) % // window sw //
   IF pe>0
    IF meth = 2 | meth=4 | meth=6
     GR.TEXT.DRAW tx,sx/11.7,2.5*(sy/25), " p=:"+FORMAT$("%.#####", ROUND(pe/j,5))+FORMAT$(" [%.#",rp)+"]"
    ELSE
     GR.TEXT.DRAW tx,sx/11.7,2.5*(sy/25), " p=:"+FORMAT$("%.#####", ROUND(pe/j,5))
    ENDIF
   ENDIF
  ELSE 
   GR.TEXT.DRAW tx,sx/11.7,2.5*(sy/25), " p: Iterate over m ..."
  ENDIF

  IF 1/m >0.00001 % // pmin! //
   GR.TEXT.DRAW tx,sx/6.5,sy/25, FORMAT$("[ %.#####",1/m)+" ]" 
  ENDIF
  GR.TEXT.ALIGN 3
  IF thmode=1
   GR.TEXT.DRAW tx,sx-sx/13,2.8*(sy/40), "dAM[0]: "+STR$(round (ABS(q0),2))
  ENDIF
  IF thmode=2
   GR.TEXT.DRAW tx,sx-sx/13,2.8*(sy/40), "dSD[0]: "+STR$(round (ABS(q0),2))
  ENDIF
  GR.TEXT.DRAW tx,sx-sx/13,4*(sy/40), INT$(sig) +"-tailed" 
  GR.TEXT.SIZE sx/15
  GR.TEXT.ALIGN 1
  GR.TEXT.DRAW tx,5,sy-sy/100, _name$+": "+des$
  GR.TEXT.ALIGN 2
  GR.TEXT.SIZE sx/5
  GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2-sy/6.5, "A"
  IF (pvsw=1|msw=1) |(pvsw=-1 & j>m_-2) % // window sw //
   IF (pg/j<=0.05 | ps/j<=0.05) 
    IF sig=1
     IF ((am1>am2)&thmode=1)|((SQR(s21)>SQR(s22))&thmode=2)
      GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, ">"
     ELSE
      GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, "<"
     ENDIF
    ELSE
     GR.TEXT.DRAW tx,sx/2-sx/7,sy/2-sy/6.5, CHR$(8800)
    ENDIF
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

  IF thmode=1
   GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2,  STR$(ROUND(am1,2))
   GR.TEXT.DRAW tx,sx/2+sx/6.5-sx/7,sy/2,  STR$(ROUND(am2,2))
  ENDIF
  IF thmode=2
   GR.TEXT.DRAW tx,sx/2-sx/6.5-sx/7,sy/2,  STR$(ROUND(SQR(s21),2))
   GR.TEXT.DRAW tx,sx/2+sx/6.5-sx/7,sy/2,  STR$(ROUND(SQR(s22),2))
  ENDIF

  GR.TEXT.SIZE sx/20
  GR.TEXT.ALIGN 3
  GR.TEXT.DRAW tx,sx-sx/13,sy/2-sy/14.5, FORMAT$("p: %.###",bnp)
  IF thmode=1
   GR.TEXT.DRAW tx,sx-sx/13,sy/2,    FORMAT$("t:  %.###",twer)
  ENDIF
  GR.TEXT.ALIGN 1
  ! // pt //
  IF scm=1:GR.COLOR 255,220,220,220,1:ENDIF % // default //
  IF scm=2:GR.COLOR 255,255, 30, 30,1:ENDIF % // red     //
  IF scm=3:GR.COLOR 255, 30,255, 30,1:ENDIF % // green   //
  IF scm=4:GR.COLOR 255,100,100,100,1:ENDIF % // inverted//
  IF scm=5:GR.COLOR 255,100,220,100,1:ENDIF % // RGB1 //
  IF scm=6:GR.COLOR 255,10,10,10,1:ENDIF % // RGB2 //

  IF thmode=1
   GR.TEXT.DRAW tx,sx/2+sx/4.5,sy/2+sy/40, FORMAT$("p: %.###",pval)+sgpv$
  ENDIF
  ! // date //
  IF scm=1:GR.COLOR 255,200,200,200,1:ENDIF % // default //
  IF scm=2:GR.COLOR 255,255, 30, 30,1:ENDIF % // red     //
  IF scm=3:GR.COLOR 255, 30,255, 30,1:ENDIF % // green   //
  IF scm=4:GR.COLOR 255, 80, 80, 80,1:ENDIF % // inverted//
  IF scm=5:GR.COLOR 255,120,120,120,1:ENDIF % // RGB1 //
  IF scm=6:GR.COLOR 255,120,120,120,1:ENDIF % // RGB2 //
  GR.TEXT.SIZE sx/25
  GR.TEXT.ALIGN 3
  GOSUB systime
  GR.TEXT.DRAW tx,sx-sx/150,sy-sy/100, Y$+"."+M$+"."+D$+"/"+h$+":"+min$+":"+sec$
  ! //
  GR.RENDER
 ENDIF
 ! //
 GR.TOUCH tc,tx,ty
 IF tc: GOTO en: ENDIF
NEXT j        % // over cycles M           //
! //

IF swvec=1
 GOSUB distout % // distribution out stream //
ENDIF
IF swout=1
 GOSUB dwass   % // dwass efficiency        //
 GOSUB logout  % // log results out stream  //
ENDIF
IF swdal
 GOSUB datout  % // raw data out stream     //
ENDIF

en:           % // wait... //
FOR i=1 TO 1000000
NEXT
! //
DO            % // wait    //
 GR.TOUCH tc1,tx,ty
 IF tc1
  ARRAY.DELETE mP_12[]
  ARRAY.DELETE pvt[]
  ARRAY.DELETE m0_12[]

  GOTO st
 ENDIF
UNTIL 0
!! 
ONERROR: GOSUB fin: END
!!
ONMENUKEY:GOSUB dialog
MENUKEY.RESUME
ONBACKKEY:GOSUB fin
END

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
! //                                            Subroutines
rand:
INCLUDE sigma.inc
!!
rn=seed1^sgm
nx=1000*(rn-FLOOR(rn))-FLOOR(1000*(rn-FLOOR(rn)))
seed1=nx
!!
RETURN
! /////////////////////////////////////////////////////////
! //                                             q0, t-value
tval:
am1=s1/n1             % // arithmetic mean x1  //
am2=s2/n2             % // arithmetic mean x2  //
s21=0
FOR i=1 TO n1
 s21=s21+(n0_1[i]-am1)^2
NEXT
s21=s21/n1            % // sample variance s21 //
s22=0
FOR i=1 TO n2
 s22=s22+(n0_2[i]-am2)^2
NEXT
s22=s22/n2            % //sample variance s22 //

SW.BEGIN thmode
 SW.CASE 1
  q0=am1-am2            % // AM diff 0         //
  SW.BREAK
 SW.CASE 2
  q0=SQR(s21)-SQR(s22)  % // SD diff 0         //
  SW.BREAK
SW.END

twer=am1-am2
twer=twer/(SQR((s21*n1+s22*n2)/(n1+n2-2))*SQR(1/n1+1/n2))
q11=0
RETURN

! /////////////////////////////////////////////////////////
! //                                   t probability p(t,n) 
tp: 
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
xx_t = pval
yy = -5
IF xx_t = 0 THEN GOTO g
fx = (ABS(xx_t))
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
 xx_t = 0
 GOTO g
ENDIF
IF zz >= 90
 xx_t = 10^-20
 fx = zz
 zz = zz-20
ENDIF
ww = ABS(xx_t*POW(10,-zz ))
ww = FLOOR(ww*POW(10,yy)+0.5)
ww = ww*(POW(10,zz))*(POW(10,-yy))
IF fx >= 90
 ww = 10^20
ENDIF
IF xx_t >= 0
 xx_t = ww
ELSE
 xx_t = ww * -1
ENDIF
g:
pval = xx_t
pval =ABS(pval)*sig       % // tail //
sgpv$="" 
IF pval<=0.1 THEN sgpv$="+" 
IF pval<=0.05 THEN sgpv$="*" 
IF pval<=0.01 THEN sgpv$="**"
twer=ABS(w4)
RETURN

! /////////////////////////////////////////////////////////
! //                                 Binomial probabilities
binom:
q_b=1-p_b:bnp0=0
fa_n=1
FOR i=1 TO n1+n2:fa_n=fa_n*i:NEXT
FOR i=0 TO n1+n2
 fa_i=1
 FOR j=1 TO i:fa_i=fa_i*j:NEXT
 fa_ni=1
 FOR j=1 TO n1+n2-i:fa_ni=fa_ni*j:NEXT
 bpP0=(fa_n/(fa_i*fa_ni))*p_b^i*q_b^(n1+n2-i) 
 bnp0=bnp0+bpP0 
 IF i=n1
  bnp=bnp0              % // binomial probability       //
  bpP=bpP0              % // binomial Point probability //
 ENDIF
NEXT
bp1=bnp                 % // 1-tailed binomial p        //         
IF bp1>0.5:bp1=1-bp1
ENDIF
bp2=2*bp1               % // 2-tailed binomial p        //
RETURN

! /////////////////////////////////////////////////////////
! //                                       Dwass efficiency
dwass:
pdw=pg/m
IF pdw>ps/m THEN pdw=ps/m
IF pdw>0 & psg>0
 edp=  1-SQR((1-pdw)/pdw)*(1/SQR(m*2*PI()))
 edsg= 1-SQR((1-psg)/psg)*(1/SQR(m*2*PI()))
ENDIF
RETURN


! /////////////////////////////////////////////////////////
!//                                  n of permutations  wP
perm:
p_n=1: p_n1=1: p_n2=1
FOR i=1 TO n1+n2:p_n=p_n*i:NEXT
FOR i=1 TO n1:p_n1=p_n1*i:NEXT
FOR i=1 TO n2:p_n2=p_n2*i:NEXT
perm=p_n/(p_n1*p_n2) 
RETURN

! /////////////////////////////////////////////////////////
!//                               P vectors start position 
exact:
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

! /////////////////////////////////////////////////////////
! //                                               Dialogs

! ////////////////////////////////////////////////////////
! //                                            main menue
dlgmain:
smb$=CHR$(9989)
GOSUB menu
std:
ARRAY.LOAD main$[], o01$,o02$,o03$,o04$,o05$,o06$,o07$,"OK",o09$
DIALOG.SELECT main,main$[], _name$+" Permutation Methods Calculator "+_ver$
SW.BEGIN main
 SW.CASE 1:GOSUB dlgmode:SW.BREAK
 SW.CASE 2:GOSUB dlgtheta:SW.BREAK
 SW.CASE 3
  swtl=swtl*-1
  IF swtl=1:sig=1:ENDIF
  IF swtl=-1:sig=2:ENDIF
  SW.BREAK
 SW.CASE 4:GOSUB dlgmethod:SW.BREAK
 SW.CASE 5:GOSUB dlgm:SW.BREAK
 SW.CASE 6
  IF meth>2 
   INPUT "Seed (0 = timevalue)",seed,0
   IF seed=0:GOSUB dlgseed:ENDIF
   !seed1=seed
   __sd_=seed
  ENDIF:SW.BREAK
 SW.CASE 7:GOSUB dlgopt:SW.BREAK
  SW.CASE 8:IF n1&n2>0:RETURN
 ELSE:GOSUB dlgmode:ENDIF:SW.BREAK
 SW.CASE 9:GOSUB exit:IF exb=1:GOSUB fin:END:ENDIF:SW.BREAK
SW.END
GOSUB menu
GOTO std
RETURN

menu:
IF inpt=2:IF fi$<>"":o01$=smq$+" Input: "+fi$:ELSE
o01$="    Input off":ENDIF:ENDIF
IF inpt=1:IF des$<>"":o01$=smq$+" Input: "+des$:ELSE
o01$="    Input off":ENDIF:ENDIF
o02$=smq$+" Theta: "+thta$
o03$=smq$+" "+INT$(sig)+"-tailed"
o04$=smq$+" Method: "+met$
IF meth=2|meth=4|meth=6 %
 o04$=smq$+" Method: "+met$+" ["+FORMAT$("%.##", rp) +" ]" 
ENDIF
IF meth>2: o05$=smq$+" M: "+INT$(m) :ENDIF
IF meth<=2: o05$=smq$+" Exact" :ENDIF
IF meth>2
 IF seed>=1: o06$=smq$+" Seed: "+INT$(seed) : ENDIF
 IF seed<1: o06$=smq$+" Seed: TIME" : ENDIF
ELSE
o06$="    ______" : ENDIF
o07$="    Options..."
o09$="exit"
RETURN

! /////////////////////////////////////////////////////////
! //                                   Raw data input mode
dlgmode:
ARRAY.LOAD mod$[], "Input","File","cancel" 
DIALOG.SELECT mode,mod$[], "Select PRM Mode..."
SW.BEGIN mode 
 SW.CASE 1                          % // direct input    //
  inpt=1:GOSUB dirinp:SW.BREAK
 SW.CASE 2                          % // File input      //
  inpt=2:GOSUB dlginp:GOSUB filinp:SW.BREAK
 SW.CASE 3:RETURN:SW.BREAK
SW.END
RETURN

! ////////////////////////////////////////////////////////
! //                                        Dialog method
dlgmethod:
ARRAY.LOAD meth$[], "P","Pr","mP","mPr", "Bt", "Btr"
DIALOG.SELECT meth,meth$[], "Select PRM Method..."
SW.BEGIN meth
 SW.CASE 1:met$=" P":SW.BREAK
 SW.CASE 2:met$=" Pr":SW.BREAK
 SW.CASE 3:met$=" mP":SW.BREAK
 SW.CASE 4:met$=" mPr":SW.BREAK
 SW.CASE 5:met$=" Bt":SW.BREAK
 SW.CASE 6:met$=" Btr":SW.BREAK
SW.END
IF meth=2|meth=4|meth=6 
 GOSUB dlgmid     % // midp               //
ENDIF
IF meth<=2
 GOSUB perm
 m=perm           % // exact permutations //
ELSE
 m=m0             % // m reset            //
ENDIF
RETURN

! ////////////////////////////////////////////////////////
! //                                         direct input
DIRinp:
sw1g=0: sw2g=0: sw2x2=0 % design switches ini //
ARRAY.LOAD dsgn$[], "1 Group","2 Groups", "%2x2", "cancel" 
DIALOG.SELECT dsgn, dsgn$[], "Select PRM Design..."
SW.BEGIN dsgn
 SW.CASE 1
  sw1g=1        % // 1 group design  //
  des$= "(x)"
  SW.BREAK
 SW.CASE 2
  sw2g=1        % // 2 groups design //
  des$="(x|g)
  SW.BREAK
 SW.CASE 3
  sw2x2=1       % // 2x2 design      //
  des$="(x|g1|g2)
  SW.BREAK
 SW.CASE 4:RETURN:SW.BREAK
SW.END

IF sw2g=1 | sw1g=1
 INPUT "n(1)", n1, 5 % n1   % // input n1 //
 IF sw2g=1
  INPUT "n(2)", n2, 5 % n2  % // input n2 //
 ENDIF
 IF sw2x2=1
  ! //////////////2x2/design//////////////
 ENDIF
ENDIF
! //                                Direct input //
IF sw1g=1
 n2=n1
ENDIF
ng=n1
IF n2> ng THEN ng=n2      % // max group n switch //
DIM n0_1[ng] % Vector a
DIM n0_2[ng] % Vector b
DIM n0_12[n1+n2,2]        % // Raw data matrix X  //
! //                           data input, sum    //
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
 INPUT intx$, adif, 1       % // crit diff        //
 am0=am1-adif               % // crit mean        //
 FOR i=1 TO n2
  n0_2[i]=(n0_1[i]-am1)+am0 % // x1 transf to x2 with am0
  s2=s2+n0_2[i] % sum2
 NEXT
ENDIF
! //                    vectors x1, x2 to (x|g)  //
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
RETURN

! ////////////////////////////////////////////////////////
! //                                     dialog inputfile
dlginp:
DIM inp$[100]
FILE.DIR pth$, inp$[] 
SELECT ninp, inp$[], _name$+" Data Input File... ", "Select File" 
fi$=inp$[ninp] 
ARRAY.DELETE inp$[] 
RETURN

! ////////////////////////////////////////////////////////
! //                                           file input
filinp:
sw2x2=1                  % // design switches //
sw1g=0                   %
filedt$=pth$+fi$
DIM cin[2,2]             % // n counter ini   //
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
! // 
DIM n0_12[n_,3]           % // Raw data matrix X  //
TEXT.OPEN R, dat, filedt$ % // Raw data read file //
FOR j=1 TO n_
 TEXT.READLN dat, in$
 in1$="" % ini
 in2$="" %
 in3$="" %
 leng= LEN(in$)           % // string lenght of row //
 sw=0
 FOR i= 1 TO leng
  lin$= MID$(in$,i,1)
  IF lin$=" " 
   sw=sw+1                % // data position switch //
  ENDIF
  IF sw=0                 % // Column n=1, 1g design //
   in1$=in1$+lin$
   des$="(x)
  ENDIF
  IF sw=1                 % // Column n=2, 2g design //
   in2$=in2$+lin$
   des$="(x|g)
  ENDIF
  IF sw=2                 % // Column n=3, 2x2 design //
   in3$=in3$+lin$
   des$="(x|g1,g2)
  ENDIF
 NEXT i
 ! //                     Data conversion and readin //
 IF in$<>"EOF" 
  n0_12[j,1]= VAL(in1$)
  IF in2$<>"" 
   n0_12[j,2]= VAL(in2$)
  ELSE
   sw1g=1                 % // 1g design             //
  ENDIF
  IF in3$<>"" 
   n0_12[j,3]= VAL(in3$)
  ELSE
   sw2x2=0                % // 2g design             //
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
 ENDIF
NEXT j
TEXT.CLOSE dat 
! //                          File input, sum       //
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
IF meth<3
 GOSUB perm
 m=perm
ENDIF

ARRAY.DELETE cin[]
ARRAY.DELETE cin2[]


RETURN

! ////////////////////////////////////////////////////////
! //                                             Dialog M
dlgm:
IF meth>2
 INPUT "Simulation Cycles M", m, m1 % Simulation cycles M
 m0=m
ENDIF
RETURN


! ////////////////////////////////////////////////////////
! //                                        Dialog options
dlgopt:
opst:
GOSUB opt
ARRAY.LOAD opt$[],op00$,op01$,op02$,op03$,op04$,op05$,op06$,op07$,op08$,"OK"
DIALOG.SELECT opt,opt$[], _name$+" Options... "
SW.BEGIN opt
 SW.CASE 1:GOSUB scheme :GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 2:GOSUB dlgwnd :GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 3:GOSUB dlgofil:GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 4
  INPUT "Binomial p[exp]=", p_b, 0.5 % // binom design p //
  GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 5
  INPUT "Dwass p[crit]=", psg, 0.05 % // dwass p //
  GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 6
  INPUT "Default M", m1, 10000 % // Default Simulation cycles M
  GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 7:rnsw=rnsw*-1
  IF rnsw=1 :rnsw$="Sigma" :ENDIF
  IF rnsw=-1:rnsw$="System":ENDIF
  GOSUB opt:GOTO opst:SW.BREAK
 SW.CASE 8:RETURN:SW.BREAK
 SW.CASE 9:SW.BREAK
 SW.CASE 10:RETURN:SW.BREAK
SW.END
GOSUB opt
RETURN

opt:
op00$=smq$+" Scheme: "+scm$
op01$=smq$+" Windows: "+win1$+win2$+win3$
IF fo1$<>" [ ] "|fo2$<>" [ ] "|fo3$<>" [ ] "
 op02$=smq$+" Files: "+fo1$+fo2$+fo3$:ELSE
op02$="     Files off":ENDIF
op03$=smq$+" Binomial p[exp]: "+ FORMAT$("%.###",p_b)
op04$=smq$+" Dwass p[crit]: "+ FORMAT$("%.###",psg)
op05$=smq$+" Default M:  "+ INT$(m1)
op06$=smq$+" Random: "+ rnsw$
op07$="    Info"
op08$="    Reset"

RETURN

! /////////////////////////////////////////////////////////
! //                                          Dialog scheme
scheme:
GOSUB schem
ARRAY.LOAD scm$[], sc01$,sc02$,sc03$,sc04$,sc05$,sc06$ %,"cancel"
DIALOG.SELECT scm, scm$[], "Select Color Scheme... "
SW.BEGIN scm
 SW.CASE 1:scm=1:scm$=" Default":SW.BREAK
 SW.CASE 2:scm=2:scm$=" Red":SW.BREAK
 SW.CASE 3:scm=3:scm$=" Green":SW.BREAK
 SW.CASE 4:scm=4:scm$=" Inverted":SW.BREAK
 SW.CASE 5:scm=5:scm$=" RGB1":SW.BREAK
 SW.CASE 6:scm=6:scm$=" RGB2":SW.BREAK
  !SW.CASE 7:RETURN:SW.BREAK
SW.END
scsw=0
RETURN

schem:
IF scm=1:sc01$=smq$+" Default":ELSE
sc01$="    Default":ENDIF
IF scm=2:sc02$=smq$+" Red":ELSE
sc02$="    Red":ENDIF
IF scm=3:sc03$=smq$+" Green":ELSE
sc03$="    Green":ENDIF
IF scm=4:sc04$=smq$+" Inverted":ELSE
sc04$="    Inverted":ENDIF
IF scm=5:sc05$=smq$+" RGB1":ELSE
sc05$="    RGB1":ENDIF
IF scm=6:sc06$=smq$+" RGB2":ELSE
sc06$="    RGB2":ENDIF
RETURN

! ////////////////////////////////////////////////////////
! //                                       Dialog outfile
dlgofil:
ofil0:
GOSUB ofil
ARRAY.LOAD ofl$[], of01$,of02$,of03$,of04$,of05$,"OK"
DIALOG.SELECT ofl,ofl$[], "Select "+_name$+" File streams... "
SW.BEGIN ofl
 SW.CASE 1
  INPUT "Data Input File: " , fi$, "prm_indat.txt"
  GOTO ofil0:SW.BREAK
 SW.CASE 2
  swout=swout*-1
  fo1$=" [ ] "
  IF swout=1
   INPUT "Output File: " , fiout$, "prm_out.txt"
   GOSUB dlgout:fo1$=" [Out] "
  ENDIF
  GOTO ofil0:SW.BREAK
 SW.CASE 3
  swdal=swdal*-1
  fo2$=" [ ] "
  IF swdal=1
   INPUT "Data Log File: " , fidt$, "prm_outdat.txt"
   fo2$=" [Dat] "
  ENDIF
  GOTO ofil0:SW.BREAK
 SW.CASE 4
  swvec=swvec*-1
  fo3$=" [ ] "
  IF swvec=1
   INPUT "Distribution Vector File: ", fids$, "prm_pvt.txt"
   fo3$=" [Dist] "
  ENDIF
  GOTO ofil0:SW.BREAK
 SW.CASE 5
  INPUT "m[max]: ", mvd, 10000
  GOTO ofil0:SW.BREAK
 SW.CASE 6:RETURN:SW.BREAK
SW.END
GOSUB ofil
RETURN

ofil:
of01$=smq$+" Input: "+fi$
of02$=smq$+" Output: "+fiout$
IF swout=-1:of02$="     Output off":ENDIF
of03$=smq$+" Data Log: "+fidt$
IF swdal=-1:of03$="     Data Log off":ENDIF
of04$=smq$+" Distr. Vector: "+fids$
IF swvec=-1:of04$="     Distr. Vector off":ENDIF
of05$=smq$+" Vector m[max]: "+INT$(mvd)
RETURN

! ////////////////////////////////////////////////////////
! //                                        Dialog outfile
dlgout:
dlgout1:
GOSUB outf
ARRAY.LOAD ofl$[], ofl01$,ofl02$,ofl03$,ofl04$,"OK"
DIALOG.SELECT ofl,ofl$[], "Select Output Options... "
SW.BEGIN ofl
 SW.CASE 1:outsw1=outsw1*-1:GOTO dlgout1:SW.BREAK
 SW.CASE 2:outsw2=outsw2*-1:GOTO dlgout1:SW.BREAK
 SW.CASE 3:outsw3=outsw3*-1:GOTO dlgout1:SW.BREAK
 SW.CASE 4:outsw4=outsw4*-1:GOTO dlgout1:SW.BREAK
 SW.CASE 5:RETURN:SW.BREAK
SW.END
GOSUB outf
RETURN

outf:
IF outsw1=1:ofl01$=smq$+" Binomial p":ELSE
ofl01$="     Binomial p off":ENDIF
IF outsw2=1:ofl02$=smq$+" Raw Values x":ELSE
ofl02$="     Raw Values x off":ENDIF
IF outsw3=1:ofl03$=smq$+" Asymptotic p":ELSE
ofl03$="     Asymptotic p off":ENDIF
IF outsw4=1:ofl04$=smq$+" n<>=":ELSE
ofl04$="     n<>= off":ENDIF
RETURN

! ////////////////////////////////////////////////////////
! //                                        Dialog window
dlgwnd:
wnd0:
GOSUB wnd
ARRAY.LOAD wnd$[], wn01$,wn02$,wn03$,wn04$,"OK"
DIALOG.SELECT wnd,wnd$[], "Select "+_name$+" Window Mode... "
SW.BEGIN wnd
  SW.CASE 1:grsw=grsw*-1:IF grsw=1:win1$=" Dist~ "
 ELSE:win1$=" [Dist] ":ENDIF
  GOSUB wnd:GOTO wnd0:SW.BREAK
  SW.CASE 2:pvsw=pvsw*-1:IF pvsw=1:win2$=" p~ "
 ELSE:win2$=" [p] ":ENDIF
  GOSUB wnd:GOTO wnd0:SW.BREAK
  SW.CASE 3:msw=msw*-1:IF msw=1:win3$=" m~ "
 ELSE:win3$=" [m] ":ENDIF
  GOSUB wnd:GOTO wnd0:SW.BREAK
 SW.CASE 4:SW.BREAK
 SW.CASE 5:RETURN:SW.BREAK
SW.END
GOSUB wnd
RETURN

wnd:
IF grsw=1:wn01$=smq$+" Dynamic Distribution":ELSE
wn01$="     Dynamic Distribution off":ENDIF
IF pvsw=1:wn02$=smq$+" p values":ELSE
wn02$="     p values off":ENDIF
IF msw=1:wn03$=smq$+" M counter":ELSE
wn03$="     M counter off":ENDIF
wn04$=smq$+" Vector"
RETURN

! ////////////////////////////////////////////////////////
! //                                          Dialog theta
dlgtheta:
GOSUB thet
ARRAY.LOAD thet$[], ot01$,ot02$,ot03$,ot04$,ot05$,ot06$,"OK"
DIALOG.SELECT thet,thet$[], "Select Theta... "
SW.BEGIN thet
 SW.CASE 1:thta$="dAM":thmode=1:RETURN:SW.BREAK
 SW.CASE 2:thta$="dSD":thmode=2:RETURN:SW.BREAK
 SW.CASE 3:thta$="%dMd":thmode=3:RETURN:SW.BREAK
 SW.CASE 4:thta$=",%dMo":thmode=4:RETURN:SW.BREAK
 SW.CASE 5:thta$="%cor":thmode=5:RETURN:SW.BREAK
 SW.CASE 6:thta$="%%":thmode=6:RETURN:SW.BREAK
 SW.CASE 7:RETURN:SW.BREAK
SW.END
GOSUB thet
RETURN

thet:
IF thmode=1:ot01$=smq$+"AM Difference [dAM]":ELSE
ot01$="   AM Difference [dAM]":ENDIF
IF thmode=2:ot02$=smq$+"SD Difference [dSD]":ELSE
ot02$="   SD Difference [dSD]":ENDIF
ot03$="   %Md Difference [dMd]"
ot04$="   %Mo Difference [dMo]"
ot05$="   %Correlation r [cor]"
ot06$="   %r____"
RETURN

! ////////////////////////////////////////////////////////
! //                                          Dialog Seed
dlgseed:
GOSUB systime
seed=INT(SQR(VAL(sec$))*1000) % // timeseed //
RETURN

! ////////////////////////////////////////////////////////
! //                                         dialog mid p
dlgmid:
INPUT "Midp=" , rp, 0.5 % // midp /////////
RETURN

! ////////////////////////////////////////////////////////
! //                                          File output

! ////////////////////////////////////////////////////////
! //                          Distribution vector file out
distout:
ARRAY.SORT pvt[]
fileot$=pth$+fids$
TEXT.OPEN A, out, fileot$
GOSUB systime
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
GOSUB head
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
FOR i=1 TO mv
 IF pvt[i] < q0
  TEXT.WRITELN out, ROUND(pvt[i], 4);_tb$;"-<" 
 ENDIF
 IF pvt[i] > q0
  TEXT.WRITELN out, ROUND(pvt[i],4) ;_tb$;">-" 
 ENDIF
 IF pvt[i] = q0
  TEXT.WRITELN out, ROUND(pvt[i], 4);_tb$;"*=*" 
 ENDIF
NEXT
GOSUB bottom
TEXT.CLOSE out
RETURN

! ////////////////////////////////////////////////////////
! //                                         Log file out 
logout:
ms$=" M:"+INT$(m)
pmn$=" pmin:"+_tb$+FORMAT$("%.###",1/m)
pb$=" pb:"+_tb$+FORMAT$("%.###",bnp)+" ["+STR$(ROUND(p_b,2))+"]"
pbp$=" Pb:"+_tb$+FORMAT$("%.###",bpP)
pb1$=" pb1:"+_tb$+FORMAT$("%.###",bp1)
pb2$=" pb2:"+_tb$+FORMAT$("%.###",bp2)
tail$=" "+INT$(sig)+"-tailed" 
ps$=" p>:"+_tb$+FORMAT$("%.###",ps/m)+sgps$
pe$=" p=:"+_tb$+FORMAT$("%.###",pe/m)
pg$=" p<:"+_tb$+FORMAT$("%.###",pg/m)+sgpg$

dif$="" 
IF sig=1
 IF ps/m<=0.05|pg/m<=0.05
  SW.BEGIN thmode
   SW.CASE 1
    IF am1>am2: dif$=">" :ENDIF
    IF am1<am2: dif$="<" :ENDIF
    SW.BREAK
   SW.CASE 2
    IF SQR(s21)>SQR(s22): dif$=">" :ENDIF
    IF SQR(s21)<SQR(s22): dif$="<" :ENDIF
    SW.BREAK
  SW.END
 ENDIF
ENDIF
IF sig=2
 IF ps/m<=0.05|pg/m<=0.05:dif$=CHR$(8800):ENDIF
ENDIF
tw$=" t:"+_tb$+FORMAT$("%.###",ABS(twer))
sgi=pval:GOSUB sigbr
pt$= " pt:"+_tb$+FORMAT$("%.###",  pval)+brs$
pt2$=" qt:"+_tb$+FORMAT$("%.###",1-pval)+brs$

edp$=" eDp:"+_tb$+FORMAT$("%.###",edp)
eds$=" eD :"+_tb$+FORMAT$("%.###",edsg)+" ["+STR$(ROUND(psg,2))+"]" 
sed$="seed: "+INT$(seed)

fileot$=pth$+fiout$
TEXT.OPEN A, out, fileot$
GOSUB lin% // - - - - - - - - - - - - - - - - - - - - -
GOSUB systime
GOSUB head
GOSUB lin% // - - - - - - - - - - - - - - - - - - - - -
ng=n1
IF n2 > ng THEN ng=n2
TEXT.WRITELN out," ";_tb$;"A";_tb$;_tb$;"B"
TEXT.WRITELN out," n";_tb$;INT$(n1);_tb$;_tb$;INT$(n2)
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
IF outsw1=1 % // binomial out //
 TEXT.WRITELN out, pb$  % // binomial p //
 TEXT.WRITELN out, pbp$ % // binomial P //
 TEXT.WRITELN out, pb1$ % // binomial p1 //
 TEXT.WRITELN out, pb2$ % // binomial p2 //
 TEXT.WRITELN out, ""
ENDIF
IF outsw2=1 % // raw values out //
 FOR i=1 TO ng
  IF i <= n1 & i <= n2
   TEXT.WRITELN out," "; INT$(i);_tb$;STR$(n0_1[i]);_tb$;_tb$;STR$(n0_2[i])
  ENDIF
  IF i > n1 
   TEXT.WRITELN out," "; INT$(i);_tb$;_tb$;_tb$;STR$(n0_2[i])
  ENDIF
  IF i > n2
   TEXT.WRITELN out," "; INT$(i);_tb$;STR$(n0_1[i])
  ENDIF
 NEXT
 TEXT.WRITELN out, ""
ENDIF
SW.BEGIN thmode
 SW.CASE 1
  TEXT.WRITELN out," AM";_tb$;STR$(ROUND(am1,2));_tb$;dif$;_tb$;STR$(ROUND(am2,2))
  TEXT.WRITELN out," SD";_tb$;STR$(ROUND(SQR(s21),2));_tb$;_tb$;STR$(ROUND(SQR(s22),2))

  TEXT.WRITELN out," dAM";_tb$;STR$(ABS(ROUND(q0,2)))
  SW.BREAK
 SW.CASE 2
  TEXT.WRITELN out," AM";_tb$;STR$(ROUND(am1,2));_tb$;_tb$;STR$(ROUND(am2,2))
  TEXT.WRITELN out," SD";_tb$;STR$(ROUND(SQR(s21),2));_tb$;dif$;_tb$;STR$(ROUND(SQR(s22),2))

  TEXT.WRITELN out," dSD";_tb$;STR$(ABS(ROUND(q0,2)))
  SW.BREAK
SW.END
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
TEXT.WRITELN out, tail$ % // tail  //
IF outsw3=1&thmode=1  % // asymptotic p, dAM //
 TEXT.WRITELN out, ""
 TEXT.WRITELN out, tw$   % // t  //
 TEXT.WRITELN out, pt$   % // pt //
 IF sig=2
  TEXT.WRITELN out, pt2$ % // qt //
 ENDIF
ENDIF
ENDIF
TEXT.WRITELN out, "" 
TEXT.WRITELN out, ps$   % // p< //
IF meth = 2 | meth=4 | meth=6
 TEXT.WRITELN out, pe$ +FORMAT$("[%.#",rp);"]"
ELSE
 TEXT.WRITELN out, pe$  % // p=   //
ENDIF
TEXT.WRITELN out, pg$   % // p>   //
TEXT.WRITELN out, pmn$  % // pmin //
IF meth=3| meth=4 % // dwass at mP,mPr
 TEXT.WRITELN out, "" 
 TEXT.WRITELN out, eds$  % // dwass //
 TEXT.WRITELN out, edp$  % // dwass //
ENDIF
GOSUB lin  % // - - - - - - - - - - - - - - - - - - - - - 
IF meth <=2
 TEXT.WRITELN out, " Method: ";met$;"/";ms$
ELSE
 TEXT.WRITELN out, " Method: ";met$;"/";ms$;"/ ";sed$;"/ ";rnsw$
ENDIF

TEXT.WRITELN out, "" 
GOSUB bottom
TEXT.CLOSE out
RETURN

! ///////////////////////////////////////////////////////
! //                                        Dat file out 
datout:
fileot$=pth$+fidt$
TEXT.OPEN A, out, fileot$
GOSUB systime
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
GOSUB head
GOSUB lin % // - - - - - - - - - - - - - - - - - - - - -
FOR i=1 TO n1
 TEXT.WRITELN out, STR$(n0_1[i])+" 1"
NEXT
FOR i=1 TO n2
 TEXT.WRITELN out, STR$(n0_2[i])+" 2"
NEXT
GOSUB bottom
TEXT.CLOSE out
RETURN

! ///////////////////////////////////////////////////////
! //                                  Outputfil routines

sigbr:
brs$="" % // sig markings for output
IF sgi <= 0.1 THEN brs$="+"
IF sgi <= 0.05 THEN brs$="*"
IF sgi <= 0.01 THEN brs$="**"
IF sgi <= 0.001 THEN brs$="***"
RETURN
! //
systime:
TIME Y$, M$, D$, h$, min$, sec$
RETURN
! //
head:
TEXT.WRITELN out,_name$;" ";_ver$;_tb$;_tb$;_tb$;"/" +Y$+"."+M$+"."+D$+"/"+h$+":"+min$+":"+sec$+"/"
RETURN
! //
lin:
ln$=""
FOR i= 1 TO 45: ln$=ln$+"-":NEXT
TEXT.WRITELN out, ln$
RETURN
! //
bottom:
TEXT.WRITELN out,"-"
RETURN
! //
EXIT:
DIALOG.MESSAGE _name$+" "+_ver$, "Exit Program... ?", exb, "Yes", "No"
RETURN
! //
! ///////////////////////////////////////////////////////
! //                                      Write ini file
fin:
TEXT.OPEN w, f_ini, "prm.ini"
TEXT.WRITELN f_ini, inf
TEXT.WRITELN f_ini, pth$
TEXT.WRITELN f_ini, p_b
TEXT.WRITELN f_ini, rp
TEXT.WRITELN f_ini, psg
TEXT.WRITELN f_ini, rnsw
TEXT.WRITELN f_ini, sig
TEXT.WRITELN f_ini, seed
TEXT.WRITELN f_ini, meth
TEXT.WRITELN f_ini, met$
TEXT.WRITELN f_ini, m0
TEXT.WRITELN f_ini, fi$
TEXT.WRITELN f_ini, mode
TEXT.WRITELN f_ini, thta$
TEXT.WRITELN f_ini, thmode
TEXT.WRITELN f_ini, rnsw$
TEXT.WRITELN f_ini, fiout$
TEXT.WRITELN f_ini, fidt$
TEXT.WRITELN f_ini, fids$
TEXT.WRITELN f_ini, grsw
TEXT.WRITELN f_ini, pvsw
TEXT.WRITELN f_ini, msw
TEXT.WRITELN f_ini, mvd
TEXT.WRITELN f_ini, inpt
TEXT.WRITELN f_ini, swout
TEXT.WRITELN f_ini, swdal
TEXT.WRITELN f_ini, swvec
TEXT.WRITELN f_ini, m1
TEXT.WRITELN f_ini, fo1$
TEXT.WRITELN f_ini, fo2$
TEXT.WRITELN f_ini, fo3$
TEXT.WRITELN f_ini, win1$
TEXT.WRITELN f_ini, win2$
TEXT.WRITELN f_ini, win3$
TEXT.WRITELN f_ini, scm$
TEXT.WRITELN f_ini, scm
TEXT.WRITELN f_ini, outsw1
TEXT.WRITELN f_ini, outsw2
TEXT.WRITELN f_ini, outsw3
TEXT.WRITELN f_ini, outsw4
TEXT.CLOSE f_ini
CONSOLE.TITLE _name$
PRINT_name$+" Permutation Methods Calculator "+_ver$
PRINT"Copyright "+_cr$+" 2023 by Dietmar Gerald Schrausser"
PRINT"https://github.com/Schrausser/PRM"
RETURN
! END //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
! //
