$PROBLEM    031 SAEM

;; $INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY
;; BLQ CYCLE DOSE PART PROFDAY PROFTIME WEIGHTB eff0
$INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY AGE BLQ CYCLE
DOSE MALEN PART PROFDAY PROFTIME WEIGHTB

$DATA     ../data/xgxr2covs.csv IGNORE=@ IGNORE=(FLAG.NE.0) IGNORE(DOSE.LT.30)

$SUBROUTINE ADVAN4 TRANS4

$PK
LTVKA=THETA(1)
LTVV2=THETA(2)
LTVCL=THETA(3)
LTVV3=THETA(4)
LTVQ=THETA(5)

MU_1=LTVKA
KA=EXP(MU_1+ETA(1))
MU_2=LTVV2
V2=EXP(MU_2+ETA(2))
AGEEFF = THETA(6)
MU_3=LTVCL+ AGEEFF * LOG(AGE/54)
CL=EXP(MU_3+ETA(3))
MU_4=LTVV3
V3=EXP(MU_4+ETA(4))
MU_5 = LTVQ
Q =EXP(MU_5+ETA(5))
S2=V2

$ERROR
  IPRED=F
IRES=DV-IPRED
SIGP=SIGMA(1,1)
SIGA=SIGMA(2,2)

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGP + SIGA)
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=F+F*ERR(1)+ERR(2)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA  (.1)             ; 1 : LTVKA ; Absorption rate [1/h] ; log
$THETA  (3)             ; 2 : LTVV2 ;  Central volume [L] ; log
$THETA  (1)             ; 3 : LTVCL ; Clearance [L/h] ; log
$THETA  (4)             ; 4 : LTVV3 ; Peripheral volume [L] ; log
$THETA  (-1)             ; 5 : LTVQ ; Intercomparmental clearance [L/h] ; log
$THETA .1              ; 6 : AGEEFF ; Age effect on clearance []; log

$OMEGA 0 FIX ; 1 : BSV.KA ; KA Between-subject variability
$OMEGA BLOCK(2)
0.1   ; 2 : BSV.V2 ; V2 Between-subject variability
 0.01   ; 2-3 : Cov.BSV.V2-CL ; V2 Between-subject variability
 0.1   ; 3 : BSV.CL ; CL Between-subject variability
$OMEGA 0 FIX ; 4 : BSV.V3 ; V3 Between-subject variability
$OMEGA 0 FIX ; 5 : BSV.Q  ; Q Between-subject variability

$SIGMA 0.1    ; SigP - Prop err
$SIGMA 0 FIX  ; SigA - Add err 


$ESTIMATION METHOD=SAEM INTERACTION NOABORT NBURN=1000 NITER=1000 CTYPE=3 MAX=99999 NSIG=3 SEED=3442 PRINT=10 RANMETHOD=P MSFO=xgxr133.msf

$ESTIMATION METHOD=IMP INTERACTION EONLY=1 NITER=20 PRINT=1 ISAMPLE=5000 RANMETHOD=P
																	    
$COV PRINT=E

$TABLE ROW KA V2 V3 CL Q PRED IPRED Y NOPRINT FILE=xgxr133_res.txt
$TABLE ETAS(1:LAST) NOAPPEND NOPRINT FILE=xgxr133_etas.txt
