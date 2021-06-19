;; 1. Based on: min035
;; 2. Description: transit compartment on insulin effect
;; x1. Author: user
$PROBLEM    using different data than min036
;| Variables 9/20 |;

$INPUT ROW ,ID ,TIME ,EVID, CMT ,AMT RATE DV MDV ROUTE DOSEINS
DOSEINSPORTAL DOSEIVG DOSEOG ESTGLUCIVINS ESTINSIV

$DATA      ../data/sims1_for_minmodel_insPK_mod.csv IGNORE=@
            IGNORE=(ESTGLUCIVINS.NE.1)

;| General Nonlinear Model                                         |;
$SUBROUTINE ADVAN6 TOL=3
$MODEL      NCOMP=6 COMP=(PORTAL) COMP=(INSCEN) COMP=(GLUC)
            COMP=(INSPER) COMP=(INSACT) COMP=(INSDELAY)

$PK
TVVI1 = THETA(1)
TVCLI = THETA(2)
TVVI2 = THETA(3)
TVQI  = THETA(4)
FFP   = THETA(5)
K12   = THETA(6)
D2    = THETA(7)

VI1 = TVVI1*EXP(ETA(1))
CLI = TVCLI*EXP(ETA(2))
VI2 = TVVI2*EXP(ETA(3))
QI  = TVQI *EXP(ETA(4))
;; assuming no insulin at baseline
I0=0

;; glucose parameters
TVB1 = EXP(THETA(8))
TVB2 = EXP(THETA(9))
TVB3 = EXP(THETA(10))
KI   = EXP(THETA(11))

B1 = TVB1*EXP(ETA(8))
B2 = TVB2*EXP(ETA(9))
B3 = TVB3*EXP(ETA(10))

; for now, baseline glucose is estimated offline
G0=11.256
A_0(3)=G0
 
$DES
  DADT(1) = -FFP * K12 * A(1)
  DADT(2) = FFP*K12*A(1) + QI*(A(4)/VI2-A(2)/VI1) - A(2)/VI1*CLI 
;; glucose conc
   DADT(3) = B1*(G0-A(3))-A(5)*A(3)
;; peripheral insulin
   DADT(4) = QI*(A(2)/VI1-A(4)/VI2)
;; insulin action
DADT(5) = -B2*A(5) + B3*(A(6)/VI1-I0)
DADT(6) = KI * (A(2) - A(6))
   

$ERROR
  IPRED=A(3)
  IRES=DV-IPRED

;  Y=A(2)/VI1+ERR(1)
Y=IPRED+EPS(1)
; Y=IPRED+IPRED*EPS(1)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA
(0, 3.28) FIX ; 1 - TVVI1
(0, 1.16) FIX ; 2 - TVCLI
(0, 4.19) FIX ; 3 - TVVI2
(0, 0.243) FIX ; 4 - TVQI
(0, 1) FIX ; 5 - FFP
(0, 1) FIX ; 6 - K12
(1) FIX ; 7 - D2
(-4.51) ; B1
(-2.91) ; B2
(-13.3) ; B3
(-2.42) ; log KI 

$OMEGA 0 FIX 
$OMEGA 0 FIX 
$OMEGA 0 FIX 
$OMEGA 0 FIX 
$OMEGA 0 FIX 
$OMEGA 0 FIX 
$OMEGA 0 FIX 
;; PD
$OMEGA .2
$OMEGA .2
$OMEGA .2

$SIGMA 0.0142
; $SIGMA 0 FIX

; $ESTIMATION MAXEVALS=9999 NOABORT INTERACTION POSTHOC
$ESTIMATION NSIG=3 MAX=9999 NOABORT PRINT=5 METHOD=1 INTERACTION

;@ Pred Variables 7/20 @;
$TABLE ROW TVCLI TVVI1 TVVI2 TVQI CLI VI1 VI2 B1 B2 B3
ETAS(1:LAST) IRES PRED IPRED CWRES NOPRINT FILE=min036_res.txt
