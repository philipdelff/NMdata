$PROBLEM    OS model
;@ Variables 35/20 @;
$INPUT ID ; unique patient identifier
TIME ; time in days
DV ;
EVID ;
BSLD ; Observed baseline SLD
IBAS3 IEC53 ; EBEs from final sVEGFR-3 model (Base, AUC50)
IBASS IKG ILAM ; EBEs from final SLD model (SLD0, KG, Lambda)

$DATA  ../data/input_data_debug.csv IGNORE=#
$SUBROUTINE ADVAN6 TOL=6

$MODEL      NCOMP=3

$THETA  (0,7.09)  ; Beta0_OS
$THETA  (0,0.298) ; Gamma_OS
$THETA  0.0115    ; Beta_SLD
$OMEGA 0 FIX ; dummy

$PK
;=========== sVEGFR-3 ================
EMAX    =         1                    ; Imax
BASE3   =         IBAS3                ; Base_sVEGFR3
MRT3    =         5.76                 ; MRT_sVEGFR3
EC503   =         IEC53                ; AUC50_sVEGFR3
KOUT3   =         1/MRT3               ; kout_sVEGFR3
KIN3    =         BASE3*KOUT3          ; Rin_sVEGFR3
EFF3    =         EMAX*440/(EC503+440) ; Emax model

;=========== SLD =====================
IBASE   =        IBASS                ; SLD0
KG      =        IKG                  ; KG
LAMBDA  =        ILAM                 ; Lambda
KD3     =        174/7/1000           ; K_sVEGFR3

;=========== SURVIVAL ================
B0H     =        THETA(1)+ETA(1)      ; Beta0_OS, with dummy ETA
GAMH    =        THETA(2)             ; Gamma_OS

;===== Compartment initialization ====
A_0(1)  =        BASE3
A_0(2)  =        IBASE

$DES
;=========== sVEGFR-3 and SLD ========
DADT(1) =   KIN3*(1-EFF3)-KOUT3*A(1)  ; sVEGFR-3
CFBV3   =   ((A(1)-BASE3)/BASE3)      ; relative change in sVEGFR-3 from baseline
DADT(2) =   KG*A(2)-(-CFBV3*KD3)*EXP(-(LAMBDA*T))*A(2) ; SLD

;=========== Time-to-event models ====
PSIH    = EXP(-B0H)      ; Psi_OS
DADT(3) = PSIH**(1/GAMH)*T**(1/GAMH-1)/(GAMH*(1+(PSIH*T)**(1/GAMH)))*EXP(THETA(3)*A(2)) ; hazard OS

$ERROR
REPI = IREP
VEGFR = A(1)
TUM     = A(2)
CHZ     = A(3)           ; cumulative hazard for survival

SUR     = EXP(-CHZ)      ; survival probability

PSIHX   = EXP(-B0H)      ; Psi_OS


IF(TUM.GT.838) TUM=838
HAZNOW  = PSIHX**(1/GAMH)*TIME**(1/GAMH-1)/(GAMH*(1+(PSIHX*TIME)**(1/GAMH)))*EXP(THETA(3)*TUM) ; hazard OS

IF(EVID.EQ.0.AND.DV.EQ.0) Y = SUR          ; probability of surviving until time=TIME
IF(EVID.EQ.0.AND.DV.EQ.1) Y = SUR*HAZNOW   ; probability of event at time=TIME

$OMEGA 0 FIX ; dummy

$ESTIMATION MAXEVAL=9999 METHOD=1 LAPLACIAN LIKE NUMERICAL SLOW NOABORT
$COV MATRIX=R

;@ Pred Variables 10/20 @;
$TABLE ID          TIME     VEGFR            TUM  DV    BSLD
       IBAS3       IEC53    IBASS            IKG  ILAM  CHZ
       ONEHEADER  NOPRINT  FORMAT=s1PE13.6  FILE=simvpcdata_debug.csv
