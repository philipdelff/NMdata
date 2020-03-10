$PROBLEM    PK
$INPUT      PKFL DV ID FLAG PERIOD TAD TIME AMT EVID CMT MDV TRTPN
            TRT1 TRT2 TRT3 TRT4
$DATA      PK.csv IGNORE=@ IGNORE=(PKFL.EQ.0)
            IGNORE=(TRTPN.EQ.1) IGNORE=(TRTPN.EQ.3)
            IGNORE=(TRTPN.EQ.4)
$SUBROUTINE ADVAN6 TOL=6
$MODEL      NCOMP=4 COMP=(cent) ;; central compartment parent
            COMP=(centmet) ;; central compartment metabolite
            COMP=(peri) ;; peripheral compartment parent
            COMP=(perimet) ;; peripheral compartment metabolite

;--------------------------
$PK

"FIRST
" COMMON/PRCOMG/ IDUM1,IDUM2,IMAX,IDUM4,IDUM5
"INTEGER IDUM1,IDUM2,IMAX,IDUM4,IDUM5
"IMAX=1000000

   ;# Set up dosing times
   IF(NEWIND.LE.1) THEN
      TDOS= -1000
      PD=0
   ENDIF
   IF(AMT.GT.0) THEN
      TDOS = TIME ; If AMT > 0, set TDOS to TIME
      PD = AMT    ; If AMT > 0, update PD
   ENDIF
   TAD2 = TIME-TDOS ; Set time after dose

   E1=EXP(ETA(1))
   E2=EXP(ETA(2))
   E3=EXP(ETA(3))
   E4=EXP(ETA(4))
   E5=EXP(ETA(5))
   E6=EXP(ETA(6))
   E7=EXP(ETA(7))


   TVCL=THETA(1)
   CL=TVCL*E1

   TVV1=THETA(2)
   V1=TVV1

   TVV2=THETA(3)
   V2=TVV2

   TVQ1=THETA(4)
   Q1=TVQ1*E2

   TVCLM=THETA(8)
   CLM=TVCLM*E3

   TVVM1=THETA(9)
   VM1=TVVM1

   TVVM2=THETA(10)
   VM2=TVVM2*E4

   TVQ2=THETA(11)
   Q2=TVQ2

   MTT=THETA(6)*E5
   NT=1+THETA(15)*E6
   KTR=NT/MTT
   NN=NT-1

   ;;-- Approximation of log(factorial(NN))
   IF(NN.GE.0.558) THEN
      L = LOG(2.5066)+(NN+.5)*LOG(NN)-NN+LOG(1+1/(12*NN))
   ELSE
      L= -0.1171157 + 0.5690*((0.558 - NN)**2) - 0.1122*(0.558 - NN)
   ENDIF

   K13=Q1/V1
   K31=Q1/V2

   TVfm=0.22
   fm=TVfm

   K10=CL*(1-fm)/V1
   K12=fm*CL/V1

   K24=Q2/VM1
   K42=Q2/VM2
   K20=CLM/VM1

   S1=V1
   S2=VM1
   F1=0
   BIO=THETA(7)*E7

   X=0.0000001
$DES
   IF(T.GE.TDOS) THEN
      DADT(1)= BIO*PD*KTR*EXP(NN*LOG(KTR*(T-TDOS) + X) - KTR*(T - TDOS)-L)  - k10*A(1) - k12*A(1) - k13*A(1)+k31*A(3)
   ELSE
      DADT(1)=  - k12*A(1) - k13*A(1)+k31*A(3)
   ENDIF
   DADT(2)=  k12*A(1) - k20*A(2) - k24*A(2)+k42*A(4)
   DADT(3)=  k13*A(1)-k31*A(3)
   DADT(4)=  k24*A(2)-k42*A(4)

$ERROR

   STRT=FLAG

   ;;Separate residual error for parent and the active metabolite

   IF(FLAG.LE.1) THEN
      IPRED=A(1)/S1
      W=sqrt( (theta(5)*IPRED)**2 + theta(13)**2 )  ;;prop and add  error parent
      Y=IPRED+W*EPS(1)
   ELSE
      IPRED=A(2)/S2
      W=sqrt( (theta(12)*IPRED)**2 + theta(14)**2 )  ;;prop and add  error metabolite
      Y=IPRED+W*EPS(1)
   ENDIF

   CP=A(1)/S1
   CM=A(2)/S2

   DEL=0
   IF (W.EQ.0) DEL=1
   IRES=DV-IPRED
   IWRES=IRES/(W+DEL)

;--------------------------
$THETA  (0,16.5964) ; 1 CL L/h parent
 (0,144.673) ; 2 V1 L parent
 (0,68.9777) ; 3 V2 L parent
 (0,15.7851) ; 4 Q1 parent
 (0,0.36446) ; 5  ERROR parent
 (0,1.00) ; 6 MTT
 1 FIX ; 7 F1
 (0,10.387) ; 8 CLM metabolite
 (0,7.53503) ; 9 VM1 L metabolite
 (0,43.3393) ; 10 VM2 L metabolite
 (0,4.55635) ; 11 Q2 metabolite
 (0,0.279073) ; 12 ERROR metabolite
 (0,3) ; 13 Add ERROR parent
 (0,3) ; 14 Add ERROR metabolite
 (1,5,12) ; 15 NTR
;--------------------------
$OMEGA  0.0558419  ; 1 CL parent
$OMEGA  0.193658  ; 2 Q1 parent
$OMEGA  0.0857882  ; 3 CL metabolite
$OMEGA  0  FIX  ; 4 V2 metabolite
$OMEGA  0.321524  ;      5 KTR
$OMEGA  0  FIX  ;      6 NTR
$OMEGA  0.0975457  ; 7 Bioavailability
;--------------------------
$SIGMA  1  FIX
;--------------------------
$ESTIMATION SIGDIG=3 NOABORT PRINT=5 MAXEVAL=9999 METHOD=1 INTERACTION
            SIGL=15
$COVARIANCE PRINT=E
$TABLE      PKFL DV ID FLAG PERIOD TAD TIME AMT EVID CMT MDV TRTPN
            IPRED IWRES RES CWRES CL V1 V2 Q1 F1 CLM fm VM1 VM2 Q2 KTR
            MTT NT ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7 NOPRINT
            ONEHEADER file=run001.txt
