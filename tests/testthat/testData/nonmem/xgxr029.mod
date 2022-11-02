;; test of NMscanInput with multiple ignores on same column. Not to be
;; run with Nonmem.

$PROBLEM    PK

;@ Variables 17/20 @;
$INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY
BLQ CYCLE DOSE PART PROFDAY PROFTIME WEIGHTB eff0

$DATA     ../data/xgxr1.csv IGNORE=@ IGNORE=(FLAG.NE.0)
IGNORE=(ID.EQ.34)
IGNORE=(ID.EQ.35)
IGNORE=(ID.EQ.36)
