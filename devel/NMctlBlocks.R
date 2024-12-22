text <- c("
; matches format
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  0.38  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
")

lines <- strsplit(text,split="\n")[[1]]

lines


om <- NMreadSection(lines=lines,section="OMEGA",keep.comments=T,as.one=T,keep.empty=T,keep.name=F)

dt.lines <- data.table(text=om)[,row:=.I]
## remove comments
dt.lines[,code:=sub(";.*","",text)]
## make "0 . 3" into "0.3"
dt.lines[,code:=gsub(" *\\. *",".",code)]
dt.lines[,code:=gsub("( *","(",code)]
dt.lines[,code:=gsub(" *)",")",code)]
dt.lines[,code:=gsub(" *, *",",",code)]
### not needed: derive (lower,init,upper)
## remove SAME - this should rather remover everything that is not either BLOCK(), a numeric or (numerics,)
dt.lines[,code:=gsub("SAME","",code)]
dt.lines[,code:=gsub("FIX","",code)]
## remove extra spaces
dt.lines[,code:=cleanSpaces(code)]
all.elements <- paste(dt.lines[,code], collapse=" ")

dt.lines[,code.blocks:=list(regmatches(code, gregexpr("BLOCK *\\( *([1-9][0-9]*) *\\)",code,perl=T)))]
dt.lines[,gsub("BLOCK *\\( *([1-9][0-9]*) *\\)","\\1",code)]

## a dt with a column containing lists of i and j. Or i and j are
## columns wit vectors - I dont know if that's possible.



## identify number of parameters in each line

## assign te indexes
