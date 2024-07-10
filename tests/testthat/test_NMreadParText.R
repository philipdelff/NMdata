
context("splitFields")
test_that("basic",{

    fileRef <- "testReference/splitFields_01.rds"

    res <- split.fields("%init;[%num2];%symbol")
    expect_equal_to_reference(res,fileRef)

})



context("NMreadParText")

readRef <- FALSE


test_that("muref SAEM",{

    fileRef <- "testReference/NMreadParText_02.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod)
    
    expect_equal_to_reference(res,fileRef)

})


test_that("merge with NMreadExt output",{

    fileRef <- "testReference/NMreadParText_03.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod)
    
    res <- mergeCheck(
        res,
        NMreadExt(file.mod)[,.(parameter,est)],
        by="parameter")

    expect_equal_to_reference(res,fileRef)
    
})


test_that("complex delimiters",{

    fileRef <- "testReference/NMreadParText_04.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA
; missing field
$THETA  (3)             ;  [] ;LTVV2
; missing end field
$THETA  (1)             ;[3]
; extra delim
$THETA  (4)             ;[4] ;LTVV3 ;
; missing end field but has delim
$THETA  (-1)             ; [5] ;

$OMEGA 0 FIX 

$SIGMA 0 FIX

")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num2];%symbol")
res

    expect_equal_to_reference(res,fileRef)


})



test_that("No SIGMA",{

    fileRef <- "testReference/NMreadParText_05.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA

$OMEGA 0 FIX 

")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num2];%symbol")


    expect_equal_to_reference(res,fileRef)


})


test_that("Complex OMEGA",{

    fileRef <- "testReference/NMreadParText_06.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA

$OMEGA 0 FIX ; fake
$OMEGA  BLOCK(4)
0.126303  ;    IIV.CLP  ; lognormalOm ;1   ;IIV     ;Between-subject variability on CLP;-
  0.024  ; IIV.CLP.V2P.cov  ; lognormalOm ;1-2 ;IIV     ;Covariance of BSV on CLP and V2P;-
  0.127  ;    IIV.V2P  ; lognormalOm ;2   ;IIV     ;Between-subject variability on V2P;-
  0.2  ; IIV.CLP.V3P.cov  ; lognormalOm ;1-3 ;IIV     ;Covariance of BSV on CLP and V3P;-
  0.2  ; IIV.V2P.V3P.cov  ; lognormalOm ;2-3 ;IIV     ;Covariance of BSV on V2P and V3P;-
  0.38  ;    IIV.V3P  ; lognormalOm ;3   ;IIV     ;Between-subject variability on V3P;-
  0.3  ; IIV.CLP.V3M.cov  ; lognormalOm ;1-4 ;IIV     ;Covariance of BSV on CLP and V3M;-
  0.2  ; IIV.V2P.V3M.cov  ; lognormalOm ;2-4 ;IIV     ;Covariance of BSV on V2P and V3M;-
  0.59  ; IIV.V3P.V3M.cov  ; lognormalOm ;3-4 ;IIV     ;Covariance of BSV on V3P and V3M;-
  0.4  ;    IIV.V3M  ; lognormalOm ;4   ;IIV     ;Between-subject variability on V3M;-

$SIGMA 1
")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num2];%symbol",format.omega="%init            ; %symbol            ; %trans       ; %num ; %panel   ; %label ; %unit")

res
    expect_equal_to_reference(res,fileRef)


})
