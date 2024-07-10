
context("splitFields")
test_that("basic",{

    fileRef <- "testReference/splitFields_01.rds"

    res <- splitFields("%init;[%num2];%symbol")
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
    
})



context("NMreadParText")

readRef <- FALSE


test_that("muref SAEM",{

    fileRef <- "testReference/NMreadParText_02.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod,format="%init;%symbol")
    
    expect_equal_to_reference(res,fileRef)
    
    if(F){
        res
        readRDS(fileRef)
    }
    
})


test_that("merge with NMreadExt output",{

    fileRef <- "testReference/NMreadParText_03.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod,format="%init;%symbol")
    
    res <- mergeCheck(
        res,
        NMreadExt(file.mod)[,.(parameter,est)],
        by="parameter")

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

    
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

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

    
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

    if(F){
        res
        readRDS(fileRef)
    }


})


test_that("Complex OMEGA",{

    fileRef <- "testReference/NMreadParText_06.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA (mL/h)
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  0.38  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
$SIGMA 1
")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num];%symbol (%unit)",
                          format.omega="%init            ; %symbol                ; %num ; %type   ; %label ; %unit",field.idx="num")


    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

})


test_that("OMEGA SAME",{

### BLOCK SAME are being skipped
    
    fileRef <- "testReference/NMreadParText_07.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
$THETA
(0,0.1) ; THE1      - 30) 1st theta
 (0,4.2) ; THE2        - 31) 2nd theta
$OMEGA  0.08   ;    IIV.TH1  ; 1  ;IIV
 $OMEGA  BLOCK(1)
 0.547465  ; IOV.TH1  ; 2 ;IOV
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME")

    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,
                          format="%init;%symbol - %idx) %label",
                          format.omega="%init; %symbol  ; %idx  ; %label "
                          )

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})
