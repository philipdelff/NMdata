context("NMcheckDataFile")

## meta is if x is metadata rather than an NMdata object
fix.time <- function(x,meta=T){
    if(meta){
        meta.x <- attr(x,"NMdata")
    } else {
        meta.x <- x
    }

    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.NMscanData <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.mod <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$mtime.mod <- NULL
    meta.x$datafile$path <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL

    if(meta){
        setattr(x,"NMdata",meta.x)
    } else {
        x <- meta.x
    }
    x
}


test_that("Using control stream file",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMcheckDataFile_01.rds"

    file.lst <- "testData/nonmem/xgxr001.lst"
    
    res1a <- NMcheckData(file=file.lst)

    res1a <- fix.time(res1a,meta=F)
    expect_equal_to_reference(res1a,fileRef)

    ## res.ref <- readRDS(fileRef)
    ## res <- lapply(names(res.ref),function(name){
    ##     expect_equal(res.ref[[name]],res1a[[name]])
    ## })
    
    res1b <- NMcheckData(file=file.lst)
    res1b <- fix.time(res1b,meta=F)

    expect_equal(res1a,res1b)

})




test_that("Input control stream missing",{
    NMdataConf(reset=TRUE)
    NMdataConf(file.mod="doesNotExist.mod")

    fileRef <- "testReference/NMcheckDataFile_02.rds"
    file.lst <- "testData/nonmem/xgxr001.lst"

    expect_error(NMcheckDataFile(file=file.lst,use.input=F))

})
