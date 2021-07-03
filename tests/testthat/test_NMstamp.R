context("NMstamp")

fix.time <- function(x){
    meta.x <- attr(x,"NMdata")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.NMscanData <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.mod <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$mtime.mod <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL
    meta.x$dataCreate$CreationTime <- NULL
    setattr(x,"NMdata",meta.x)
}


test_that("basic",{
    fileRef <- "testReference/NMstamp_1.rds"
    ##    outfile <- "testOutput/x_stamp_1.rds"
    x <- 1
    NMstamp(x,script="test_NMstamp.R")

    fix.time(x)
    
    expect_equal_to_reference(x,fileRef,version=2)
})
