## library(devtools)
## setwd("tests/testthat")
## load_all()

context("NMscanMultiple")
## NMdata_filepath <- function(...) {
##     system.file(..., package = "NMdata")
## }

## file.nm <- function(...) NMdata_filepath("examples/nonmem",...)
## file.data <- function(...) NMdata_filepath("examples/data",...)

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
    meta.x$datafile$path <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL
    setattr(x,"NMdata",meta.x)
    invisible(x)
}

NMdataConf(reset=TRUE)
test_that("basic",{

    fileRef <- "testReference/NMscanMultiple1.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    res <- NMscanMultiple(dir=system.file("examples/nonmem",package="NMdata"),file.pattern="xgxr.*\\.lst", check.time = FALSE,quiet=TRUE)
    ## dim(res)

    res <- lapply(res,fix.time)
    
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ##    expect_equal(unNMdata(res1),unNMdata(readRDS(fileRef)))
    ## data.table(attributes(readRDS(fileRef))$meta$variables$variable,attributes(res1)$meta$variables$variable)
})
