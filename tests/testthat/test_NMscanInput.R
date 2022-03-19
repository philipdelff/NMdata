## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("NMscanInput")

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
}



NMdataConf(reset=TRUE)

test_that("basic",{

    fileRef <- "testReference/NMscanInput_1.rds"
    file.lst <- "testData/nonmem/xgxr004.lst"

    ## res1 <- NMscanInput(file=file.lst,applyFilters = T,as.fun="none")
### using as.data.table for as.fun is not recommended but still allowed
    res <-
        NMscanInput(file=file.lst,applyFilters = T,as.fun="data.table")
    fix.time(res)
    expect_equal_to_reference(res,fileRef,version=2)
})


### this one has NMdata meta data
test_that("input has NMdata meta data",{
    fileRef <- "testReference/NMscanInput_2.rds"
    ## load_all("c:/Users/delff/working_copies/NMdata")

    file.lst <- "testData/nonmem/xgxr011.lst"
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="DATA")
    

    res <- NMscanInput(file=file.lst,applyFilters = T,as.fun="data.table")
    fix.time(res)
    nm1 <- NMinfo(res)
    expect_equal_to_reference(nm1,fileRef,version=2)
## readRDS(fileRef)$tables; nm1$tables    
    
})


test_that("single = filter",{
    ## load_all("c:/Users/delff/working_copies/NMdata")


    file.lst <- "testData/nonmem/xgxr009.lst"
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="DATA")
    res <- NMscanInput(file=file.lst,applyFilters = T,as.fun="data.table")
    expect_equal(res[,unique(DOSE)],10)
    
})


test_that("Duplicate columns in input data",{
    fileRef <- "testReference/NMscanInput3.rds"
    ## file.lst <- system.file("examples/nonmem/xgxr015.lst", package="NMdata")
    file.lst <- "testData/nonmem/xgxr015.lst"

    ## res <- NMscanData(file=file.lst)
    ## res <- NMscanData(file=file.lst)

    ## load_all("../../")
    ## debugonce(NMscanInput)
    inpdat <- expect_warning(NMscanInput(file=file.lst))
    
})

test_that("single-char ignore",{
    NMdataConf(reset=T)
    fileRef <- "testReference/NMscanInput4.rds"
    file.lst <- "testData/nonmem/estim_debug.lst"

    ## inpdat <- NMscanInput(file=file.lst,applyFilters=T,file.mod=function(x)sub("\\.lst$",".ctl",x))
    res <- NMscanInput(file=file.lst,applyFilters=T,file.mod=function(x)fnExtension(x,".ctl"))
    expect_equal(nrow(res),98)
    
    fix.time(res)
    expect_equal_to_reference(res,fileRef,version=2)

})



test_that(".mod with mix of space and , in $INPUT",{
    fileRef <- "testReference/NMscanInput5.rds"
    file.lst <- "testData/nonmem/min036.mod"

    inpdat <- NMscanInput(file=file.lst)

    expect_equal_to_reference(colnames(inpdat),fileRef,version=2)
    
})


test_that("Erroneously basing a filter on translated column names",{
    ## user could be 
    expect_error(
        NMscanInput("testData/nonmem/min036mod.mod",applyFilters=TRUE)
    )
})

test_that("Including meta data",{
    NMdataConf(reset=T)
    fileRef <- "testReference/NMscanInput6.rds"
    file.lst <- "testData/nonmem/xgxr004.lst"

    res <-
        NMscanInput(file=file.lst,applyFilters = T,details=T, as.fun="data.table")

    ## res$meta$details$file <- "file"
    ## res$meta$details$file.mtime <- NULL
    fix.time(res)    
    
    expect_equal_to_reference(res,fileRef,version=2)
    
    
})


test_that("CYCLE=DROP",{

    fileRef <- "testReference/NMscanInput_7.rds"
    file.lst <- system.file("examples/nonmem/xgxr002.lst",package="NMdata")

    ## res <- NMscanInput(file=file.lst,applyFilters = T,as.fun="none")
### using as.data.table for as.fun is not recommended but still allowed
    res <-
        NMscanInput(file=file.lst,applyFilters = T,as.fun="data.table")

    fix.time(res)
    nm1 <- NMinfo(res)
    expect_equal_to_reference(nm1,fileRef,version=2)
})
