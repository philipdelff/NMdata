## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("NMscanInput")

test_that("basic",{

    fileRef <- "testReference/NMscanInput.rds"
    file.lst <- system.file("examples/nonmem/xgxr004.lst",package="NMdata")

    ## res1 <- NMscanInput(file=file.lst,applyFilters = T,as.fun="none")
    ### using as.data.table for as.fun is not recommended but still allowed
    res1 <- NMscanInput(file=file.lst,applyFilters = T,as.fun=as.data.table)

    expect_equal_to_reference(res1,fileRef,version=2)
})


test_that("single = filter",{
    
    file.lst <- system.file("examples/nonmem/xgxr009.lst", package="NMdata")
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="DATA")
    res1 <- NMscanInput(file=file.lst,applyFilters = T,as.fun="none")
    expect_equal(res1[,unique(DOSE)],10)
    
})

test_that("Duplicate columns in input data",{
    fileRef <- "testReference/NMscanInput3.rds"
    file.lst <- system.file("examples/nonmem/xgxr015.lst", package="NMdata")

    ## res <- NMscanData(file=file.lst)
    ## res <- NMscanData(file=file.lst)

    ## load_all("../../")
    ## debugonce(NMscanInput)
    inpdat <- expect_warning(NMscanInput(file=file.lst))
    
})