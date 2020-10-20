## library(devtools)
## load_all("~/working_copies/NMdata")
## setwd("~/working_copies/NMdata/tests/testthat")

context("NMtransInput")

test_that("basic",{

    fileRef <- "testReference/NMtransInput.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")

    ## res1 <- NMtransInput(file=file.lst,applyFilters = T,as.fun="none")
    ### using as.data.table for as.fun is not recommended but still allowed
    res1 <- NMtransInput(file=file.lst,applyFilters = T,as.fun=as.data.table)

    expect_equal_to_reference(res1,fileRef,version=2)
})


test_that("single = filter",{
    
    file.lst <- NMdata_filepath("examples/nonmem/xgxr009.lst")
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="DATA")
    res1 <- NMtransInput(file=file.lst,applyFilters = T,as.fun="none")
    expect_equal(res1[,unique(DOSE)],10)
    
})

