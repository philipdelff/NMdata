context("addTAD")

test_that("basic",{
    fileRef <- "testReference/addTAD_01.rds"

    dat <- readRDS("testData/data/xgxr2.rds")
    res <- addTAD(data=dat)

    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("repeated dosing data",{
    fileRef <- "testReference/addTAD_02.rds"

    dat <- readRDS("testData/data/mad.rds")
    res <- addTAD(data=dat,as.fun="data.table")

    dims(dat,res)

    expect_equal_to_reference(res,fileRef,version=2)
})
