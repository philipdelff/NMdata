context("addTAD")

test_that("basic",{
    fileRef <- "testReference/addTAPD_01.rds"

    dat <- readRDS("testData/data/xgxr2.rds")
    res <- addTAPD(data=dat)

    expect_equal_to_reference(res,fileRef,version=2)
    ## dims(res,readRDS(fileRef))
    ## compareCols(res,readRDS(fileRef))
})


test_that("repeated dosing data",{
    fileRef <- "testReference/addTAPD_02.rds"

    dat <- readRDS("testData/data/mad.rds")
    res <- addTAPD(data=dat,as.fun="data.table")

    dims(dat,res)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("Custom names and discard one",{
    fileRef <- "testReference/addTAPD_03.rds"

    dat <- readRDS("testData/data/mad.rds")
    res1 <- addTAPD(data=dat,as.fun="data.table")
    res2 <- addTAPD(data=dat,col.tpdos="ATSPD",col.ndoses=NULL,as.fun="data.table")

    expect_equal( setdiff(colnames(res1),colnames(res2)),c("NDOSES", "TPDOS" ))
    expect_equal(setdiff(colnames(res2),colnames(res1)),c("ATSPD"))
    
    
})
