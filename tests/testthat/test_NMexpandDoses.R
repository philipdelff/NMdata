context("NMexpandDoses")

test_that("basic",{
    fileRef <- "testReference/NMexpandDoses_01.rds"
    
    mad <- readRDS("testData/data/mad.rds")

    res <- NMexpandDoses(mad,as.fun="data.table")

    expect_equal_to_reference(res,fileRef,version=2)
})


