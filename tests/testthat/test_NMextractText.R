## library(devtools)
## load_all("~/working_copies/NMdata")
## setwd("~/working_copies/NMdata/tests/testthat")

context("NMextractText")

test_that("basic",{

    fileRef <- "testReference/NMextractText_1.rds"
    file.lst <- "testData/nonmem/xgxr004.lst"
    res1 <- NMextractText(file.lst,section="THETA",char.section="\\$")

    expect_equal_to_reference(res1,fileRef,version=2)
})

test_that("basic - lst mode",{

    fileRef <- "testReference/NMextractText_2.rds"

    ## readRDS(fileRef)
    file.lst <- "testData/nonmem/xgxr004.lst"
    res1 <- NMextractText(file.lst,section="THETA",char.section="\\$",type="mod")
    expect_equal_to_reference(res1,fileRef,version=2)

    res1 <- NMextractText(file.lst,section="THETA",char.section="\\$",type="lst")
    expect_null(res1)
})
