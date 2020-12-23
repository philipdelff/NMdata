## library(devtools)
## load_all("~/working_copies/NMdata")
## setwd("~/working_copies/NMdata/tests/testthat")

context("NMextractText")

test_that("basic",{

    fileRef <- "testReference/NMextractText_1.rds"
    file.lst <- system.file("examples/nonmem/xgxr004.lst",package="NMdata")
    res1 <- NMextractText(file.lst,section="THETA",char.section="\\$")

    expect_equal_to_reference(res1,fileRef,version=2)
})
