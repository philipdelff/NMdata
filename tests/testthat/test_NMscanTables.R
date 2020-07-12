library(devtools)
load_all("~/working_copies/NMdata")
setwd("~/working_copies/NMdata/tests/testthat")

context("NMscanTables")

test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanTables1.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")

    ## res <- NMscanData(file=file.lst,debug=T)
    res <- NMscanTables(file=file.lst,debug=F)

    expect_equal_to_reference(res,fileRef,version=2)
})
