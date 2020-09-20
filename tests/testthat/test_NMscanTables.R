## library(devtools)
## load_all("~/working_copies/NMdata")
## setwd("~/working_copies/NMdata/tests/testthat")

context("NMscanTables")

test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanTables1.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")

    ## res <- NMscanData(file=file.lst)
    res <- NMscanTables(file=file.lst)

    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("Details table",{
    fileRef <- "testReference/NMscanTables2.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")
### this will make trouble because meta data table contains absolute
### paths which is machine dependent. So removin path.
    res <- NMscanTables(file=file.lst,details=T)
    res$meta[,file:=basename(file)]
    ## res$meta
    expect_equal_to_reference(res,fileRef,version=2)
})
