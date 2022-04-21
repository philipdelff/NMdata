## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("fnExtension")

test_that("basic",{

    fileRef <- "testReference/fnExtension_1.rds"
    
    res1 <- list(
        fnExtension("feww.csv",".rds")
       ,
        ## Adding extension where not existing is also supported
        fnExtension("feww",".rds")
       ,
        ## we can remove extension
        fnExtension("feww.csv","")
    )

    expect_equal_to_reference(res1,fileRef)
    
})
