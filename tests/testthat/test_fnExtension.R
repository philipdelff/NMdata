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
        ## we don't need the period
        fnExtension("feww","rds")
       ,
        ## we can remove extension
        fnExtension("feww.csv","")
    
    )

    expect_equal_to_reference(res1,fileRef)
    
})

test_that("Rerieve extension",{

    expect_equal(fnExtension("feww.csv"),"csv")
    expect_equal(fnExtension("feww.aa.csv"),"csv")
    expect_equal(fnExtension("csv"),"csv")

        
})
