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

test_that("Retrieve extension",{

    expect_equal(fnExtension("feww.csv"),"csv")
    expect_equal(fnExtension("feww.aa.csv"),"csv")
    expect_equal(fnExtension(".csv"),"csv")
## no extension to retrieve
    expect_equal(fnExtension("egef"),"")
        
})


test_that("skip directory .s",{
    expect_equal(fnExtension("fe/../egef",".csv"),"fe/../egef.csv")
    expect_equal(fnExtension("fe/./egef",".csv"),"fe/./egef.csv")
})

