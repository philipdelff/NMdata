context("fnAppend")

test_that("basic",{

    fileRef <- "testReference/fnAppend_1.rds"
    
    res1 <- list(
## numeric
        fnAppend("feww.csv",1)
       ,
        ## character
        fnAppend("feww.csv","one")
       ,
        ## 
        fnAppend("feww.csv","")
    )

    expect_equal_to_reference(res1,fileRef)
    
})

test_that("skip directory double dots",{
    ## todo. This should return an error. There is no extension to
    ## append in front of.
    fnAppend("fe/../egef","hmm")
    ## should also return error:
    fnAppend("egef","hmm")
})
