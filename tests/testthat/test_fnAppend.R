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

## library(devtools)
## load_all("~/wdirs/NMdata")
test_that("skip directory double dots",{
    ## todo. This should return an error. There is no extension to
    ## append in front of.
    expect_error(fnAppend("fe/../egef","hmm"))
    ## should also return error:
    expect_error(fnAppend("egef","hmm"))
})

test_that("empty string does notning",{

    str1 <- "fe.ef"
    str2 <- fnAppend(str1,"")
    expect_identical(str1,str2)

})
