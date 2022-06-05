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
