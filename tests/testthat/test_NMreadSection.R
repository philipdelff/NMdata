context("NMreadSection")




test_that("basic",{

    fileRef <- "testReference/NMreadSection_01.rds"
    
    section <- "input"
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod"
                        ,section=section
                         )
    
    expect_equal_to_reference(res,fileRef,version=2)

})

test_that("basic",{

    fileRef <- "testReference/NMreadSection_02.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- "testData/nonmem/xgxr001.lst"

    res1 <- NMreadSection(file=file.lst,section="DATA")
    ## res1 <- NMreadSection(file=file.lst,section="DATA",debug=T)

    expect_equal_to_reference(res1,fileRef,version=2)
})


test_that("Section not found",{

    section <- "simulation"
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod"
                        ,section=section
                         )
    
    expect_null(res)

})
