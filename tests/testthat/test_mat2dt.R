
context("mat2dt")

test_that("basic",{
    fileRef <- "testReference/mat2dt_1.rds"

    cov <- NMreadCov("testData/nonmem/xgxr032.cov")

    res <- mat2dt(cov)

    
    expect_equal_to_reference(res,fileRef)

})
