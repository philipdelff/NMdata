
context("NMreadCov")

test_that("basic",{

    fileRef <- "testReference/NMreadCov_01.rds"
    file.cov <- "testData/nonmem/estim_debug.cov"

    res <- NMreadCov(file=file.cov)
    expect_equal_to_reference(res,fileRef)
})

