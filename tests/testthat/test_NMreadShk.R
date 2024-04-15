context("NMreadShk")

NMdataConf(reset=TRUE)

test_that("Basic",{
    fileRef <- "testReference/NMreadShk_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    res <- NMreadShk(file.mod)

    expect_equal_to_reference(res,fileRef)

})
