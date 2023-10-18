context("NMreadPhi")

test_that("basic",{

    fileRef <- "testReference/NMreadPhi_01.rds"
    file.phi <- "testData/nonmem/xgxr003.phi"

    res.dt <- NMreadPhi(file=file.phi,as.fun="data.table")
    expect_equal_to_reference(res.dt,fileRef)
})


test_that("basic - all from multiple models",{

    fileRef <- "testReference/NMreadPhi_02.rds"
    file.phi <- c("testData/nonmem/xgxr003.phi","testData/nonmem/xgxr006.phi")

    res.dt <- NMreadPhi(file=file.phi,as.fun="data.table")
    expect_equal_to_reference(res.dt,fileRef)
})
