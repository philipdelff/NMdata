context("NMreadPhi")

test_that("basic",{

    fileRef <- "testReference/NMreadPhi_01.rds"
    file.phi <- "testData/nonmem/xgxr003.phi"

    res.dt <- NMreadPhi(file=file.phi,as.fun="data.table")
    expect_equal_to_reference(res.dt,fileRef)
})

