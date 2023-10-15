if(F){
    context("NMreadCov")

    test_that("basic",{

        fileRef <- "testReference/NMreadCov_01.rds"
        file.cov <- "testData/nonmem/xgxr003.cov"

        res.dt <- NMreadCov(file=file.cov)
        expect_equal_to_reference(res.dt,fileRef)
    })
}
