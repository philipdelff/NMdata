if(F){
    context("NMrelate")
    source("../../devel/NMrelate.R")

    test_that("basic",{
        file.mod <- "testData/nonmem/xgxr032.mod"

        NMrelate(file.mod,type="eta")
        NMrelate(file.mod,type="theta")
        NMrelate(file.mod,type="sigma")
    })
}
