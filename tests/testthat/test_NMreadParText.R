context("NMreadExt")

readRef <- FALSE


test_that("muref SAEM",{

    fileRef <- "testReference/NMreadParText_02.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod)
    
    expect_equal_to_reference(res,fileRef)

})


test_that("merge with NMreadExt output",{

    fileRef <- "testReference/NMreadParText_03.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod)
    
    res <- mergeCheck(
        res,
        NMreadExt(file.mod)[,.(parameter,est)],
        by="parameter")

    expect_equal_to_reference(res,fileRef)
    
})
