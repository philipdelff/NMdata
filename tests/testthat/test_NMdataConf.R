context("NMdataConf")

test_that("defaults",{

    fileRef <- "testReference/NMdataConf1.rds"

    defaults <- NMdataConf()
    defaults$as.fun <- NULL
    defaults$file.mod <- NULL
    defaults$modelname <- NULL
    
    expect_equal_to_reference(defaults,fileRef)
})

test_that("reset",{

    defaults <- NMdataConf()
    NMdataConf(as.fun="data.table")
    NMdataConf(reset=TRUE)
    defaults2 <- NMdataConf()

    defaults$as.fun <- NULL
    defaults$file.mod <- NULL
    defaults$modelname <- NULL
    defaults2$as.fun <- NULL
    defaults2$file.mod <- NULL
    defaults2$modelname <- NULL
    
    
    expect_equal(defaults,defaults2)
})

test_that("unnamed argument",{
    expect_error(
        NMdataConf("data.table")
    )
})

test_that("unknown argument",{
    expect_error(
        NMdataConf(asfun="data.table")
    )
})

test_that("unknown value",{
    expect_error(
        NMdataConf(as.fun="datatable")
    )
})

test_that("change fun in globalenv does not affect NMdataConf()",{

    NMdataConf(reset=TRUE)
    
    afun <- identity
    NMdataConf(modelname=afun)
    defaults <- NMdataConf()
    afun <- class
    defaults2 <- NMdataConf()

    defaults$as.fun <- NULL
    defaults$file.mod <- NULL
    defaults$modelname <- NULL
    defaults2$as.fun <- NULL
    defaults2$file.mod <- NULL
    defaults2$modelname <- NULL
    
    
    expect_equal(defaults,defaults2)
})
