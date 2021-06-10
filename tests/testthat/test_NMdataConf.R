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
