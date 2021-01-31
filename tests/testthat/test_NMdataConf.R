context("NMdataConf")

test_that("defaults",{

    fileRef <- "testReference/NMdataConf1.rds"

    defaults <- NMdataConf()
    
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
    expect_error(NMdataConf("data.table"))
}
