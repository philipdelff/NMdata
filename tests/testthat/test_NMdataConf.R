context("NMdataConf")

NMdataConf(reset=TRUE)

test_that("defaults",{

    fileRef <- "testReference/NMdataConf_01.rds"
    ## ref <- readRDS(fileRef)

    defaults <- NMdataConf()
    defaults$as.fun <- NULL
    defaults$file.mod <- NULL
    defaults$file.cov <- NULL
    defaults$file.ext <- NULL
    defaults$file.phi <- NULL
    defaults$modelname <- NULL

    expect_equal_to_reference(defaults,fileRef)

##     compareCols(readRDS(fileRef),defaults)
})

test_that("reset",{

    defaults <- NMdataConf()
    NMdataConf(as.fun="data.table")
    NMdataConf(reset=TRUE)
    defaults2 <- NMdataConf()

    defaults$as.fun <- NULL
    defaults$file.mod <- NULL
    defaults$file.cov <- NULL
    defaults$file.ext <- NULL
    defaults$file.phi <- NULL
    defaults$modelname <- NULL
    defaults2$as.fun <- NULL
    defaults2$file.mod <- NULL
    defaults2$file.cov <- NULL
    defaults2$file.ext <- NULL
    defaults2$file.phi <- NULL
    defaults2$modelname <- NULL
    
    
    expect_equal(defaults,defaults2)

    res.conf <- NMdataConf()
    res.noreset <- NMdataConf(reset=FALSE)
    expect_equal(res.conf,res.noreset)

    ## we can't reset and do new stuff simultaneously
    expect_error(NMdataConf(reset=TRUE,file.mod=identity))

    NMdataConf(file.mod="reset")
    
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
    defaults$file.cov <- NULL
    defaults$file.ext <- NULL
    defaults$file.phi <- NULL
    defaults$modelname <- NULL
    defaults2$as.fun <- NULL
    defaults2$file.mod <- NULL
    defaults2$file.cov <- NULL
    defaults2$file.ext <- NULL
    defaults2$file.phi <- NULL
    defaults2$modelname <- NULL
    
    
    expect_equal(defaults,defaults2)
})


test_that("reset single option",{

    defaults <- NMdataConf()
    NMdataConf(as.fun="data.table",file.mod=identity)
    NMdataConf(as.fun=NULL)
    c1 <- NMdataConf()

    defaults <- NMdataConf()
    NMdataConf(as.fun="data.table",file.mod=identity)
    NMdataConf(as.fun="default")
    c2 <- NMdataConf()
    
    expect_equal(c1,c2)
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
    defaults$file.cov <- NULL
    defaults$file.ext <- NULL
    defaults$file.phi <- NULL
    defaults$modelname <- NULL

    defaults2$as.fun <- NULL
    defaults2$file.mod <- NULL
    defaults2$file.cov <- NULL
    defaults2$file.ext <- NULL
    defaults2$file.phi <- NULL
    defaults2$modelname <- NULL
    
    expect_equal(defaults,defaults2)
})


test_that("deprecated use.rds",{

    fileRef <- "testReference/NMdataConf_02.rds"
    
    NMdataConf(reset=TRUE)
    
    ## NMdataConf(use.rds=TRUE)
    NMdataConf(formats.read=c("csv"))
    new <- NMdataConf()
    
    new$as.fun <- NULL
    new$file.cov <- NULL
    new$file.mod <- NULL
    new$file.ext <- NULL
    new$file.phi <- NULL
    new$modelname <- NULL

    
    expect_equal_to_reference(new,fileRef)
    ## compareCols(readRDS(fileRef),defaults)
})

test_that("reset removes unknown",{
    NMdataConf(reset=T)

    opts <- NMdataConf()
    expect_false("unknown.option"%in%names(NMdataConf()))
    
    NMdataConf(unknown.option="test",allow.unknown = TRUE)
    
    expect_true(NMdataConf()$"unknown.option"=="test")
    
    NMdataConf(reset=TRUE)
    expect_false("unknown.option"%in%names(NMdataConf()))

})
