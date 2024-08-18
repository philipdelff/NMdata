context("NMdataConf")

NMdataConf(reset=TRUE)

    ## can't compare functions
dropFuns <- function(x){
    x$as.fun <- NULL
    x$file.mod <- NULL
    x$file.cov <- NULL
    x$file.ext <- NULL
    x$file.phi <- NULL
    x$file.shk <- NULL
    x$modelname <- NULL
    x
}

test_that("defaults",{

    fileRef <- "testReference/NMdataConf_01.rds"
    ## ref <- readRDS(fileRef)

    defaults <- NMdataConf()
    defaults <- dropFuns(defaults)

    expect_equal_to_reference(defaults,fileRef)

    ## compareCols(readRDS(fileRef),defaults)
    
})

test_that("reset",{

    defaults <- NMdataConf()
    NMdataConf(as.fun="data.table")
    NMdataConf(reset=TRUE)
    defaults2 <- NMdataConf()

    defaults <- dropFuns(defaults)
    defaults2 <- dropFuns(defaults2)
    
    
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

    defaults <- dropFuns(defaults)
    defaults2 <- dropFuns(defaults2)
    
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

    defaults <- dropFuns(defaults)
    defaults2 <- dropFuns(defaults2)
    
    expect_equal(defaults,defaults2)
})


test_that("deprecated use.rds",{

    fileRef <- "testReference/NMdataConf_02.rds"
    
    NMdataConf(reset=TRUE)
    
    ## NMdataConf(use.rds=TRUE)
    NMdataConf(formats.read=c("csv"))
    new <- NMdataConf()

    new <- dropFuns(new)
    
    expect_equal_to_reference(new,fileRef)
    ## compareCols(readRDS(fileRef),new)
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
