context("NMreadExt")

readRef <- FALSE

test_that("basic - pars",{

    fileRef <- "testReference/NMreadExt_01.rds"
    file.ext <- "testData/nonmem/xgxr003.ext"
    if(readRef) ref <- readRDS(fileRef)
 
    res <- NMreadExt(file=file.ext,as.fun="data.table")
    expect_equal_to_reference(res,fileRef)
})

test_that("basic - all",{

    fileRef <- "testReference/NMreadExt_02.rds"
    file.ext <- "testData/nonmem/xgxr003.ext"

    res <- NMreadExt(file=file.ext,as.fun="data.table",return="all")
    expect_equal_to_reference(res,fileRef)
})

test_that("basic - all from multiple models",{

    fileRef <- "testReference/NMreadExt_03.rds"
    file.ext <- c("testData/nonmem/xgxr003.ext","testData/nonmem/xgxr006.ext")

    res <- NMreadExt(file=file.ext,as.fun="data.table",return="all")
    expect_equal_to_reference(res,fileRef)
})


test_that("muref - all",{

    fileRef <- "testReference/NMreadExt_04.rds"
    file.ext <- "testData/nonmem/xgxr031.ext"

    res <- NMreadExt(file=file.ext,as.fun="data.table",return="all")
    expect_equal_to_reference(res,fileRef)
})

test_that("muref SAEM - all",{

    fileRef <- "testReference/NMreadExt_05.rds"
    file.ext <- "testData/nonmem/xgxr032.ext"

    res <- NMreadExt(file=file.ext,as.fun="data.table",return="all")
    expect_equal_to_reference(res,fileRef)
})
