context("NMgenText")

test_that("basic",{
    fileRef <- "testReference/NMgenText_1.rds"
    ## pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- readRDS(file="testData/data/xgxr2.rds")
    res <- NMgenText(pk,width=95)
    expect_equal_to_reference(res,fileRef)
})

test_that("df vs dt",{
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.df <- as.data.frame(pk)
    expect_equal(NMgenText(pk),NMgenText(pk.df))
})


test_that("dir.data",{
    fileRef <- "testReference/NMgenText_2.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMgenText(pk,file="data1.csv")
    res.dir <- NMgenText(pk,file="data1.csv",dir.data="../nonmem")

    expect_false(is.null(res$INPUT))
    expect_equal(res$INPUT,res.dir$INPUT)
    expect_equal_to_reference(res.dir$DATA,fileRef)
})

test_that("pseudonyms",{
    fileRef <- "testReference/NMgenText_3.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
#### wrong
    ## NMgenText(pk,file="data1.csv",pseudo=c(PART="SPART"))
    res <- NMgenText(pk,file="data1.csv",copy=c(SPART="PART"),width=95)

    expect_equal_to_reference(res,fileRef)
})

test_that("rename - same order as in pseudonym syntax",{
    fileRef <- "testReference/NMgenText_4.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMgenText(pk,file="data1.csv",rename=c(SPART="PART"),width=90)
    expect_equal_to_reference(res,fileRef)
})

test_that("drop",{
    fileRef <- "testReference/NMgenText_5.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMgenText(pk,file="data1.csv",drop=c("PART"),width=95)
    expect_equal_to_reference(res,fileRef)
})

test_that("capitalize",{
    fileRef <- "testReference/NMgenText_6.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMgenText(pk,file="data1.csv",capitalize=T,width=95)
    expect_equal_to_reference(res,fileRef)
})

