## library(devtools)
## setwd("tests/testthat")
## load_all()
library(data.table)
context("compareCols")

test_that("basic",{

    fileRef <- "testReference/compareCols_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    ## pk.reduced <- copy(pk)
    ## pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced <- pk[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]


    res1 <- compareCols(pk,pk.reduced)

    expect_equal_to_reference(res1,fileRef)

})

test_that("diff.only=FALSE",{

    fileRef <- "testReference/compareCols_2.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]

    res1 <- compareCols(pk,pk.reduced,diff.only=FALSE)

    expect_equal_to_reference(res1,fileRef)

})

### This is almost the same above, only usig x1 and x2 for data names. This is tested in "messy names" too.
test_that("diff.only=FALSE, keepNames = F",{

    fileRef <- "testReference/compareCols_3.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]

    res1 <- compareCols(pk,pk.reduced,diff.only=FALSE,keep.names = F)

    expect_equal_to_reference(res1,fileRef)

    res2 <- compareCols(pk,pk.reduced,diff.only=FALSE,keep.names = F)
    expect_equal(res1,res2)
})




test_that("messy names",{
    ## tested because names created problems in mergeCheck when data.tables are created inside the function call like mergeCheck(data.table(foo=bar),df2). However, compareCols and dims work on ellipses (mergeCheck uses df1, df2, and ... is passed as extra args to merge). No problem for compareCols and dims.

    fileRef <- "testReference/compareCols_4.rds"

    res1 <- list(
        ## no name given. Correct even if looks very bad
        compareCols(
            data.table(variable=c("CPDVG","CMDVG","efef"))
           ,
            data.table(variable=c("CPDVG","CMDVG"),compound=c("C-1","C-2"))
        )
        ## now, clean because of keepNames=F
       ,
        compareCols(
            data.table(variable=c("CPDVG","CMDVG","efef"))
           ,
            data.table(variable=c("CPDVG","CMDVG"),compound=c("C-1","C-2"))
           ,keepNames=FALSE)
    )
    
    expect_equal_to_reference(res1,fileRef)

})


test_that("cols.wanted",{

    fileRef <- "testReference/compareCols_5.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]

    res1 <- compareCols(pk,pk.reduced,cols.wanted=c("TIME","NAME","NOEXISTS"))

    expect_equal_to_reference(res1,fileRef)

})

test_that("list.data",{

##    fileRef <- "testReference/compareCols_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]

    res1 <- compareCols(pk,pk.reduced)
    res2 <- compareCols(list.data=list(pk=pk,pk.reduced=pk.reduced))

    expect_equal(res1,res2)

})
