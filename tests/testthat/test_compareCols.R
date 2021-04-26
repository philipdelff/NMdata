## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("compareCols")

test_that("basic",{

    fileRef <- "testReference/compareCols_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]

    res1 <- list(
        compareCols(pk,pk.reduced)
       ,
        compareCols(pk,pk.reduced,diff.only=FALSE)
           ,
        compareCols(pk,pk.reduced,diff.only=FALSE,keepNames = F)
    )

    expect_equal_to_reference(res1,fileRef)

})



test_that("messy names",{
## tested because names created problems in mergeCheck when data.tables are created inside the function call like mergeCheck(data.table(foo=bar),df2). However, compareCols and dims work on ellipses (mergeCheck uses df1, df2, and ... is passed as extra args to merge). No problem for compareCols and dims.

    fileRef <- "testReference/compareCols_2.rds"

    res1 <- list(
## no name given. Correct even if looks very bad
        compareCols(
            data.table(variable=c("CPDVG","CMDVG","efef"))
           ,
            data.table(variable=c("CPDVG","CMDVG"),compound=c("VX-548","VRT-420"))
        )
        ## now, clean because of keepNames=F
        ,
        compareCols(
            data.table(variable=c("CPDVG","CMDVG","efef"))
           ,
            data.table(variable=c("CPDVG","CMDVG"),compound=c("VX-548","VRT-420"))
           ,keepNames=FALSE)
    )
    
    expect_equal_to_reference(res1,fileRef)

})
