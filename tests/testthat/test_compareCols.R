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
