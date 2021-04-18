## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("compareCols")

test_that("basic",{

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk.reduced <- copy(pk)
    pk.reduced <- pk.reduced[1:(.N%/%2)]
    pk.reduced[,CYCLE:=NULL]
    pk.reduced[,AMT:=as.character(AMT)]


    compareCols(pk,pk.reduced)
    compareCols(pk,pk.reduced,diff.only=FALSE)

}
