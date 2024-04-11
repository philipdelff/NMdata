## library(devtools)
## load_all()

context("flagsCount")

test_that("No flags table provided",{

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    flagsCount(pk[EVID==0])
})
