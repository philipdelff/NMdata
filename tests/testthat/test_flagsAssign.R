## library(devtools)
## load_all()

context("flagsAssign")

test_that("subset of zero rows",{

    testOut <- "testOutput/flagsCount_5.csv"
    
    pk0 <- readRDS(file="testData/data/xgxr2.rds")
    
    dt.flags <- read.csv(text="FLAG,flag,condition
0,Dosing,EVID==1
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    pk1 <- flagsAssign(pk0,tab.flags=dt.flags,flags.increasing=T,subset.data="EVID==8")
    setindex(pk0,NULL)
    setindex(pk1,NULL)
    expect_equal(pk1,pk0)
    
})
