## install_github("philipdelff/NMdata@v0.0.7.2")
## library(NMdata)
## library(data.table)


context("flags")

test_that("basic",{

    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

### two flag tables with different order of the two conditions
    dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,EVID==0&BLQ==1
100,Negative time,EVID==0&TIME<0")
    
    
    ## add the two different exclusion flags
    pk <- flagsAssign(pk,tab.flags=dt.flags,subset.data="EVID==0")
    pk[EVID==1,FLAG:=0]
    pk[EVID==1,flag:="Dosing"]

### these were addedd to the dsCreate script
    ## pk[,flag2:=NULL]
    ## pk[,FLAG2:=NULL]

    fileRef <- "testReference/flagsAssign_1.rds"
    expect_equal_to_reference(pk,fileRef)

### and count the two different exclusions
    ## two obs are discarded due to negative time
    tab.count <- flagsCount(pk[EVID==0],dt.flags,col.flagn="FLAG",col.flagc="flag")

    fileRef <- "testReference/flagsCount_1.rds"
    expect_equal_to_reference(tab.count,fileRef)

})


test_that("alternative order",{
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    dt.flags2 <- fread(text="FLAG2,flag2,condition
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    pk <- flagsAssign(pk,tab.flags=dt.flags2,col.flagn="FLAG2",col.flagc="flag2",subset.data="EVID==0")

    pk[EVID==1,FLAG2:=0]
    pk[EVID==1,flag2:="Dosing"]

    ## all excluded due to below LLOQ
    tab.count <- flagsCount(pk[EVID==0],dt.flags2,col.flagn="FLAG2",col.flagc="flag2")

    fileRef <- "testReference/flagsAssign_2.rds"
    expect_equal_to_reference(pk,fileRef)
    fileRef <- "testReference/flagsCount_2.rds"
    expect_equal_to_reference(tab.count,fileRef)
    
})




test_that("incresing order",{
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    dt.flags2 <- fread(text="FLAG2,flag2,condition
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    pk <- flagsAssign(pk,tab.flags=dt.flags2,col.flagn="FLAG2",col.flagc="flag2",subset.data="EVID==0",flags.increasing=T)

    pk[EVID==1,FLAG2:=0]
    pk[EVID==1,flag2:="Dosing"]

    ## all excluded due to below LLOQ
    tab.count <- flagsCount(pk[EVID==0],dt.flags2,col.flagn="FLAG2",col.flagc="flag2",flags.increasing=T)

    fileRef <- "testReference/flagsAssign_3.rds"
    expect_equal_to_reference(pk,fileRef)
    fileRef <- "testReference/flagsCount_3.rds"
    expect_equal_to_reference(tab.count,fileRef)
    
})

