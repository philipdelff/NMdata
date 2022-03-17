## library(devtools)
## load_all()

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

    ## pk[EVID==1,FLAG2:=0]
    ## pk[EVID==1,flag2:="Dosing"]
    pk <- flagsAssign(pk,col.flagn="FLAG2",col.flagc="flag2",subset.data="EVID==1",flagc.0="Dosing")
    
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

test_that("Include EVID==1",{
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    
    dt.flags <- fread(text="FLAG,flag,condition
0,Dosing,EVID==1
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    expect_error(flagsAssign(pk,tab.flags=dt.flags,flags.increasing=T))
    pk <- flagsAssign(pk,tab.flags=dt.flags,flags.increasing=T,subset.data="EVID==0")
    pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing",
                      col.flagn="flagn",col.flagc="flagc")
    
    ## all excluded due to below LLOQ
    tab.count <- flagsCount(pk[EVID==0],dt.flags,col.flagn="FLAG",col.flagc="flag",flags.increasing=T)

    fileRef <- "testReference/flagsAssign_4.rds"
    expect_equal_to_reference(pk,fileRef)
    fileRef <- "testReference/flagsCount_4.rds"
    expect_equal_to_reference(tab.count,fileRef)
    
})


test_that("A NULL data set",{
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    
    dt.flags <- fread(text="FLAG,flag,condition
0,Dosing,EVID==1
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    expect_warning(flagsAssign(data=pk[0],tab.flags=dt.flags,flags.increasing=T))
    

})



test_that("Writing data - data.frames",{

    testOut <- "testOutput/flagsCount_5.csv"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- as.data.frame(pk)
    
    dt.flags <- read.csv(text="FLAG,flag,condition
0,Dosing,EVID==1
100,Below LLOQ,EVID==0&BLQ==1
10,Negative time,EVID==0&TIME<0")
    
    pk <- flagsAssign(pk,tab.flags=dt.flags,flags.increasing=T,subset.data="EVID==0")
    pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing")
    
    ## all excluded due to below LLOQ
    tab.count <- flagsCount(pk[pk$EVID==0,],dt.flags,flags.increasing=T,file=testOut)

    testRes <- fread(testOut,header=T)
    
    fileRef <- "testReference/flagsCount_5.rds"
    expect_equal_to_reference(testRes,fileRef)
    
})


test_that("count by",{

    fileRef <- "testReference/flagsCount_6.rds"

    pk <- readRDS("testReference/flagsAssign_1.rds")

    dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,EVID==0&BLQ==1
100,Negative time,EVID==0&TIME<0")

### and count the two different exclusions
    ## two obs are discarded due to negative time
    tab.count <- flagsCount(pk[EVID==0],dt.flags,col.flagn="FLAG",col.flagc="flag",by="TRTACT")

    expect_equal_to_reference(tab.count,fileRef)
    
})
