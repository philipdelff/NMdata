
test_that("basic",{
    fileRef <- "testReference/NMcheckData_1.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMcheckData(pk)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("No col.flagn",{
    fileRef <- "testReference/NMcheckData_2.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- NMcheckData(pk,col.flagn=FALSE)
    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("Misc findings",{
    fileRef <- "testReference/NMcheckData_3.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    colnames(pk)[24] <- "ret.4"
    ## a dup col name
    pk[2,ret.4:="3,mg"]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})

#### This should give an error but does not
test_that("TIME with characters",{
    fileRef <- "testReference/NMcheckData_5.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    ## a character in TIME
    pk[,TIME:=as.character(TIME)]
    pk[ROW==204,TIME:=paste0(TIME,"p")]

    ##    load_all("~/wdirs/NMdata/")
    
    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})



test_that("Misc findings and dup colname",{
    fileRef <- "testReference/NMcheckData_5.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    colnames(pk)[24] <- "ret.4"
    ## a dup col name
    colnames(pk)[22] <- "NAME"
    pk[2,ret.4:="3,mg"]

    res <- expect_warning(NMcheckData(pk))
    expect_equal_to_reference(res,fileRef,version=2)
    
})
