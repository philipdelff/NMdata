fix.time <- function(x){
    meta.x <- attr(x,"NMdata")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.NMscanData <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.mod <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$mtime.mod <- NULL
    meta.x$datafile$path <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL
    setattr(x,"NMdata",meta.x)
}


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

test_that("TIME with characters",{
    fileRef <- "testReference/NMcheckData_4.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    ## a character in TIME
    pk[,TIME:=as.character(TIME)]
    pk[ROW==204,TIME:=paste0(TIME,"p")]

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

test_that("missing EVID",{
    fileRef <- "testReference/NMcheckData_6.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,EVID:=NULL]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})


test_that("missing ID",{
        
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,ID:=NULL]

    expect_error( NMcheckData(pk))
})

test_that("missing ID",{
    fileRef <- "testReference/NMcheckData_7.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,MDV:=1]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("missing ID",{
        
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,ID:=NULL]

    expect_error( NMcheckData(pk))
})

test_that("With ADDL, no II",{
    fileRef <- "testReference/NMcheckData_8.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[EVID==1,ADDL:=1]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("With II, no ADDL",{
    fileRef <- "testReference/NMcheckData_9.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[EVID==1,II:=24]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("Using control stream file",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMcheckData_10.rds"

    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    
    res1 <- NMcheckData(file=file.lst)

    fix.time(res1)
    expect_equal_to_reference(res1,fileRef)


    res1b <- NMcheckData(file=file.lst)
    fix.time(res1b)

    expect_equal(res1,res1b)

})