## library(devtools)
## load_all("NMdata")
## setwd("NMdata/tests/testthat")

## load_all()

context("NMscanTables")

test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanTables1.rds"
    ## file.lst <- system.file("examples/nonmem/xgxr003.lst",package="NMdata")
    file.lst <- "testData/nonmem/xgxr003.lst"

    res.dt <- NMscanTables(file=file.lst,as.fun="data.table",rep.count=TRUE)
    meta <- NMinfoDT(res.dt)
    meta$tables[,file.mtime:=NULL]
    meta$tables[,file:=basename(file)]
    writeNMinfo(res.dt,meta)
    expect_equal_to_reference(res.dt,fileRef,version=2)

    ## test that we have data.tables from using as.fun=none
    expect_true(all(unlist(lapply(res.dt,is.data.table))))

    ## and if we convert to df, we get exactly the same as when relying on default
    res.dt.df <- lapply(res.dt,as.data.frame)
    res.df <- NMscanTables(file=file.lst,rep.count=TRUE)
    ## unNMdata(res.df)
    setattr(res.df,"NMdata",NULL)
    expect_equal(res.df,res.dt.df)

})


test_that("Details table",{
    fileRef <- "testReference/NMscanTables2.rds"
    file.lst <- system.file("examples/nonmem/xgxr003.lst", package="NMdata")

    res <- NMscanTables(file=file.lst,as.fun="data.table",rep.count=TRUE)
### this will make trouble because meta data table contains absolute
### paths which is machine dependent. So removing path.
    meta <- attributes(res)$NMdata
    meta$tables[,file:=basename(file)]
    meta$tables$file.mtime <- NULL
    writeNMinfo(res,meta)
    ## df approach
    ## res$meta$file <- basename(res$meta$file)
    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("$TABLE header options",{

    fileRef <- "testReference/NMscanTables3.rds"
    file.lst <- "testData/nonmem/xgxr024.lst"

    res <- NMscanTables(file=file.lst,as.fun="data.table",rep.count=TRUE)
    meta <- NMinfoDT(res)
    meta$tables[,file:=basename(file)]
    meta$tables$file.mtime <- NULL
    writeNMinfo(res,meta)
    
    expect_equal_to_reference(res,fileRef,version=2)
    
})

test_that("Two firstonly, one full-length",{

    fileRef <- "testReference/NMscanTables4.rds"
    file.lst <- "testData/nonmem/xgxr025.lst"

    res <- NMscanTables(file=file.lst,details=T,as.fun="data.table",rep.count=TRUE)

    meta <- NMinfoDT(res)
    meta$tables[,file:=basename(file)]
    meta$tables$file.mtime <- NULL
    writeNMinfo(res,meta)

    expect_equal_to_reference(res,fileRef,version=2)
    
})


test_that("Commented output table",{

    fileRef <- "testReference/NMscanTables5.rds"
    file.lst <- "testData/nonmem/xgxr028.lst"

    tabs <- NMscanTables(file.lst)

    meta <- NMinfoDT(tabs)
    meta$tables[,file:=basename(file)]
    meta$tables$file.mtime <- NULL
    writeNMinfo(tabs,meta)

    expect_equal_to_reference(tabs,fileRef,version=2)

})
