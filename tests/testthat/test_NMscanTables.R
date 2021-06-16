## library(devtools)
## load_all("NMdata")
## setwd("NMdata/tests/testthat")

## load_all("c:/Users/delff/working_copies/NMdata")

context("NMscanTables")

test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanTables1.rds"
    file.lst <- system.file("examples/nonmem/xgxr003.lst",package="NMdata")

    res.dt <- NMscanTables(file=file.lst,as.fun="data.table",tab.count=TRUE)
    expect_equal_to_reference(res.dt,fileRef,version=2)

    ## test that we have data.tables from using as.fun=none
    expect_true(all(unlist(lapply(res.dt,is.data.table))))

    ## and if we convert to df, we get exactly the same as when relying on default
    res.dt.df <- lapply(res.dt,as.data.frame)
    res.df <- NMscanTables(file=file.lst,tab.count=TRUE)
    expect_equal(res.df,res.dt.df)

})


test_that("Details table",{
    fileRef <- "testReference/NMscanTables2.rds"
    file.lst <- system.file("examples/nonmem/xgxr003.lst", package="NMdata")

    res <- NMscanTables(file=file.lst,details=T,as.fun="data.table",tab.count=TRUE)
### this will make trouble because meta data table contains absolute
### paths which is machine dependent. So removing path.
    res$meta[,file:=basename(file)]
    res$meta$file.mtime <- NULL
    ## df approach
    ## res$meta$file <- basename(res$meta$file)
    expect_equal_to_reference(res,fileRef,version=2)
})
