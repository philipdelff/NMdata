## library(devtools)
## setwd("tests/testthat")
## load_all("../../")

context("NMscanData")
NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}


fix.time <- function(x){
    meta.x <- attr(x,"meta")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.call <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$tables$details$file <- NULL
    meta.x$details$tables$file.mtime <- NULL
    setattr(x,"meta",meta.x)
}

test_that("basic",{

    fileRef <- "testReference/NMscanData1.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    res1 <- NMscanData(file=file.lst,quiet=T,order.columns = F)
    ## dim(res1)

    fix.time(res1)
    
    expect_equal_to_reference(res1,fileRef,version=2)
## without meta
##    expect_equal(unNMdata(res1),unNMdata(readRDS(fileRef)))
    ## data.table(attributes(readRDS(fileRef))$meta$variables$variable,attributes(res1)$meta$variables$variable)
})


test_that("Modifications to column names in $INPUT",{

    fileRef <- "testReference/NMscanData2.rds"
    ## res.ref <- readRDS(fileRef)
    
    file.lst <- NMdata_filepath("examples/nonmem/xgxr002.lst")

    res <- NMscanData(file=file.lst,check.time = FALSE)
    fix.time(res)
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
})


test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanData3.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")

    ## res <- NMscanData(file=file.lst)
    res <- NMscanData(file=file.lst,check.time = FALSE)
    fix.time(res)
    
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
})

test_that("Interpret IGNORE statement",{
    fileRef <- "testReference/NMscanData4.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")

    ## res <- NMscanData(file=file.lst)
    ## res <- NMscanData(file=file.lst)

    res <- NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE)
    fix.time(res)
    ## names(res$row)
    
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
})


test_that("List of ACCEPT statements and vs separate statements",{
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    file2.lst <- NMdata_filepath("examples/nonmem/xgxr007.lst")

    NMgetSection(file1.lst,section="PROBLEM")
    NMgetSection(file2.lst,section="PROBLEM")
    res1 <- NMscanData(file=file1.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE)
    res2 <- NMscanData(file=file2.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE) 
    setattr(res1,"meta",NULL)
    setattr(res2,"meta",NULL)
    expect_identical(res1,res2)
})


### find out how much can be tested. 
test_that("merge by filters or not",{
    ##    fileRef <- "testReference/NMscanData4.rds"
    file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    file2.lst <- NMdata_filepath("examples/nonmem/xgxr008.lst")

    ## NMgetSection(file1.lst,section="PROBLEM")
    ## NMgetSection(file2.lst,section="PROBLEM")
    res1 <- NMscanData(file=file1.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE)
    res2 <- NMscanData(file=file2.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE)
    setnames(res2,"EFF0","eff0")
    setcolorder(res1,colnames(res2))

    ## the var tables are different because ROW is input in one,
    ## output in the other. This is as expected.

    ## cbind(attr(res1,"var"),attr(res2,"var"))
    setattr(res1,"meta",NULL)
    setattr(res2,"meta",NULL)

    expect_equal(res1,res2)
})

test_that("Only a firstonly without ID but with ROW",{
### This should work because ROW is in firstonly table.
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData11.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

### impossible with filters
    expect_error(
        expect_warning(
            NMscanData(file=file.lst,merge.by.row=FALSE,col.row="ROW",check.time = FALSE)
        )
    )
    ## load_all()
    res1 <- NMscanData(file=file.lst,merge.by.row=TRUE,col.row="ROW",check.time = FALSE)
    fix.time(res1)
    expect_equal_to_reference(res1,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res1),unNMdata(readRDS(fileRef)))    
})



test_that("Only a firstonly, no ID, no ROW",{
### use.input is TRUE but cbind.by.filters is FALSE. This should give an error because input cannot be used even though it is requested

    ## this one only outputs a firstonly that cannot be merged onto
    ## input. use.input=T so input data should be returned.

    fileRef <- "testReference/NMscanData12.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr012.lst")
    ## NMgetSection(file.lst,section="DATA")
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="TABLE")
    
    expect_error(
        expect_warning(
            res1 <- NMscanData(file=file.lst,check.time = FALSE)
        )
    )
})


test_that("FO and row-level output. No ID, no row.",{
    
    ## row-level output returned because cbind.by.filters=F, and firstonly is without ID and row. Warning that firstonly is dropped. Correct. 
    fileRef <- "testReference/NMscanData13.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst,check.time = FALSE)
    )
    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

})

test_that("FO and row-level output. No ID, no row. cbind.by.filters=T",{
    ## row-level output+input returned because cbind.by.filters=T, and firstonly is without ID and row. Correct. 
    fileRef <- "testReference/NMscanData14.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    NMgetSection(file.lst,section="PROBLEM")
    
    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE)
    )
    fix.time(res1)
    summary(res1)$variables
    summary(res1)$tables
    summary(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

})



test_that("Only a firstonly without ID but with ROW",{
### cbind.by.filters is TRUE, so ROW is not used to recover firstonly data.

    fileRef <- "testReference/NMscanData15.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

###### Why are we not getting an error or warning here? Because col.row is available?
    res1 <- expect_error(
        expect_warning(
            NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE)
        )
    )

    ##    tabs=NMscanTables(file=file.lst)
    ## tabs
})

test_that("Only a firstonly without ID but with ROW. Using merge.by.row=TRUE.",{
### ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData15b.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,col.row="ROW",merge.by.row=TRUE,check.time = FALSE)
    
    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})

### recoverRows without a row identifier

test_that("recoverRows without a row identifier",{

    fileRef <- "testReference/NMscanData16.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,,merge.by.row=FALSE,recover.rows = T,as.fun="data.table",check.time = FALSE)
    dim(res1)
    res1[,table(nmout,DOSE)]
    fix.time(res1)
    
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})

### get a data.frame
test_that("use as.fun to get a data.frame",{
### cbind.by.filters is TRUE, so ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData17.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

    res1 <- NMscanData(file=file.lst,merge.by.row=FALSE,recover.rows = T,as.fun=as.data.frame,check.time = FALSE)
    dim(res1)
    class(res1)
    with(res1,table(nmout,DOSE))

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})



### get a tibble
test_that("use as.fun to get a tibble",{
### cbind.by.filters is TRUE, so ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData18.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,merge.by.row=FALSE,recover.rows = T,as.fun=tibble::as_tibble,check.time = FALSE)
    dim(res1)
    class(res1)

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})


## test the dir structure with input.txt/output.txt 
## load_all("../../")
test_that("dir structure with input.txt/output.txt",{
    ## options(NMdata.as.fun="none")
    
    ## options(NMdata.file.mod=function(file) file.path(dirname(file),"input.txt"))
    ## options(NMdata.modelname=function(file) basename(dirname(normalizePath(file))))

    NMdataConf(as.fun="data.table")
    NMdataConf(file.mod=function(file) file.path(dirname(file),"input.txt"))
    NMdataConf(modelname=function(file) basename(dirname(normalizePath(file))))
    
    filedir.lst <- NMdata_filepath("examples/nonmem/xgxr001dir/output.txt")
    res1dir <- NMscanData(filedir.lst,check.time = FALSE)
    expect_equal(attr(res1dir,"meta")$details$model,"xgxr001dir")
    umod <- unique(res1dir[,model])
    expect_equal(length(umod),1)
    expect_equal(umod,"xgxr001dir")

    
    ## options(NMdata.file.mod=NULL)
    ## options(NMdata.modelname=NULL)
    NMdataConf(file.mod="default")
    NMdataConf(modelname=NULL)
    
    file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    res1 <- NMscanData(file=file.lst,check.time = FALSE)

    unNMdata(res1)
    unNMdata(res1dir)
    colnames(res1[,!("model")])==
        colnames(res1dir[,!("model")])
    ## this test isn't ready. How do I execute the input.txt/output.txt model?
    ##expect_equal(res1[,!("model")],res1dir[,!("model")])

    NMdataConf(as.fun=NULL)
})


test_that("Duplicate columns in input data",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData20.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr015.lst")

    ## res <- NMscanData(file=file.lst)
    ## res <- NMscanData(file=file.lst)

    ## load_all("../../")
    ## debugonce(NMscanData)
    res <- expect_warning(NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE))
    fix.time(res)
    ## names(res$row)
    
    expect_equal_to_reference(res,fileRef,version=2)
})


### this is not a real test. Need to be able to test how the merges were performed. 
## test_that("col.row and merge.by.row=TRUE from NMdataConf",{
##     load_all("../../")
    
##     NMdataConf(reset=TRUE)
##     NMdataConf(col.row="ROW", merge.by.row=TRUE)
##     NMdataConf()

##     file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")

    
##     res1 <- NMscanData(file=file.lst)
## })


test_that("Modifying row identifier",{
    NMdataConf(reset=TRUE)
    file.lst <- NMdata_filepath("examples/nonmem/xgxr016.lst")

    res <-
        expect_error(
            NMscanData(file=file.lst,merge.by.row=TRUE,check.time = FALSE)
        )

})
