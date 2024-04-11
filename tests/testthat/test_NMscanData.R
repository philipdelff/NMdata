## library(devtools)
## setwd("tests/testthat")
## load_all()

context("NMscanData")
library(data.table)
data.table::setDTthreads(1)

## NMdata_filepath <- function(...) {
##     system.file(..., package = "NMdata")
## }

## file.nm <- function(...) NMdata_filepath("examples/nonmem",...)
## file.data <- function(...) NMdata_filepath("examples/data",...)

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
    meta.x$datafile$path.csv <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$datafile$path.fst <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL
    setattr(x,"NMdata",meta.x)
    invisible(x)
}

NMdataConf(reset=TRUE)
test_that("basic",{

    fileRef <- "testReference/NMscanData1.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    ## file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    file.lst <- "testData/nonmem/xgxr001.lst"
    ## NMreadSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    res <- NMscanData(file=file.lst, quiet=T, order.columns = F, merge.by.row=FALSE, check.time = FALSE)
    ## res2 <- NMscanData(file=file.lst, quiet=T, order.columns = F, merge.by.row=FALSE, check.time = FALSE,rep.count=T)
    ## dim(res)

    fix.time(res)
    
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
    ## data.table(attributes(readRDS(fileRef))$meta$variables$variable,attributes(res1)$meta$variables$variable)
})


test_that("Modifications to column names in $INPUT",{

    fileRef <- "testReference/NMscanData2.rds"
    ## res.ref <- readRDS(fileRef)
    
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr002.lst")
    file.lst <- "testData/nonmem/xgxr002.lst"

    res1 <- NMscanData(file=file.lst, check.time = FALSE, merge.by.row=FALSE, quiet=T)

    res <- list(
        NMinfo(res1,"input.colnames"),
        NMinfo(res1,"columns"),
        colnames(res1)
    )

    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))

})

test_that("No translation of column names in $INPUT",{

    fileRef <- "testReference/NMscanData2b.rds"
    ## res.ref <- readRDS(fileRef)
    
    file.lst <- "testData/nonmem/xgxr002.lst"

    res1 <- NMscanData(file=file.lst, check.time = FALSE, merge.by.row=FALSE, translate.input = T, quiet=T)
    res2 <- NMscanData(file=file.lst, check.time = FALSE, merge.by.row=FALSE, translate.input = F, quiet=T)

    dt.cnames <- data.table(colnames(res1),colnames(res2))
    expect_equal_to_reference(dt.cnames,fileRef,version=2)

})


test_that("Multiple output table formats",{

    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData_03.rds"
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")
    file.lst <- "testData/nonmem/xgxr003.lst"


    ## res <- NMscanData(file=file.lst)
    res <- NMscanData(file=file.lst, check.time = FALSE, merge.by.row=FALSE, quiet=T)
    fix.time(res)

    
    expect_equal_to_reference(res,fileRef,version=2)

    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))

    ## only meta
    ## expect_equal(NMinfo(res),NMinfo(readRDS(fileRef)))

})

test_that("Interpret IGNORE statement",{

    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData4.rds"
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    file.lst <- "testData/nonmem/xgxr004.lst"

    ## res <- NMscanData(file=file.lst)
    ## res <- NMscanData(file=file.lst)

    res <- NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE, quiet=T)
    fix.time(res)
    ## names(res$row)
    
    expect_equal_to_reference(res,fileRef,version=2)

    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
})


test_that("List of ACCEPT statements and vs separate statements",{
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    ## file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    ## file2.lst <- NMdata_filepath("examples/nonmem/xgxr007.lst")
    file1.lst <- "testData/nonmem/xgxr006.lst"
    file2.lst <- "testData/nonmem/xgxr007.lst"

    NMreadSection(file1.lst,section="PROBLEM")
    NMreadSection(file2.lst,section="PROBLEM")
    res1 <- NMscanData(file=file1.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE, quiet=T)
    res2 <- NMscanData(file=file2.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE, quiet=T) 
    setattr(res1,"NMdata",NULL)
    setattr(res2,"NMdata",NULL)
    expect_identical(res1,res2)
})


test_that("merge by filters or not",{
    ##    fileRef <- "testReference/NMscanData4.rds"
    ## file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    ## file2.lst <- NMdata_filepath("examples/nonmem/xgxr008.lst")
    file1.lst <- "testData/nonmem/xgxr006.lst"
    file2.lst <- "testData/nonmem/xgxr008.lst"

    
    ## NMreadSection(file1.lst,section="PROBLEM")
    ## NMreadSection(file2.lst,section="PROBLEM")

    res1 <- NMscanData(file=file1.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE, quiet=T)
    res2 <- NMscanData(file=file2.lst,merge.by.row=FALSE,col.model=NULL,check.time = FALSE, quiet=T)
    setnames(res2,"EFF0","eff0",skip_absent=T)
    setcolorder(res1,colnames(res2))

    ## the var tables are different because ROW is input in one,
    ## output in the other. This is as expected.

    ## cbind(attr(res1,"var"),attr(res2,"var"))
    setattr(res1,"NMdata",NULL)
    setattr(res2,"NMdata",NULL)

    expect_equal(res1,res2)
})

### BUG 011. NA rows used for fo table. 0 IDs.
#### it is the rare situation where
##### there is only first-only output (nmout=FALSE everywhere)
##### col.id is not in first-only table

##### nmout does not exactly represent the split we need here. We need
##### to know which rows were enriched by output.

test_that("Only a firstonly without ID but with ROW",{
### This should work because ROW is in firstonly table.
    NMdataConf(reset=TRUE)
    ## NMdataConf(as.fun="data.table")
    fileRef <- "testReference/NMscanData_11.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    file.lst <- "testData/nonmem/xgxr011.lst"
    ## NMreadSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

### impossible with filters at the moment. Cannot be implemented because id level cannot be merged onto row level input.
    expect_error(
        expect_warning(
            NMscanData(file=file.lst,merge.by.row=FALSE,col.row="ROW",check.time = FALSE, quiet=T)
        )
    )

    ## merge.by.row should be able to merge. 
    res <-
        expect_warning(
            NMscanData(file=file.lst,merge.by.row=TRUE,col.row="ROW",check.time = FALSE, quiet=T)
        )
    fix.time(res)
    expect_equal_to_reference(res,fileRef,version=2)
    ## without meta
    ## expect_equal(unNMdata(res),unNMdata(readRDS(fileRef)))
    ## ref <- readRDS(fileRef)
    
})



test_that("Only a firstonly, no ID, no ROW",{
### use.input= TRUE, cbind.by.filters=FALSE.
    ## This should give an error because input cannot be used even though it is requested

    ## this one only outputs a firstonly that cannot be merged onto
    ## input. use.input=T so input data should be returned.

    fileRef <- "testReference/NMscanData12.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr012.lst")
    file.lst <- "testData/nonmem/xgxr012.lst"
    ## NMreadSection(file.lst,section="DATA")
    ## NMreadSection(file.lst,section="PROBLEM")
    ## NMreadSection(file.lst,section="TABLE")
    
    expect_error(
        expect_warning(
            res1 <- NMscanData(file=file.lst,check.time = FALSE, quiet=T)
        )
    )
})


test_that("FO and row-level output. No ID, no row.",{

#### merge.by.row = "ifAvailable" or FALSE
    ## Only row-level output returned because cbind.by.filters=F, and firstonly is without ID and row. Warning that firstonly is dropped. Correct. 

    fileRef <- "testReference/NMscanData_13.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    file.lst <- "testData/nonmem/xgxr013.lst"
    NMreadSection(file.lst,section="PROBLEM")
    ## NMreadSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")
    NMreadSection(file.lst,section="TABLE")

    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst,check.time = FALSE, quiet=T)
    )
    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

    ## merge.by.row=F cannot combine and returns output. OK
    res2 <- expect_warning(
        NMscanData(file=file.lst,check.time = FALSE,merge.by.row=T, quiet=T)
    )

    ## if we leave use.input out and can only get row level. OK
    res2 <- expect_warning(
        NMscanData(file=file.lst,check.time = FALSE,use.input=F, quiet=T)
    )

    
})



### uses ACCEPT: DOSE.GT.20 explining only 731 rows
test_that("FO and row-level output. No ID, no row. cbind.by.filters=T",{
    ## row-level output+input returned because cbind.by.filters=T, and firstonly is without ID and row. Correct. 
    fileRef <- "testReference/NMscanData_14.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    file.lst <- "testData/nonmem/xgxr013.lst"
    NMreadSection(file.lst,section="PROBLEM")
    
    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE, quiet=T)
    )
    fix.time(res1)
    summary(res1)$variables
    summary(res1)$tables
    summary(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

})


## bug. This should return a meaningful error msg
## "bug" confirmed after release of 0.0.16. 
test_that("Only a firstonly without ID but with ROW",{
### cbind.by.filters is TRUE, so ROW is not used to recover firstonly data.

    fileRef <- "testReference/NMscanData15.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    file.lst <- "testData/nonmem/xgxr011.lst"
    NMreadSection(file.lst,section="DATA")
    NMreadSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- expect_error(
                                        #expect_warning(
        NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE, quiet=T)
        ## )
    )

    ##    tabs=NMscanTables(file=file.lst)
    ## tabs
})

test_that("Only a firstonly without ID but with ROW. Using merge.by.row=TRUE.",{
### ROW is used to recover firstonly data.
    ##    NMdataConf(as.fun="data.table")
    ## NMdataConf(as.fun=NULL)
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData_15b.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    file.lst <- "testData/nonmem/xgxr011.lst"
    NMreadSection(file.lst,section="DATA")
    NMreadSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- expect_warning(
        NMscanData(file=file.lst,col.row="ROW",merge.by.row=TRUE,check.time = FALSE, quiet=T)
    )
    
    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})

### recoverRows without a row identifier

test_that("recoverRows without a row identifier",{

    fileRef <- "testReference/NMscanData_16.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    file.lst <- "testData/nonmem/xgxr004.lst"
    NMreadSection(file.lst,section="DATA")
    NMreadSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. 
    ## tabs <- NMscanTables(file.lst)
    ## tabs
    res1 <- NMscanData(file=file.lst,merge.by.row=FALSE,recover.rows = T,as.fun="data.table",check.time = FALSE, quiet=T)
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

    fileRef <- "testReference/NMscanData_17.rds"

    file.lst <- "testData/nonmem/xgxr004.lst"
    NMreadSection(file.lst,section="DATA")
    NMreadSection(file.lst,section="TABLE")

    res1 <- NMscanData(file=file.lst,merge.by.row=FALSE,recover.rows = T,as.fun=as.data.frame,check.time = FALSE, quiet=T)
    dim(res1)
    class(res1)
    with(res1,table(nmout,DOSE))

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

#### explore
    ## ref <- readRDS(fileRef)
    ## compareCols(res1,ref)

    ## setorder(ref,ROW)
    ## expect_equal(
    ##     res1,ref
    ## )

    ## ref <- readRDS(fileRef)
    ## names(attributes(res1))
    ## names(attributes(ref))

})



### get a tibble
test_that("use as.fun to get a tibble",{
### cbind.by.filters is TRUE, so ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData_18.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    file.lst <- "testData/nonmem/xgxr004.lst"
    NMreadSection(file.lst,section="DATA")
    NMreadSection(file.lst,section="TABLE")

### notice that DV PRED RES WRES are returned in firstonly. 
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,merge.by.row=FALSE,recover.rows = T,as.fun=tibble::as_tibble,check.time = FALSE, quiet=T)
    dim(res1)
    class(res1)

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

###### explore diffs
    ##     ref <- readRDS(fileRef)
    ##     compareCols(res1,ref)
    ##     setorder(ref,ROW)
    ##     expect_equal(
    ##         unNMdata(res1),unNMdata(ref)
    ##     )

    ##     ref <- readRDS(fileRef)
    ##     names(attributes(res1))
    ##     names(attributes(ref))
    ## class(res1)
    ## class(ref)
    
})


## test the dir structure with input.txt/output.txt 

test_that("dir structure with input.txt/output.txt",{
    ## options(NMdata.as.fun="none")
    
    ## options(NMdata.file.mod=function(file) file.path(dirname(file),"input.txt"))
    ## options(NMdata.modelname=function(file) basename(dirname(normalizePath(file))))

    NMdataConf(reset=TRUE)
    NMdataConf(as.fun="data.table")
    ## NMdataConf(file.mod=function(file) file.path(dirname(file),"input.txt"))
    NMdataConf(file.mod=identity)
    NMdataConf(modelname=function(file) basename(dirname(normalizePath(file))))
    
    ## filedir.lst <- file.nm("xgxr001dir/output.txt")
    filedir.lst <- "testData/nonmem/xgxr001dir/output.txt"
    res1dir <- NMscanData(filedir.lst,check.time = FALSE,merge.by.row=F, quiet=T)

    ## test model names from NMinfo and in result data
    expect_equal(NMinfo(res1dir,"details")$model,"xgxr001dir")
    umod <- unique(res1dir[,model])
    expect_equal(length(umod),1)
    expect_equal(umod,"xgxr001dir")


})


#### compare mod/lst vs input/output
## this test isn't ready. 
### Todo: Delete components from NMinfo and then test equality

test_that("input.txt/output.txt - unset modelname",{
    NMdataConf(reset=TRUE)
    NMdataConf(as.fun="data.table")
    ## NMdataConf(file.mod=function(file) file.path(dirname(file),"input.txt"))
    NMdataConf(file.mod=identity)
    NMdataConf(modelname=function(file) basename(dirname(normalizePath(file))))
    
    ## filedir.lst <- file.nm("xgxr001dir/output.txt")
    filedir.lst <- "testData/nonmem/xgxr001dir/output.txt"
    res1dir <- NMscanData(filedir.lst,check.time = FALSE,merge.by.row=T, quiet=T)

    NMdataConf(file.mod="default")
    NMdataConf(modelname=NULL)
    
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    file.lst <- "testData/nonmem/xgxr001.lst"
    res1 <- NMscanData(file=file.lst,check.time = FALSE, quiet=T)

    ## unNMdata(res1)
    ## unNMdata(res1dir)

    ## this test isn't ready. How do I execute the input.txt/output.txt model?

    els.details <- c("call","model","file.lst","file.mod")
    for(elem in els.details){
        attributes(res1)$NMdata$details[elem] <- NULL
        attributes(res1dir)$NMdata$details[elem] <- NULL
    }
    
    attributes(res1)$NMdata$datafile$DATA <- NULL
    attributes(res1dir)$NMdata$datafile$DATA <- NULL

    attributes(res1)$NMdata$datafile$string <- NULL
    attributes(res1dir)$NMdata$datafile$string <- NULL

    fix.time(res1)
    meta.res1 <- NMinfo(res1)
    meta.res1$details$logtime.lst <- NULL
    setattr(res1,"NMdata",meta.res1)

    fix.time(res1dir)
    meta.res1dir <- NMinfo(res1dir)
    meta.res1dir$details$logtime.lst <- NULL
    setattr(res1dir,"NMdata",meta.res1dir)
    
    ## these differ a little in the two estimates
    cols.differ <- c("TVKA","TVCL","TVV3","TVQ","KA","CL","V3","Q","V2","IPRED","PRED","RES","WRES")
    res1[,(cols.differ):=NULL]
    res1dir[,(cols.differ):=NULL]

    expect_equal(res1[,!("model")],res1dir[,!("model")])

    NMdataConf(as.fun=NULL)
})


test_that("output.txt, file.mod=identity - NMinfo file.mod=output.txt?",{

    NMdataConf(reset=TRUE)
    NMdataConf(as.fun="data.table")
    NMdataConf(file.mod=identity)
    NMdataConf(modelname=function(file) basename(dirname(normalizePath(file))))
    
    ## filedir.lst <- file.nm("xgxr001dir/output.txt")
    filedir.lst <- "testData/nonmem/xgxr001dir/output.txt"
    res1 <- NMscanData(filedir.lst,check.time = FALSE,merge.by.row=F, quiet=T)

    expect_equal(basename(NMinfo(res1,"details")$file.lst),"output.txt")
})
## 


test_that("Duplicate columns in input data",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData_20.rds"
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr015.lst")
    file.lst <- "testData/nonmem/xgxr015.lst"

    ## debugonce(NMscanData)
    res <- expect_warning(
        NMscanData(file=file.lst,merge.by.row=FALSE,check.time = FALSE, quiet=T)
    )
    fix.time(res)
    ## names(res$row)
    
    expect_equal_to_reference(res,fileRef,version=2)
})


### this is not a real test. Need to be able to test how the merges were performed. 
## test_that("col.row and merge.by.row=TRUE from NMdataConf",{

##     NMdataConf(reset=TRUE)
##     NMdataConf(col.row="ROW", merge.by.row=TRUE)
##     NMdataConf()

##     file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")


##     res1 <- NMscanData(file=file.lst)
## })


test_that("Modifying row identifier",{
    NMdataConf(reset=TRUE)
    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr016.lst")
    file.lst <- "testData/nonmem/xgxr016.lst"

    res <-
        expect_error(
            NMscanData(file=file.lst,merge.by.row=TRUE,check.time = FALSE, quiet=T)
        )

})

test_that("merge.by.row=ifAvailable when available",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMscanData_21.rds"

    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    
    res1 <- NMscanData(file=file.lst,merge.by.row="ifAvailable",check.time=FALSE, quiet=T)
    ## dim(res1)

### we don't need metadata for this test
    unNMdata(res1)
    
    expect_equal_to_reference(res1,fileRef,version=2)

})


test_that("merge.by.row=ifAvailable when not available",{

    fileRef <- "testReference/NMscanData_22.rds"

    ## file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    file.lst <- "testData/nonmem/xgxr004.lst"


    res1 <- NMscanData(file=file.lst,merge.by.row="ifAvailable",recover.rows = T,as.fun="data.table",check.time = FALSE, quiet=T)
    dim(res1)
    res1[,table(nmout,DOSE)]
    fix.time(res1)

    ## compareCols(readRDS(fileRef),res1)
    
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
##### explore diffs
    ## ref <- readRDS(fileRef)
    ## compareCols(res1,ref)
    ## setorder(ref,ROW)
    ## expect_equal(
    ##     unNMdata(res1),unNMdata(ref)
    ## )

    
})

test_that("col.row does not exist, but merge.by.row==TRUE",{
### col.row does not exist, but merge.by.row==TRUE
    fileRef <- "testReference/NMscanData_22b.rds"
    NMdataConf(reset=T)
    NMdataConf(check.time=F)
    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    
    ## warning becauses the merge was not possible even though merge.by.row
    res1 <- 
        expect_warning(
            NMscanData(file=file.lst,col.row="NONEXIST",merge.by.row=TRUE, quiet=T)
        )
    fix.time(res1)
    expect_equal_to_reference(res1,fileRef)

##### explore diffs
    ## ref <- readRDS(fileRef)
    ## indices( attributes(res1)$NMdata$tables)
    ## indices(attributes(ref)$NMdata$tables)

    
    
})


test_that("col.row is NULL, but merge.by.row==TRUE",{
### col.row is NULL, but merge.by.row==TRUE

    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    
    ## error is that it's not in output. It's not in input either though
    expect_error(
        res1=NMscanData(file=file.lst,col.row=NULL,merge.by.row=TRUE, quiet=T)
    )

    
})


test_that("A filter without operator",{

    fileRef <- "testReference/NMscanData_23.rds"

    file.lst <- "testData/nonmem/xgxr010.lst"

    res1 <- NMscanData(file=file.lst,merge.by.row="ifAvailable",as.fun="data.table",check.time = FALSE, quiet=T)
    res1[,.N,by=.(DOSE)]
    fix.time(res1)
    
    expect_equal_to_reference(
        res1,fileRef,version=2
    )
    
})


test_that("Including a redundant output table",{
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    fileRef <- "testReference/NMscanData_24.rds"
    ## file.lst <- file.nm("xgxr019.lst")
    file.lst <- "testData/nonmem/xgxr019.lst"

    ## notice no cols are taken from the redundant table - correct
    res1 <- NMscanData(file=file.lst,merge.by.row="ifAvailable",as.fun="data.table",check.time = FALSE, quiet=T)
    ##     tabs1 <- NMscanTables(file=file.lst,as.fun="data.table",details=T,rep.count=F)
    ##     tabs1$meta
    ## tabs1$data[[4]]
    ## NMinfo(res1,"tables")

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

    ## inp1 <- NMscanInput(file=file.lst)
    
}
)


## this one has a commented DATA section and an extra DV in output table
test_that("redundant output",{
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
### ctl file naming is supported by default now
    ## NMdataConf(file.mod=function(x)sub("\\.lst$",".ctl",x))
    NMdataConf(check.time=FALSE)

    fileRef <- "testReference/NMscanData_25.rds"
    file.lst <- "testData/nonmem/estim_debug.ctl"

    ## notice no cols are taken from the redundant table - correct
    res1 <- expect_message(
        NMscanData(file=file.lst, quiet=T)
    )


    ##     tabs1 <- NMscanTables(file=file.lst,as.fun="data.table",details=T,rep.count=F)
    ##     tabs1$meta
    ## tabs1$data[[4]]
    ## NMinfo(res1,"tables")

    fix.time(res1)
    expect_equal_to_reference(
        res1,fileRef,version=2
    )

    ## inp1 <- NMscanInput(file=file.lst)
    
}
)


## check.time (warning)
### I'm afraid this could give warnings when run in check or on CRAN. 
if(FALSE){
    test_that("check time warning",{
        NMdataConf(reset=T)
        fileRef <- "testReference/NMscanData26.rds"
        
        ## file.lst <- "testData/nonmem/xgxr001.lst"

        dir.test <- "testData/nonmem/check.time"
        if(dir.exists(dir.test)) unlink(dir.test,recursive=TRUE)
        dir.create(dir.test)


        file.copy("testData/nonmem/xgxr001.lst",dir.test)
        file.copy("testData/nonmem/xgxr001_res.txt",dir.test)
        file.copy("testData/nonmem/xgxr001.mod",dir.test)

        file.lst <- file.path(dir.test,"xgxr001.lst")
        file.mod <- fnExtension(file.lst,".mod")
        data.new <- sub("../data","../../data",NMreadSection(file.mod,section="data"))
        NMwriteSection(file.mod,section="data",newlines=data.new)
        
        res1 <- expect_warning(
            NMscanData(file=file.lst, quiet=F, order.columns = F, merge.by.row=FALSE, quiet=T)
        )
        ## dim(res1)'
        
        ## without meta
        expect_equal_to_reference(unNMdata(res1),fileRef)
        ## data.table(attributes(readRDS(fileRef))$meta$variables$variable,attributes(res1)$meta$variables$variable)
    })
}

test_that("$INPUT copy",{
    NMdataConf(reset=T)
    NMdataConf(check.time=FALSE)
    
    file.lst.1 <- "testData/nonmem/xgxr022.lst"
    res.1 <- NMscanData(file.lst.1, quiet=T)

    ## NMinfo(res,"input.colname")
    NMinfo(res.1,"input.colnames")
    NMinfo(res.1,"columns")

    file.lst.2 <- "testData/nonmem/xgxr001.lst"
    res.2 <- NMscanData(file.lst.2, quiet=T)

    expect_equal(ncol(res.1)-ncol(res.2),1)
    expect_equal(setdiff(colnames(res.1),colnames(res.2)),c("COMP","EFF0"))

    cols.1 <- NMinfo(res.1,"columns")
    cols.2 <- NMinfo(res.2,"columns")

    expect_equal(setdiff(cols.1$variable,cols.2$variable),c("COMP","EFF0"))
    expect_equal(setdiff(cols.2$variable,cols.1$variable),c("eff0"))

})

test_that("only firstonly. Has col.id, no col.row.",{
######  trickers this 
    ## use.input&&!any(tables$meta$full.length)
    NMdataConf(reset=T)
    NMdataConf(check.time=FALSE)

    
    fileRef <- "testReference/NMscanData_27.rds"
    
    file.lst <- "testData/nonmem/xgxr023.lst"
    res <- NMscanData(file.lst,quiet=TRUE)
    res <- fix.time(res)

    expect_equal_to_reference(res,fileRef)

})


test_that("Two firstonly, one full-length",{

    fileRef <- "testReference/NMscanData_28.rds"
    file.lst <- "testData/nonmem/xgxr025.lst"

    res <- NMscanData(file=file.lst,check.time=F, quiet=T)
    res <- fix.time(res)
    ## ref <- readRDS(fileRef)
    
    expect_equal_to_reference(res,fileRef,version=2)
    
})

test_that("Two firstonly, one full-length with col.nmrep",{
    NMdataConf(reset=TRUE)
#### NMREP is now added to the number of columns taken from the
#### output table that has NMREP. Is that what we want? Or +1? +1 is
#### very complicated for user. Maybe better: if col.nmrep, it is treated like any other column, but in NMinfo(,"tables") there is a column, hasTABLENO
    fileRef <- "testReference/NMscanData_28b.rds"
    file.lst <- "testData/nonmem/xgxr025.lst"

    res <- NMscanData(file=file.lst,check.time=F,col.nmrep=T, quiet=T)
    res <- fix.time(res)
    ##  ref <- readRDS(fileRef)
    
    expect_equal_to_reference(res,fileRef,version=2)
    
})


test_that("Input data as character",{

    fileRef <- "testReference/NMscanData29.rds"
    file.lst <- "testData/nonmem/xgxr026.lst"

    expect_error(NMscanData(file=file.lst,check.time=F))
    
    res <- NMscanData(file=file.lst,check.time=F,use.rds=F, quiet=T)
    sapply(res,class)
    res <- NMscanData(file=file.lst,check.time=F,use.rds=T,merge.by.row = F, quiet=T)
    sapply(res,class)
    
    ## res <- fix.time(res)
    ## ## ref <- readRDS(fileRef)
    
    ## expect_equal_to_reference(res,fileRef,version=2)
    
})



test_that("Input control stream missing",{
    NMdataConf(reset=TRUE)
    NMdataConf(file.mod="doesNotExist.mod")

    fileRef <- "testReference/NMscanData_30.rds"
    file.lst <- "testData/nonmem/xgxr001.lst"
    res <- NMscanData(file=file.lst,use.input=F, quiet=T)
    ## check file.mod wasn't used. Notice, merge.by.row is still "ifAvailable" - this was never needed because use.input=FALSE.
    ##    NMinfo(res1a,"details")
    fix.time(res)
    expect_equal_to_reference(res,fileRef,version=2)
    
})


test_that("simulation model with subproblems",{
    NMdataConf(reset=TRUE)

    fileRef <- "testReference/NMscanData_31.rds"
    
### sim a model with subproblems
    ## library(devtools)
    ## load_all("~/wdirs/NMexec")
    ## file.mod <- "testData/nonmem/xgxr014.mod"
    ## ## NMexec(file.mod,sge=FALSE,wait=T)
    ## doses <- NMcreateDoses(TIME=0,AMT=data.table(AMT=c(10,100),DOSE=c(10,100)))
    ## simdat <- addEVID2(doses,time.sim=1:24,CMT=2)
    ## simdat[,DV:=NA][
    ##     ,ROW:=.I]
    ## NMcheckData(simdat)

    ## NMdataConf(as.fun="data.table")

    ## sim1 <- NMsim(file.mod,data=simdat,
    ##               suffix.sim="testsim1",dir.sim="testData/simulations"
    ##              ,seed=343108,subproblems=100,nmquiet=T)

### NMsim saves serialized rds. Resaving as version 2
    ## res.sim <- readRDS("testData/simulations/NMsimData_xgxr014_testsim1.rds")
    ## saveRDS(res.sim,"testData/simulations/NMsimData_xgxr014_testsim1.rds",version=2)
##### sim done
    
    res <- NMscanData("testData/simulations/xgxr014_testsim1.lst",check.time=F, quiet=T)
    
    fix.time(res)
    meta.x <- attr(res,"NMdata")
    meta.x$details$time.ok <- NULL
    setattr(res,"NMdata",meta.x)
    
    expect_equal_to_reference(res,fileRef,version=2)
}
)


test_that("csv vs rds vs fst",{

    fileRef <- "testReference/NMscanData_32.rds"

    file.lst <- "testData/nonmem/xgxr014.lst"


    
    res.csv <- NMscanData(file=file.lst, order.columns = F, merge.by.row=FALSE, check.time = FALSE,formats.read=cc(csv), quiet=T)

    res.rds <- NMscanData(file=file.lst, order.columns = F, merge.by.row=FALSE, check.time = FALSE,formats.read=cc(rds,csv), quiet=T)

    res.fst <- NMscanData(file=file.lst, order.columns = F, merge.by.row=FALSE, check.time = FALSE,formats.read=cc(fst,csv), quiet=T)

    res <- dims(res.csv,res.rds,res.fst)

    expect_equal_to_reference(res,fileRef,version=2)
    
})
