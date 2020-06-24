library(devtools)
load_all("~/working_copies/NMdata")
setwd("~/working_copies/NMdata/tests/testthat")

context("NMscanData")

test_that("basic",{

    fileRef <- "testReference/NMscanData1.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    res1 <- NMscanData(file=file.lst)

    expect_equal_to_reference(res1,fileRef)
})


test_that("Modifications to column names in $INPUT",{

    fileRef <- "testReference/NMscanData2.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr002.lst")

    res <- NMscanData(file=file.lst)

    expect_equal_to_reference(res,fileRef)
})


test_that("Multiple output table formats",{

    fileRef <- "testReference/NMscanData3.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")

    ## res <- NMscanData(file=file.lst,debug=T)
    res <- NMscanData(file=file.lst,debug=F)

    expect_equal_to_reference(res,fileRef)
})

test_that("Interpret IGNORE statement",{
    fileRef <- "testReference/NMscanData4.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")

    ## res <- NMscanData(file=file.lst,debug=T)
    ## res <- NMscanData(file=file.lst,debug=F)

    res <- NMscanData(file=file.lst,mergeByFilters = T, debug=F)

    ## names(res$row)
    
    expect_equal_to_reference(res,fileRef)
})



test_that("List of ACCEPT statements and vs separate statements",{
    
    file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    file2.lst <- NMdata_filepath("examples/nonmem/xgxr007.lst")

    NMgetSection(file1.lst,section="PROBLEM")
    NMgetSection(file2.lst,section="PROBLEM")
    res1 <- NMscanData(file=file1.lst,mergeByFilters = T,debug=F,add.name=NULL)
    res2 <- NMscanData(file=file2.lst,mergeByFilters = T, debug=F,add.name=NULL)

    expect_identical(res1,res2)
})


### find out how much can be tested. 
test_that("merge by filters or not",{
    ##    fileRef <- "testReference/NMscanData4.rds"
    file1.lst <- NMdata_filepath("examples/nonmem/xgxr006.lst")
    file2.lst <- NMdata_filepath("examples/nonmem/xgxr008.lst")

    ## NMgetSection(file1.lst,section="PROBLEM")
    ## NMgetSection(file2.lst,section="PROBLEM")
    res1 <- NMscanData(file=file1.lst,mergeByFilters = T,debug=F,add.name=NULL)
    res2 <- NMscanData(file=file2.lst,mergeByFilters = T, debug=F,add.name=NULL)

    setcolorder(res1,colnames(res2))
    
    expect_equal(res1,res2)
})


test_that("Only a firstonly without ID but with ROW",{
### This should work because ROW is in firstonly table.
    
    fileRef <- "testReference/NMscanData11.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs
    
    res1 <- NMscanData(file=file.lst,debug=F)
    expect_equal_to_reference(res1,fileRef)
    
})



test_that("Only a firstonly, no ID, no ROW",{
### use.input is TRUE but mergeByFilters is FALSE. This should give an error because input cannot be used even though it is requested

    ## this one only outputs a firstonly that cannot be merged onto
    ## input. use.input=T so input data should be returned.

    fileRef <- "testReference/NMscanData12.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr012.lst")
    ## NMgetSection(file.lst,section="DATA")
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="TABLE")
    
    expect_error(
        res1 <- NMscanData(file=file.lst,debug=F)
    )
})


test_that("FO and row-level output. No ID, no row.",{
    ## row-level output returned because mergeByFilters=F, and firstonly is without ID and row. Warning that firstonly is dropped. Correct. 
    fileRef <- "testReference/NMscanData13.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")
    
    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst)
    )
    
    expect_equal_to_reference(
        res1,fileRef
    )

})

test_that("FO and row-level output. No ID, no row. mergeByFilters=T",{
    ## row-level output+input returned because mergeByFilters=T, and firstonly is without ID and row. Correct. 
    fileRef <- "testReference/NMscanData14.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr013.lst")
    NMgetSection(file.lst,section="PROBLEM")
    
    ## tabs <- NMscanTables(file=file.lst)
    res1 <- expect_warning(
        NMscanData(file=file.lst,mergeByFilters=T)
    )
    
    expect_equal_to_reference(
        res1,fileRef
    )

})



test_that("Only a firstonly without ID but with ROW",{
### mergeByFilters is TRUE, so ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData15.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr011.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

    ### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,mergeByFilters=T)

})


### recoverRows without a row identifier

test_that("recoverRows without a row identifier",{
### mergeByFilters is TRUE, so ROW is used to recover firstonly data.

    fileRef <- "testReference/NMscanData16.rds"

    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    NMgetSection(file.lst,section="DATA")
    NMgetSection(file.lst,section="TABLE")

    ### notice that DV PRED RES WRES are returned in firstonly. This is horrible.
    ## tabs <- NMscanTables(file.lst)
    ## tabs

    res1 <- NMscanData(file=file.lst,mergeByFilters=T,recoverRows = T)
    dim(res1)
    res1[,table(nmout,DOSE)]
})

