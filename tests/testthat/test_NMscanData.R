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
    res <- NMscanData(file=file.lst)

    expect_equal_to_reference(res,fileRef)
})

test_that("Interpret IGNORE statement",{
    fileRef <- "testReference/NMscanData4.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")

    ## res <- NMscanData(file=file.lst,debug=T)
    ## res <- NMscanData(file=file.lst,debug=F)
    res <- NMscanData(file=file.lst,mergeByFilters = T, debug=F)

    names(res$row)
    
    expect_equal_to_reference(res,fileRef)
})



test_that("List of ACCEPT statements and vs separate statements",{
    ##    fileRef <- "testReference/NMscanData4.rds"
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

    setcolorder(res1$row,colnames(res2$row))
    
    lapply(res1,dim)
    lapply(res2,dim)

    expect_equal(res1,res2)
})
