context("lstExtractTime")

test_that("basic",{

    fileRef <- "testReference/lstExtractTime1.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    file.lst <- "testData/nonmem/xgxr001.lst"
    
    res <- lstExtractTime(file=file.lst,tz.lst="America/New_York")
    
    expect_equal_to_reference(res,fileRef,version=2)
    
})

if(F){
    ### this depends on OS
test_that("no tz given",{

    fileRef <- "testReference/lstExtractTime2.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    file.lst <- "testData/nonmem/xgxr001.lst"
    
    res <- lstExtractTime(file=file.lst)
    
    expect_equal_to_reference(res,fileRef,version=2)
    
})
}
