context("print.summary_NMdata")
test_that("basic",{

    fileRef <- "testReference/print_summary_NMdata_01.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    file.lst <- "testData/nonmem/xgxr001.lst"

    res <- capture_output(NMscanData(file=file.lst,  order.columns = F, merge.by.row=FALSE, check.time = FALSE))
    
    
    expect_equal_to_reference(res,fileRef,version=2)
})
