test_that("basic",{

    fileRef <- "testReference/NMinfo_1.rds"
    
    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
    res1 <- NMscanData(file=file.lst,quiet=T,order.columns = F,merge.by.row=FALSE,check.time=F)

    info.cols <- NMinfo(res1,info="columns")
    
    expect_equal_to_reference(info.cols,fileRef,version=2)

})
