NMdataConf(reset=TRUE)
test_that("basic",{

    fileRef <- "testReference/NMcheckInput_01.rds"

    file.lst <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")
 
    res1 <- NMcheckInput(file=file.lst)

    expect_equal_to_reference(res1,fileRef)
    

    })
