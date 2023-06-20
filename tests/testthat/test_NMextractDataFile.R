
context("NMextractDataFile")

test_that("basic",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMextractDataFile_1.rds"
    ## file.lst <- system.file("examples/nonmem/xgxr004.lst",package="NMdata")
    file.lst <- "testData/nonmem/xgxr004.lst"
    res1 <- NMextractDataFile(file.lst)
    ## these have to be non-absolute paths to work across different
    ## testing environments
    res1$path.csv <- basename(res1$path.csv)
    res1$path.rds <- basename(res1$path.rds)
    res1$path.fst <- basename(res1$path.fst)

    expect_equal_to_reference(res1,fileRef,version=2)
})

