
context("NMextractDataFile")

test_that("basic",{

    fileRef <- "testReference/NMextractDataFile_1.rds"
    file.lst <- system.file("examples/nonmem/xgxr004.lst",package="NMdata")
    res1 <- NMextractDataFile(file.lst)
    ## these have to be non-absolute paths to work across different
    ## testing environments
    res1$path <- basename(res1$path)
    res1$path.rds <- basename(res1$path.rds)

    expect_equal_to_reference(res1,fileRef,version=2)
})
