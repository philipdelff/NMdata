context("NMreadSection")

test_that("basic",{

    fileRef <- "testReference/NMreadSection_1.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- system.file("examples/nonmem/xgxr001.lst",package="NMdata")

    res1 <- NMreadSection(file=file.lst,section="DATA")
    ## res1 <- NMreadSection(file=file.lst,section="DATA",debug=T)

    expect_equal_to_reference(res1,fileRef,version=2)
})
