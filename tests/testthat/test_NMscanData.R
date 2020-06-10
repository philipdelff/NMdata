context("NMscanData")

test_that("basic",{

    fileRef <- "testReference/NMscanData1.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr001.lst")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    res1 <- NMscanData(file=file.lst)

    expect_equal_to_reference(res1,fileRef)
})
