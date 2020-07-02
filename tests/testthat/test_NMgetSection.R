context("NMgetSection")

test_that("basic",{

    fileRef <- "testReference/NMgetSection.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- NMdata_filepath("examples/nonmem/run001.lst")
    ## NMgetSection(NMdata_filepath("examples/nonmem/run001.lst"),section="DATA")

    res1 <- NMgetSection(file=file.lst,section="DATA")
    ## res1 <- NMgetSection(file=file.lst,section="DATA",debug=T)

    expect_equal_to_reference(res1,fileRef,version=2)
})
