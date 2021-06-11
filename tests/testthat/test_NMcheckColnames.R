context("NMcheckColnames")
NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}


test_that("basic",{

    fileRef <- "testReference/NMcheckColnames_1.rds"

    file.lst <- "../../inst/examples/nonmem/run001.lst"
    
    res1 <- NMcheckColnames(file=file.lst)
    ## dim(res1)

    expect_equal_to_reference(res1,fileRef,version=2)
})
