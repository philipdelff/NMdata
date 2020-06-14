context("NMtransInput")

test_that("basic",{
    
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    res1 <- NMtransInput(file=file.lst,applyFilters = T)

    expect_equal_to_reference(res1,fileRef)
})
