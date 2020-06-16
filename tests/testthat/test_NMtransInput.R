context("NMtransInput")

test_that("basic",{

    fileRef <- "testReference/NMtransInput.rds"
    file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
    res1 <- NMtransInput(file=file.lst,applyFilters = T)

    expect_equal_to_reference(res1,fileRef)
})


test_that("single = filter",{
    
    file.lst <- NMdata_filepath("examples/nonmem/xgxr009.lst")
    ## NMgetSection(file.lst,section="PROBLEM")
    ## NMgetSection(file.lst,section="DATA")
    res1 <- NMtransInput(file=file.lst,applyFilters = T)
    expect_equal(res1[,unique(DOSE)],10)
    
})


