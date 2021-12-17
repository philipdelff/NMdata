test_that("basic",{
    fileRef <- "testReference/NMisNumeric_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- lapply(pk,NMisNumeric)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("many element-wise",{
    fileRef <- "testReference/NMisNumeric_2.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- pk[,lapply(.SD,NMisNumeric,each=T)]

    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("element-wise",{
    fileRef <- "testReference/NMisNumeric_3.rds"

    res <- NMisNumeric(c("1","1p"),each=T)

    expect_equal_to_reference(res,fileRef,version=2)
})
