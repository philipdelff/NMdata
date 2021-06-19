context("NMorderColumns")

test_that("last NULL",{

    fileRef <- "testReference/NMorderColumns_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    pk1 <- NMorderColumns(pk)
    pk2 <- NMorderColumns(pk,last=c("FLAG",NULL))
    pk3 <- NMorderColumns(pk,last=c(NULL))


    expect_equal(colnames(pk1),colnames(pk3))

    dtres <- data.table(colnames(pk1),colnames(pk2),colnames(pk3))

    expect_equal_to_reference(
        dtres
       ,fileRef)

})
