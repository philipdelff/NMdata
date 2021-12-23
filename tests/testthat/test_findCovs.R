
test_that("basic",{
    fileRef <- "testReference/findCovs_1.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res <- findCovs(pk)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("with by",{
    fileRef <- "testReference/findCovs_2.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    res <- findCovs(pk,by="ID")
    expect_equal_to_reference(res,fileRef,version=2)
})

## 
test_that("deprecated cols.id",
          expect_warning(findCovs(data.frame(x=1),cols.id="w"))
          )
