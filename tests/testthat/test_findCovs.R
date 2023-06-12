context("findCovs")

test_that("basic",{
    fileRef <- "testReference/findCovs_1.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    unNMdata(pk)
    res <- findCovs(pk)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("with by",{
    fileRef <- "testReference/findCovs_2.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    unNMdata(pk)
    
    res <- findCovs(pk,by="ID")
    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("with by of length 2",{
    fileRef <- "testReference/findCovs_3.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    unNMdata(pk)
    res <- findCovs(pk,by=c("ID","trtact"))
    expect_equal_to_reference(res,fileRef,version=2)
})


## 
test_that("deprecated cols.id",{
    expect_message(
        findCovs(data.frame(x=rep(1:2,each=2),y=c(rep(c("a"),2),"v","w")),cols.id="x")
    )
    expect_error(
        findCovs(data.frame(x=rep(1:2,each=2),y=c(rep(c("a"),2),"v","w")),cols.id="x",by="x")
    )
})
