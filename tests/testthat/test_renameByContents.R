## load_all("c:/Users/delff/working_copies/NMdata")

## renameByContents(pk,fun.test=NMisNumeric,fun.rename=tolower,invert.test=T)

## pk2 <- renameByContents(pk[,!("trtact")],fun.test=NMisNumeric,fun.rename=tolower,invert.test=T)
## pk2

context("renameByContents")

test_that("basic",{

    fileRef <- "testReference/renameByContents_1.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,trtact:=NULL]
    pk <- renameByContents(data=pk,
                           fun.test=NMisNumeric,
                           fun.rename = tolower,
                           invert.test = TRUE)

    expect_equal_to_reference(pk,fileRef)

})
