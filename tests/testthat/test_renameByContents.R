## load_all("c:/Users/delff/working_copies/NMdata")

## renameByContents(pk,fun.test=NMisNumeric,fun.rename=tolower,invert.test=T)

## pk2 <- renameByContents(pk[,!("trtact")],fun.test=NMisNumeric,fun.rename=tolower,invert.test=T)
## pk2

context("renameByContents")
library(data.table)

test_that("basic",{

    fileRef <- "testReference/renameByContents_1.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk[,trtact:=NULL]
    pk.0 <- copy(pk)
    pk.1 <- renameByContents(data=pk,
                           fun.test=NMisNumeric,
                           fun.rename = tolower,
                           invert.test = TRUE)
    res <- colnames(pk.1)
    
    ## res <- compareCols(pk,pk.0)
    ## expect_equal_to_reference(res,fileRef)
    expect_equal_to_reference(res,fileRef)
    
    ## pkref <- readRDS(fileRef)
    ## setattr(pk,"NMdata",NULL)
    ## setattr(pkref,"NMdata",NULL)
    ## pk$FLAG2=NULL
    ## pk$flag2=NULL
    ## pk <- NMorderColumns(pk)    
    ## pkref <- NMorderColumns(pkref)
    ## ## this is because flag changed from "pre-dose sample" to "negative time"
    ## pk$flag[pk$flag=="Negative time"] <- "Pre-dose sample"
    ## expect_equal(pk,pkref)

})
