
context("NMrelate")

NMdataConf(reset=TRUE)

test_that("basic",{
    file.mod <- "testData/nonmem/xgxr032.mod"
    fileRef <- "testReference/NMrelateOne_01.rds"
    
    res <- list(
        NMrelateOne(file=file.mod,par.type="eta")
       ,
        NMrelateOne(file.mod,par.type="theta")
       ,
        NMrelateOne(file.mod,par.type="sigma")
    )

    expect_equal_to_reference(res,fileRef)
})

test_that("Find all",{

    file.mod <- "testData/nonmem/xgxr032.mod"
    fileRef <- "testReference/NMrelate_01.rds"

    ## getLines(file.mod)
    res <- NMrelate(file=file.mod)
    
    expect_equal_to_reference(res,fileRef)
})


test_that("merge with NMreadExt results",{

    file.mod <- "testData/nonmem/xgxr032.mod"
    fileRef <- "testReference/NMrelate_02.rds"

    res.rel <- NMrelate(file=file.mod)
    res.ext <- NMreadExt(file=file.mod,as.fun="data.table")

    res <- list(
        ext.all=mergeCheck(res.ext[!is.na(par.type)],res.rel,by=cc(model,par.type,i,j),fun.na.by=NULL,all.x=T,common.cols="drop.y")
       ,
        ext.nofix=mergeCheck(res.ext[!is.na(par.type)&FIX==0],res.rel,by=cc(model,par.type,i,j),fun.na.by=NULL,common.cols="drop.y")
       ,
        labs.all=mergeCheck(res.rel,res.ext[!is.na(par.type)],by=cc(model,par.type,i,j),fun.na.by=NULL,all.x=T,common.cols="drop.y")
    )

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        compareCols(ref$ext.nofix,   res$ext.nofix)

    }
    
})


test_that("2 models",{

    file.mod <- c("testData/nonmem/xgxr032.mod",
                  "testData/nonmem/xgxr024.mod")

    fileRef <- "testReference/NMrelate_03.rds"

    res1 <- NMrelate(file=file.mod[1])
    res2 <- NMrelate(file=file.mod[2])
    res <- NMrelate(file=file.mod)

    expect_equal_to_reference(res,fileRef)

    ##    NMrelateOne(file.mod[2])
})
