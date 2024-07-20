
context("NMrelate")

### sigma doesn't quite work
test_that("basic",{
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMrelateOne(file=file.mod,par.type="eta")
    NMrelateOne(file.mod,par.type="theta")
    NMrelateOne(file.mod,par.type="sigma")
})

test_that("Find all",{

    file.mod <- "testData/nonmem/xgxr032.mod"
    ## getLines(file.mod)
    res <- NMrelate(file=file.mod)
    res    
})


test_that("merge with NMreadExt results",{

    file.mod <- "testData/nonmem/xgxr032.mod"
    res.rel <- NMrelate(file=file.mod)
    res.ext <- NMreadExt(file=file.mod,as.fun="data.table")

    mergeCheck(res.ext[!is.na(par.type)],res.rel,by=cc(model,par.type,i,j),fun.na.by=NULL,all.x=T)
    
    
})


test_that("2 models",{

    file.mod <- c("testData/nonmem/xgxr032.mod",
                  "testData/nonmem/xgxr024.mod")
    ## getLines(file.mod)
    res <- NMrelate(file=file.mod)

    res
})
