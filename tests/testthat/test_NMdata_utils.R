context("NMdata Utils")
NMdataConf(reset=TRUE)

NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}


test_that("basic",{
    NMdataConf(reset=T)
    
    file.lst.1 <- system.file("examples/nonmem/xgxr001.lst" ,package="NMdata")

    res1 <- NMscanData(file=file.lst.1, quiet=T, order.columns = F, merge.by.row=FALSE, check.time = FALSE)

    file.lst.2 <- NMdata_filepath("examples/nonmem/xgxr002.lst")

    res2 <- NMscanData(file=file.lst.2, check.time = FALSE, merge.by.row=FALSE,quiet=TRUE)

    ## can't rbind data.frames - cols don't match
    expect_error(rbind(res1,res2,fill=T))

    NMdataConf(as.fun="data.table")
    res1 <- NMscanData(file=file.lst.1, quiet=T, order.columns = F, merge.by.row=FALSE, check.time = FALSE)
    res2 <- NMscanData(file=file.lst.2, check.time = FALSE, merge.by.row=FALSE,quiet=TRUE)

    expect_equal(is.NMdata(res1),TRUE)
    
    res <- rbind(res1,res2,fill=T)
    expect_equal(nrow(res),1810)
    expect_equal(class(res),c("data.table","data.frame"))
    
})
