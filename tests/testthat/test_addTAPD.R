context("addTAPD")

test_that("basic",{
    fileRef <- "testReference/addTAPD_01.rds"

    dat <- readRDS("testData/data/xgxr2.rds")
    res <- addTAPD(data=dat)

    expect_equal_to_reference(res,fileRef,version=2)
    ## dims(res,readRDS(fileRef))
    ## compareCols(res,readRDS(fileRef))
})


test_that("repeated dosing data",{
    fileRef <- "testReference/addTAPD_02.rds"

    dat <- readRDS("testData/data/mad.rds")
    res <- addTAPD(data=dat,as.fun="data.table")

    dims(dat,res)

    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("Custom names and discard one",{
    fileRef <- "testReference/addTAPD_03.rds"

    dat <- readRDS("testData/data/mad.rds")
    res1 <- addTAPD(data=dat,as.fun="data.table")
    res2 <- addTAPD(data=dat,col.tpdos="ATSPD",col.ndoses=NULL,as.fun="data.table")

    expect_equal( setdiff(colnames(res1),colnames(res2)),c("NDOSES", "TPDOS" ))
    expect_equal(setdiff(colnames(res2),colnames(res1)),c("ATSPD"))
    
    
})


test_that("SDOSE",{
    NMdataConf(as.fun="data.table")

    dat <- readRDS("testData/data/xgxr2.rds")
    res1 <- addTAPD(data=dat)
    res2 <- addTAPD(data=dat,SDOS=1000)

    res2[,PDOSAMT:=PDOSAMT*1000]
    res2[,DOSCUMA:=DOSCUMA*1000]
    expect_equal(res1,res2,version=2)
})


if(F){
    library(devtools)
    load_all("~/wdirs/NMexec")

    doses.1 <- NMcreateDoses(TIME=c(0,3,12,24),CMT=1)
    doses.2 <- NMcreateDoses(TIME=0,addl=list(ADDL=4,II=6),CMT=2)
    doses.all <- rbind(doses.1,doses.2,fill=TRUE)
    doses.all <- egdt(doses.all[,!("ID")],data.table(ID=1:2))
    doses.all <- addEVID2(doses.all,time.sim=seq(0,26,by=2),CMT=3)

    addTAPD(doses.all,subset.dos="CMT==1",cols.suffix="1")

}
