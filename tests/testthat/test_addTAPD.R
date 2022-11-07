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
### this example shows most key features. Reduce number of rows and build examples and tests on it.
    
## library(devtools)
    ## load_all("~/wdirs/NMexec")
    ## NMdataConf(as.fun="data.table")
    
    doses.1 <- NMcreateDoses(TIME=c(0,3,12,24),CMT=1,AMT=400e3)
    doses.2 <- NMcreateDoses(TIME=0,addl=list(ADDL=4,II=6),CMT=2,AMT=20)
    doses.all <- rbind(doses.1,doses.2,fill=TRUE)
    doses.all <- egdt(doses.all[,!("ID")],data.table(ID=1:2))
    dat.all <- addEVID2(doses.all,time.sim=seq(0,26,by=2),CMT=3)
    dat.all <- NMorderColumns(dat.all)
    dat.all[,ROW:=.I]
    dat.all2 <- addTAPD(dat.all,subset.dos = "CMT==1",suffix.cols="1")

    compareCols(dat.all,dat.all2)
    dat.all2

    dat.all2
    ## problem
#### II and ADDL are edited. addTAPD should not use the row from expansion but only merge back columns from them? addTAPD should return the original ADDL and II values.
    dat.all3 <- addTAPD(dat.all2,subset.dos = "CMT==2",suffix.cols="2")

    dat.all3

    dat.all3 <- addTAPD(dat.all2,subset.dos = "CMT==2",order.evid=c(4,1,0,2),suffix.cols="2b")
    dat.all3

}


### double check NMexpandDoses. Is the first one retrned marked nmexpanded? That would be a bug.
