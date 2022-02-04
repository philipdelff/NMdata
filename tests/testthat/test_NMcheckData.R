context("mergeCheck")

## meta is if x is metadata rather than an NMdata object
fix.time <- function(x,meta=T){
    if(meta){
        meta.x <- attr(x,"NMdata")
    } else {
        meta.x <- x
    }

    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.NMscanData <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.mod <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$mtime.mod <- NULL
    meta.x$datafile$path <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL

    if(meta){
        setattr(x,"NMdata",meta.x)
    } else {
        x <- meta.x
    }
    x
}


test_that("basic",{
    fileRef <- "testReference/NMcheckData_1.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    res <- NMcheckData(pk)

   expect_equal_to_reference(res,fileRef,version=2)
})

test_that("No col.flagn",{
    fileRef <- "testReference/NMcheckData_2.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    res <- NMcheckData(pk,col.flagn=FALSE)
    expect_equal_to_reference(res,fileRef,version=2)
})

test_that("Misc findings",{
    fileRef <- "testReference/NMcheckData_3.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")

    colnames(pk)[24] <- "ret.4"
    ## a comma in a string - but with FLAG>0
    pk[2,ret.4:="3,mg"]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})

test_that("TIME with characters",{
    fileRef <- "testReference/NMcheckData_4.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")

    ## a character in TIME
    pk[,TIME:=as.character(TIME)]
    pk[ROW==204,TIME:=paste0(TIME,"p")]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})



test_that("Misc findings and dup colname",{
    fileRef <- "testReference/NMcheckData_5.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")

    colnames(pk)[24] <- "ret.4"
    ## a dup col name
    colnames(pk)[22] <- "NAME"
    pk[2,ret.4:="3,mg"]

    res <- expect_warning(NMcheckData(pk))
    expect_equal_to_reference(res,fileRef,version=2)
    
})

test_that("missing EVID",{
    fileRef <- "testReference/NMcheckData_6.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[,EVID:=NULL]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    
})


test_that("missing ID",{
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[,ID:=NULL]

    expect_error( NMcheckData(pk))
})

test_that("missing MDV",{
    fileRef <- "testReference/NMcheckData_7.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[,MDV:=1]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
    ##         expect_equal(as.data.table(res)[level=="row"],as.data.table(readRDS(fileRef))[level=="row"])
})


test_that("With ADDL, no II",{
    fileRef <- "testReference/NMcheckData_8.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[EVID==1,ADDL:=1]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("With II, no ADDL",{
    fileRef <- "testReference/NMcheckData_9.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[EVID==1,II:=24]

    res <- NMcheckData(pk)
    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("Using control stream file",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/NMcheckData_10.rds"

    file.lst <- "testData/nonmem/xgxr001.lst"
    
    res1a <- NMcheckData(file=file.lst)

    res1a <- fix.time(res1a,meta=F)
    expect_equal_to_reference(res1a,fileRef)

    ## res.ref <- readRDS(fileRef)
    ## res <- lapply(names(res.ref),function(name){
    ##     expect_equal(res.ref[[name]],res1a[[name]])
    ## })
    
    res1b <- NMcheckData(file=file.lst)
    res1b <- fix.time(res1b,meta=F)

    expect_equal(res1a,res1b)

})



test_that("ID and row with leading 0",{
    
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    fileRef <- "testReference/NMcheckData_11.rds"
    
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[,ID:=paste0("0",ID)]
    pk[,ROW:=paste0("0",ROW)]

####### we need to report original ID and ROW, not the numeric translations. 
    
    
    
    res <- NMcheckData(pk)
    ## res
    expect_equal_to_reference(res,fileRef,version=2)
})



######### Covariates
test_that("One covariate varying within ID",{
    
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    fileRef <- "testReference/NMcheckData_12.rds"
    pk <- readRDS(file="testData/data/xgxr2.rds")    

    pk[1500,WEIGHTB:=30]  
    res <- NMcheckData(pk,covs=c("trtact","WEIGHTB","CYCLE","DOSE"))
    ## res
    expect_equal_to_reference(res,fileRef,version=2)
})


test_that("with IOV",{
    NMdataConf(reset=T)
    ##    NMdataConf(as.fun=data.table)
    
    fileRef <- "testReference/NMcheckData_13.rds"
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[,FED:=0]
    pk[ID>170,`:=`(PERIOD=1)]
    pk.fed <- pk[ID>170][,`:=`(TIME=TIME+240,PERIOD=2,FED=1,DV=DV*.79)]
    ## an error
    pk.fed[ID==180&TIME>242&TIME<250,FED:=0]
    pk2 <- rbind(pk,
                 pk.fed)
    pk2[,ROW:=.I]

    res <- NMcheckData(pk2,covs.occ=list(PERIOD=c("FED")),cols.num="REE")
    expect_equal_to_reference(res,fileRef,version=2)

})

test_that("covariates within subsets",{
    NMdataConf(reset=T)
    ##    NMdataConf(as.fun=data.table)
    
    fileRef <- "testReference/NMcheckData_14.rds"
    pk <- readRDS(file="testData/data/xgxr2.rds")
    pk[EVID==0,LLOQ:=1]
    pk[EVID==1,site:="home"]
    pk[EVID==0,ASSAY:=3]
    pk[EVID==1,ASSAY:=c(rep(NA,3),1,rep(NA,.N-4))]

    ## it finds way too many for ASSAY. Should only find 1.
    res <- NMcheckData(pk,cols.num=list("EVID==0"=c("LLOQ","ASSAY"),"EVID==1"=c("site"),"WEIGHTB"))
    expect_equal_to_reference(res,fileRef,version=2)

})


##
