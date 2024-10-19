context("NMorderColumns")

test_that("last NULL",{

    fileRef <- "testReference/NMorderColumns_01.rds"

    ## pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- readRDS(file="testData/data/xgxr2.rds")|> setDT()

    pk1 <- NMorderColumns(pk)
    pk2 <- NMorderColumns(pk,last=c("FLAG",NULL))
    pk3 <- NMorderColumns(pk,last=c(NULL))


    expect_equal(colnames(pk1),colnames(pk3))

    dtres <- data.table(colnames(pk1),colnames(pk2),colnames(pk3))

    expect_equal_to_reference(
        dtres
       ,fileRef)

})

test_that("Non-numeric DATE and TIME",{

    fileRef <- "testReference/NMorderColumns_02.rds"

    pk <- readRDS(file="testData/data/xgxr2.rds")
    setDT(pk)

    pk[,time.tz:=as.POSIXct("2000/01/01",tz="UTC")+TIME*3600]
## pk[,DATE:=as.character(as.Date(time.tz),format="%y/%m/%d")]
 ##   pk[,TIME:=as.character(time.tz,format="%H:%M:%S")]

    pk[,DATE:=format(as.Date(time.tz),format="%y/%m/%d")]
    pk[,TIME:=format(time.tz,format="%H:%M:%S")]
    

    pk1 <- NMorderColumns(pk)
    
    dtres <- data.table(colnames(pk),colnames(pk1))

    expect_equal_to_reference(
        dtres
       ,fileRef)
readRDS(fileRef)
})
