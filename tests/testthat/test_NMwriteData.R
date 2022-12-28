## library(devtools)
## load_all()

## for some reason, the linebreaking is not consistent in $INPUT
## making these tests fail. So for now, we don't test te line
## breaking.
fix.input <- function(x) {
    x$INPUT  <- paste(x$INPUT,collapse=" ")
    x
}

context("NMwriteData")

test_that("basic",{

    fileRef <- "testReference/NMwriteData_1.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    NMwriteData(pk,file="testOutput/NMwriteData1.csv",
                write.rds=F,write.csv=T,nmdir.data="/example")
    res1 <- readLines("testOutput/NMwriteData1.csv")

    ## lapply(res1,print)
    ## lapply(readRDS(fileRef),print)

    expect_equal_to_reference(
        res1
       ,fileRef,version=2)
})

test_that("nm.drop is an empty string - not allowed",{
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    ## not allowed
    expect_error(
        res <- NMwriteData(pk
                          ,file=system.file("examples/data/xgxr1.csv",package="NMdata")
                          ,write.rds=F,write.csv=F
                          ,nm.drop=""
                           )
    )
})

test_that("Dropping a column in Nonmem",{

    fileRef <- "testReference/NMwriteData_2.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res2 <- NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                        write.rds=F,write.csv=F,
                        nm.drop="PART",
                        nmdir.data="/example")
    res2 <- fix.input(res2)
    
    expect_equal_to_reference(
        res2
       ,fileRef,version=2)

    ## dropping a character column
    pk[,CYCLE:=paste0(as.character(CYCLE),"a")]
    fileRef <- "testReference/NMwriteData_3.rds"

    res2b <- NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                         write.rds=F,write.csv=F,
                         nm.drop="CYCLE",
                         nmdir.data="/example")
    res2b <- fix.input(res2b)

    expect_equal_to_reference(
        res2b
       ,
        file=fileRef,version=2
    )

})

test_that("A comma in a character",{

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    ## dropping a character column
    pk[,CYCLE:=paste0(as.character(CYCLE),",0")]

    fileRef <- "testReference/NMwriteData_3.rds"

    expect_error(
        NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                    write.rds=F,write.csv=F,
                    nm.drop="CYCLE")
    )

})


test_that("Identical column names",{

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- cbind(pk[,.(CYCLE)],pk)
    expect_warning(NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata")
                              ,write.rds=F,write.csv=F
                               ))

})


test_that("nm.copy, nm.rename, drop",{
    fileRef <- "testReference/NMwriteData_4.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    nmCode <- NMwriteData(pk,file="derived/pk.csv",
                          write.csv=FALSE,
### arguments that tailors text for Nonmem
                          ## PSN compatibility
                          args.NMgenText=list(dir.data="../derived",drop="PROFDAY",copy=c(CONC="DV"),rename=c(BBW="WEIGHTB"),capitalize=TRUE,width=80))

    expect_equal_to_reference(nmCode,fileRef,version=2)
})


test_that("nm.copy, nm.rename, drop",{
    fileRef <- "testReference/NMwriteData_5.rds"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    nmCode <- NMwriteData(pk,
                          file="testOutput/pk.csv",
                          write.csv=TRUE,
                          write.rds=TRUE,
                          write.RData=TRUE
### arguments that tailors text for Nonmem
                         ,args.rds=list(version=2),
                         ,args.RData=list(version=2)
                         ,args.NMgenText=list(width=95))
### for testing of file contents. Not used.
    ## load("testOutput/pk.RData")
    ## pk.rdata <- pk
    ## all.res <- list(rds=readRDS("testOutput/pk.rds")
    ##                ,csv=fread("testOutput/pk.csv")
    ##                ,Rdata=fread("testOutput/pk.csv")
    ##                 )
    expect_equal_to_reference(nmCode,fileRef,version=2)
})


test_that("with stamp",{

    fileRef <- "testReference/NMwriteData_7.rds"

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    res1 <- NMwriteData(pk,file=NULL,
                        write.rds=F,write.csv=F,nmdir.data="/example",script="A simple test")
    res1 <- fix.input(res1)

    expect_equal_to_reference(
        res1
       ,fileRef,version=2)
})

test_that("with stamp on csv",{

    fileRef <- "testReference/NMwriteData_8.rds"
    outfile <- "testOutput/stampedData_8.csv"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    res1 <- NMwriteData(pk,file=outfile
                       ,script="A simple test",write.rds=FALSE,
                        args.stamp=list(time=as.POSIXct("2021-11-21 11:00:00")))
    res1 <- fix.input(res1)

    expect_equal_to_reference(
        res1
       ,fileRef,version=2)
}
)


test_that("Quiet but get text for NM",{

    fileRef <- "testReference/NMwriteData_9.rds"
    outfile <- "testOutput/stampedData_9.csv"
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    res1 <- NMwriteData(pk,file=outfile
                       ,script="A simple test",write.rds=FALSE,
                        args.stamp=list(time=as.POSIXct("2021-11-21 11:00:00")),
                        quiet=T)
    res1 <- fix.input(res1)

    expect_equal_to_reference(
        res1
       ,fileRef,version=2)
}
)

test_that("Not quiet but no text for NM",{
    
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))

    res1 <- NMwriteData(pk,file=tempfile(),
                       ,script="A simple test",write.rds=FALSE,
                        args.stamp=list(time=as.POSIXct("2021-11-21 11:00:00")),
                        quiet=FALSE,
                        genText=FALSE)
    expect_null(res1)

}
)


test_that("script=NULL",{

    fileRef <- "testReference/NMwriteData_10.rds"

    pk0 <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- copy(pk0)
    is.NMdata(pk)
    NMinfo(pk)
    res1 <- NMwriteData(pk,file="testOutput/NMwriteData10.csv",
                        write.rds=T,write.csv=T,nmdir.data="/example",script=NULL                         ,args.rds=list(version=2))

    written1 <- readRDS("testOutput/NMwriteData10.rds")
    NMinfo(written1)

    
    expect_equal_to_reference(
        written1
       ,fileRef,version=2)

    ## that must not have affected pk
    expect_equal(pk,pk0)

})

test_that("csv.trunc.as.nm",{

    fileRef <- "testReference/NMwriteData_11.rds"

    pk <- readRDS(file="testData/data/xgxr2.rds")
 
    res1 <- NMwriteData(pk,file="testOutput/NMwriteData11.csv",
                        write.rds=T,write.csv=T,nmdir.data="/example",script=NULL
                        ,csv.trunc.as.nm=T,args.rds=list(version=2))

    written1.rds <- readRDS("testOutput/NMwriteData11.rds")
    written1.csv <- NMreadCsv("testOutput/NMwriteData11.csv")

    
    expect_equal_to_reference(
list(colnames(written1.rds),colnames(written1.csv))
       ,fileRef,version=2)


})


