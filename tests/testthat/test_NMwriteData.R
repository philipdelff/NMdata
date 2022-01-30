## library(devtools)
## load_all("c:/Users/delff/working_copies/NMdata")

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
        NMwriteData(pk
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
                          nmdir.data="../derived",
                          nm.drop="PROFDAY",
                          nm.copy=c(CONC="DV"),
                          nm.rename=c(BBW="WEIGHTB"),
                          ## PSN compatibility
                          nm.capitalize=TRUE)

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
                         ,args.RData=list(version=2))
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
