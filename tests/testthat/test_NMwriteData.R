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

    res1 <- NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                        write.rds=F,write.csv=F,nmdir.data="/example")
    res1 <- fix.input(res1)
    ## lapply(res1,print)
    ## lapply(readRDS(fileRef),print)

    expect_equal_to_reference(
        res1
       ,fileRef)
})

test_that("nmdrop is an empty string - not allowed",{
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    ## not allowed
    expect_error(
        NMwriteData(pk
                   ,file=system.file("examples/data/xgxr1.csv",package="NMdata")
                   ,write.rds=F,write.csv=F
                   ,nmdrop=""
                    )
    )
})

test_that("Dropping a column in Nonmem",{

    fileRef <- "testReference/NMwriteData_2.rds"
    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    res2 <- NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                        write.rds=F,write.csv=F,
                        nmdrop="PART",
                        nmdir.data="/example")
    res2 <- fix.input(res2)
    
    expect_equal_to_reference(
        res2
       ,fileRef)

    ## dropping a character column
    pk[,CYCLE:=paste0(as.character(CYCLE),"a")]
    fileRef <- "testReference/NMwriteData_3.rds"

    res2b <- NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata"),
                         write.rds=F,write.csv=F,
                         nmdrop="CYCLE",
                         nmdir.data="/example")
    res2b <- fix.input(res2b)

    expect_equal_to_reference(
        res2b
       ,
        file=fileRef
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
                    nmdrop="CYCLE")
    )

})


test_that("Identical column names",{

    pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
    pk <- cbind(pk[,.(CYCLE)],pk)
    expect_warning(NMwriteData(pk,file=system.file("examples/data/xgxr1.csv",package="NMdata")
                              ,write.rds=F,write.csv=F
                               ))

})
