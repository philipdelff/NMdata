## library(devtools)
## load_all("c:/Users/delff/working_copies/NMdata")

context("NMwriteData")

test_that("basic",{

    pk <- readRDS(file=file.path(NMdata_filepath(),"examples/data/xgxr2.rds"))

    fileRef <- "testReference/NMwriteData_1.rds"

    expect_equal_to_reference(
        NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                    write.rds=F,write.csv=F)
       ,fileRef)


    ## not allowed
    expect_error(NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                             write.rds=F,write.csv=F,
                             nmdrop="")
                 )

})

test_that("Dropping a column in Nonmem",{

    fileRef <- "testReference/NMwriteData_2.rds"
    pk <- readRDS(file=file.path(NMdata_filepath(),"examples/data/xgxr2.rds"))
    expect_equal_to_reference(
        NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                    write.rds=F,write.csv=F,
                    nmdrop="PART")
       ,fileRef)

    ## dropping a character column
    pk[,CYCLE:=as.character(CYCLE)]
    fileRef <- "testReference/NMwriteData_3.rds"

    expect_equal_to_reference(
        NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                    write.rds=F,write.csv=F,
                    nmdrop="CYCLE"),
        file=fileRef
    )

})

test_that("A comma in a character",{

    ## dropping a character column
    pk[,CYCLE:=paste0(as.character(CYCLE),",0")]

    fileRef <- "testReference/NMwriteData_3.rds"

    expect_error(
        NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                    write.rds=F,write.csv=F,
                    nmdrop="CYCLE")
    )

})


test_that("Identical column names",{

    pk <- readRDS(file=file.path(NMdata_filepath(),"examples/data/xgxr2.rds"))
    pk <- cbind(pk[,.(CYCLE)],pk)
    expect_warning(NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),
                write.rds=F,write.csv=F
                ))

})
