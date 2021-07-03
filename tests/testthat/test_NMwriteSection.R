context("NMwriteSection")

test_that("basic",{

    fileRef <- "testReference/NMwriteSection_1.rds"
    
    outfile <- "testOutput/xgxr011_update1.mod"
    newlines <- "$INPUT ROW ID TIME EVID CMT AMT DV FLAG STUDY EFF0"
    section <- "input"
    NMwriteSection(system.file("examples/nonmem/xgxr011.mod", package = "NMdata")
                  ,section=section
                  ,newlines=newlines
                  ,newfile=outfile)
    res <- readLines(outfile)
    expect_equal_to_reference(res,fileRef,version=2)

})




## same, with list approach
test_that("list.section",{

    fileRef <- "testReference/NMwriteSection_2.rds"

    outfile <- "testOutput/xgxr011_update2.mod"
    newlines <- "$INPUT ROW ID TIME EVID CMT AMT DV FLAG STUDY EFF0"
    section <- "input"
    NMwriteSection( system.file("examples/nonmem/xgxr011.mod", package = "NMdata")
                  ,list.section=list(input=newlines)
                  ,newfile=outfile
                   )
    res <- readLines(outfile)
    expect_equal_to_reference(res,fileRef,version=2)

})


if(F){
    text["INPUT"]
    ## with output from NMwriteData
    NMwriteSection( system.file("examples/nonmem/xgxr011.mod", package = "NMdata"),
                   list.section=text["INPUT"],newpath=NULL
                   )
}
