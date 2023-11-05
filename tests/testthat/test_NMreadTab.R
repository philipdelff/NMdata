## NMreadTab("testData/nonmem/estim_debug.cov")
context("NMreadTab")

### test we get TABLE.NO and NMREP right

test_that("Table with repetitions",{
    fileRef <- "testReference/NMreadTab_01.rds"
    
    res <- NMreadTab("testData/simulations/xgxr014_subprobs/NMsim_xgxr014_subprobs.tab")

    expect_equal_to_reference(res,fileRef,version=2)
})


if(F){
    ## NOHEADER
    res <- NMreadTab("testData/nonmem/xgxr033_res_a.txt")
    ## ONEHEADER
    res <- NMreadTab("testData/nonmem/xgxr033_res_c.txt")
    ## NOLABEL
    res <-
        NMreadTab("testData/nonmem/xgxr033_res_d.txt")

    res <- NMscanTables("testData/nonmem/xgxr033.lst")
    lapply(res,head)

    res <- NMscanTables("testData/nonmem/xgxr033.lst",col.tableno=TRUE)
    lapply(res,head)
}
