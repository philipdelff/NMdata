
context("dt2mat")

test_that("basic",{
    fileRef <- "testReference/dt2mat_1.rds"

    ext <- NMreadExt("testData/nonmem/xgxr021.mod",as.fun="data.table")

    omegas <- ext[par.type=="OMEGA"]
    
    res <- dt2mat(omegas)

    omegas[i==j,.(i,j,value)]
    omegas[i!=j,.(i,j,value)]
    res
    
    expect_equal_to_reference(res,fileRef)

})

