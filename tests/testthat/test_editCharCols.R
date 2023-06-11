library(data.table)

context("editCharCols")

test_that("basic",{

    fileRef <- "testReference/editCharCols_01.rds"
    
    dat <- data.table(A=1:3,text=cc(a,"a,d","g"))

    res1 <- editCharCols(dat,pattern=",","")

    expect_equal_to_reference(res1,fileRef)
})


test_that("with factors",{

    dat <- data.table(A=1:3,text=cc(a,"a,d",g),fac=cl("a","a,d","g"))

    expect_error(editCharCols(dat,pattern=",",""))

})
