context("dims")

test_that("basic",{

    fileRef <- "testReference/dims_01.rds"

    a1 <- data.table(a=1:3,b=letters[1:3])
    a2 <- a1[,.(a)][,c:=5:7]

    res <- dims(a1,a2)

    expect_equal_to_reference(res,fileRef)

    ## dims(list.data=list(a1=a1,a2=a2))    

    ## dims(a1=a1,a2=a2,keepNames=F)
})
    
