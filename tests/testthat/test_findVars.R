context("findVars")

test_that("basic",{

    fileRef <- "testReference/findVars1.rds"
    
    dt1 <- data.table(a=1,b=1:2)
    dt2 <- data.table(b=1:2)

    v1 <- findVars(dt1)
    v2 <- findVars(dt2)

    expect_equal_to_reference(v1,fileRef,version=2)
    expect_equal(v1,v2)

})


test_that("basic2",{

    fileRef <- "testReference/findVars2.rds"
    
    dt1 <- data.table(a=1,b=c(1,2,2),c=c(3,4,4),d=c(4,4,3),e=4:6)
    dt1
    r1 <- findVars(dt1)
    ## not a. OK
    ##    r1
    r2 <- findVars(dt1,by=c("b"))
    ## not a and c. OK
    ##    r2

    expect_equal_to_reference(list(r1,r2),fileRef,version=2)

})



## 
test_that("deprecated cols.id",{
    expect_warning(
        findVars(data.frame(x=rep(1:2,each=2),y=c(rep(c("a"),2),"v","w")),cols.id="x")
    )
    expect_error(
        findVars(data.frame(x=rep(1:2,each=2),y=c(rep(c("a"),2),"v","w")),cols.id="x",by="x")
    )
})
