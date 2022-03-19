
context("mergeCheck")

test_that("basic",{

    fileRef <- "testReference/mergeCheck1.rds"

    df1 <- data.frame(x = 1:10,
                      y=letters[1:10],
                      stringsAsFactors=FALSE)
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11,
                      stringsAsFactors=FALSE)

    mc1 <- mergeCheck(df1,df2,by="y")
    
    expect_equal_to_reference(mc1,fileRef,version=2)

})


test_that("increasing number of rows",{

    ##     fileRef <- "testReference/mergeCheck2.rds"
    
    df1 <- data.frame(x = 1:10,
                      y=letters[1:10],
                      stringsAsFactors=FALSE)
    df2 <- data.frame(y=c("a",letters[1:11]),
                      x2 = c(44,1:11),
                      stringsAsFactors=FALSE)

    expect_error(
        expect_warning(mergeCheck(df1,df2,by="y"))
    )
    
})

test_that("a dt and a df",{
    ## library(data.table)
    
    fileRef <- "testReference/mergeCheck3.rds"

    dt1 <- data.table(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11,
                      stringsAsFactors=FALSE)

### notice, this is a left join
    res3 <- mergeCheck(dt1,df2,by="y")
    
    expect_equal_to_reference(res3,fileRef,version=2)
    
})

test_that("a df and a dt",{
    ## library(data.table)
    
    fileRef <- "testReference/mergeCheck4.rds"

    dt1 <- data.table(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11,
                      stringsAsFactors=FALSE)


    res4 <- mergeCheck(df2,dt1,by="y",all.x=T)
    
    expect_equal_to_reference(res4,fileRef,version=2)
    
})

test_that("duplicate column name",{

    dt1 <- data.table(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11,
                      x=21:31,
                      stringsAsFactors=FALSE)

    expect_warning(
        mergeCheck(dt1,df2,by="y",all.x=T)
    )
    
})


test_that("handling of common cols",{
    
    fileRef <- "testReference/mergeCheck6.rds"

    dt1 <- data.table(a=1:3,b=3:5,c=letters[8:10])
    dt2 <- data.table(a=1:3,b=3:5,q=letters[8:10])
    expect_warning(mergeCheck(dt1,dt2,by="a"))

    ## dtres=mergeCheck(dt1,dt2,by="a",fun.commoncols = message)
    dtres <- mergeCheck(dt1,dt2,by="a",fun.commoncols = function(x)NULL)
    expect_equal_to_reference(dtres,fileRef,version=2)

})

test_that("specifying expected number of new columns",{
    fileRef <- "testReference/mergeCheck7.rds"

    dt1 <- data.table(a=1:3,b=3:5,c=letters[8:10])
    dt2 <- data.table(a=1:3,b=3:5,q=letters[8:10])
    dt3 <- dt2[,!c("b")]

    ## compareCols(dt1,dt3,diff.only=FALSE)
    dtres <- mergeCheck(dt1,dt3,by="a",ncols.expect = 1)

    expect_equal_to_reference(dtres,fileRef,version=2)

})


test_that("Zero-row df1 must give an error",{

    dt1 <- data.table(a=1:3,b=3:5,c=letters[8:10])[0]
    dt2 <- data.table(a=1:3,b=3:5,q=letters[8:10])
    dt3 <- dt2[,!c("b")]

    ## compareCols(dt1,dt3,diff.only=FALSE)
    expect_error(
        mergeCheck(dt1,dt3,by="a")
    )

})

test_that("Duplicating input rows",{
    dt1 <- data.table(a=1:3,b=3:5,c=letters[8:10])
    dt2 <- data.table(a=c(1:3,3),b=c(3:6),q=letters[8:11])
    dt3 <- dt2[,!c("b")]

    ## compareCols(dt1,dt3,diff.only=FALSE)
    expect_error(
        mergeCheck(dt1,dt3,by="a",ncols.expect = 1)
    )

})

test_that("deprecated df1 and df2",{
    dt1 <- data.table(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11,
                      stringsAsFactors=FALSE)

    ## compareCols(dt1,dt3,diff.only=FALSE)
    expect_message(
        mergeCheck(df1=dt1,df2=df2,by="y",ncols.expect = 1)
    )


})

test_that("missing values in by",{
    dt1 <- data.table(x = 1:10,
                      y=c(letters[1:9],NA))
    dt2 <- data.table(y=letters[1:11],
                      x2 = 1:11)

    ## compareCols(dt1,dt3,diff.only=FALSE)
    ##
    ## mergeCheck(dt1,dt2,by="y",ncols.expect = 1)
    ## mergeCheck(dt1,dt2,by="y",ncols.expect = 1,all.x=T)
    dt2[3,y:=NA]
    expect_error(
        mergeCheck(dt1,dt2,by="y",ncols.expect = 1)
    )
    expect_error(
        mergeCheck(dt2,dt1,by="y",ncols.expect = 1)
    )

})
