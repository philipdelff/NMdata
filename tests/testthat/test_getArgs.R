context("getArgs")

test_that("basic",{

    ## fileRef <- "testReference/getArgs_01.rds"
    
    fun1 <- function(a=1,b=2,c){
        ## browser()            
        getArgs()
    }

    
    res1 <- fun1(a=1,b=2)
    expect_equal(res1,list(a=1,b=2))

    expect_equal(
        fun1(b=2),
        list(b=2))


    expect_equal(
        fun1(a=1),
        list(a=1))

    ## argument value passed in a variable in global env
    bb=3
    expect_equal(
        fun1(b=bb),
        list(b=3))

    ## And without naming the arg
    expect_equal(
        fun1(bb),
        list(a=3))


#### ... is used
    fun1.bar <- function(a,...){
        fun1(a,...)
    }

    expect_equal(
        fun1.bar(a=1,b=2)
      , list(a=1,b=2))
})


if(F){

    fun2 <- function(a=1,b=2,c){
        getArgs2()
    }

    fun2(a=1,b=2)

    fun2(b=2)

    fun2(b=bb)

    fun2(bb)
    fun2.bar <- function(a,...){
        fun2(a,...)
    }

    fun2.bar(a=1,b=2)

}
