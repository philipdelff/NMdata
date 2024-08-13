context("deprecatedArg")

test_that("basic",{


    fun1 <- function(a=1,b=2){
        ## b is deprecated
        args <- getArgs(sys.call(),parent.frame())
        a <- deprecatedArg("b","a",args=args)
        a
    }

    expect_error(
        fun1(a=1,b=2)
    )
    expect_message(
        fun1(b=2)
    )

    expect_equal(fun1(a=1),1)

    bb=3
    fun1(b=bb)

    
})
