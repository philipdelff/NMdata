test_that("All tests",{
    fileRef <- "testReference/cc_1.rds"

    res <- list(
        cc(a,b,`a b`)
       ,cc(a,b,"a b")
        ## be careful with spaces and special characters
       ,cc( d)
       ,cc(" d")
       ,cc()
    )

    expect_equal_to_reference(res,fileRef,version=2)
})
