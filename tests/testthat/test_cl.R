context("cl")

test_that("All tests",{
    fileRef <- "testReference/cl_1.rds"

    x1 <- c("b","a")
    x2 <- c("a","b")

    res <- list(
        cl("b","a")
       ,cl(x1)
       ,cl(x2)
    )

    expect_equal_to_reference(res,fileRef)
})
