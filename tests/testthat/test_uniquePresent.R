context("uniquePresent")

test_that("basic",{

    dat0 <- readRDS("testData/data/xgxr2.rds")

    dat <- copy(dat0)
    dat[c(3,30,50,55,50),WEIGHTB:=NA]
    dat
    dat[,WEIGHTB:=uniquePresent(WEIGHTB),by=.(ID)]

    expect_equal(dat0,dat)

})

