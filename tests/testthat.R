library(testthat)
library(NMdata)

expect_equal_to_reference = function(x,y,...) {
  # wrapper to handle data.table v1.14.2 whose as.data.frame() now removes indices; news item 27
  # this will pass data.table before and after 1.14.2, and without needing to rewrite all the
  # RDS reference files which contain the index on the data.frame
  
  if (is.list(x) && is.data.frame(x$input.colnames)) {
    y = readRDS(y)
    attr(x$input.colnames, "index") = NULL
    attr(y$input.colnames, "index") = NULL
    testthat::expect_equal(x,y,...)
  } else if (inherits(x,"NMdata")) {
    y = readRDS(y)
    attr(attr(x,"NMdata")$input.colnames, "index") = NULL
    attr(attr(y,"NMdata")$input.colnames, "index") = NULL
    attr(attr(x,"NMdata")$tables, "index") = NULL
    attr(attr(y,"NMdata")$tables, "index") = NULL
    testthat::expect_equal(x,y,...)
  } else if (is.data.frame(x) && !inherits(x,"data.table")) {
    y = readRDS(y)
    attr(x, "index") = NULL
    attr(y, "index") = NULL
    testthat::expect_equal(x,y,...)
  } else {
    testthat::expect_equal_to_reference(x,y,...)
  }
}

test_check("NMdata")
