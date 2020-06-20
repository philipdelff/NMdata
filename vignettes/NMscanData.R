## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(NMdata)

## ----eval=TRUE-----------------------------------------------------------
res1 <- NMscanData(NMdata_filepath("examples/nonmem/xgxr002.lst"))
head(res1)

## ----eval=FALSE----------------------------------------------------------
#  res1 <- NMscanData(NMdata_filepath("examples/nonmem/run001.lst"))
#  names(res1)
#  lapply(res1,dim)

