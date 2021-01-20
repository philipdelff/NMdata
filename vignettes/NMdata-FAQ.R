## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun=tibble::as_tibble)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun="data.table")

## ----eval=F-------------------------------------------------------------------
#  ## for tibbles
#  NMdataConfig(as.fun=tibble::as_tibble)
#  ## for data.table
#  NMdataConfig(as.fun="data.table")

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConfig(file.mod=function(file) file.path(dirname(file),"input.txt"))

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConfig(modelname=function(file) basename(dirname(normalizePath(file))))

