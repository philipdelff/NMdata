## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun=tibble::as_tibble)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun="none")

## ----eval=F-------------------------------------------------------------------
#  ## for tibbles
#  options(NMdata.as.fun=tibble::as_tibble)
#  ## for data.table
#  options(NMdata.as.fun="none")

## ----eval=FALSE---------------------------------------------------------------
#  options(NMdata.file.mod=function(file) file.path(dirname(file),"input.txt"))

## ----eval=FALSE---------------------------------------------------------------
#  options(NMdata.modelname=function(file) basename(dirname(normalizePath(file))))

