## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=TRUE,include=FALSE--------------------------------------------------
## library(devtools)
## load_all("C:/Users/delff/working_copies/NMdata")

## ----setup,include=F----------------------------------------------------------
library(NMdata)
## library(ggplot2)
library(data.table)

## theme_set(theme_bw()+theme(legend.position="bottom"))


## -----------------------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
class(pk)

## -----------------------------------------------------------------------------
dim(pk)
class(pk)
dt.cov <- pk[,.(ID=unique(ID)[1:10])]
dt.cov[,COV:=sample(1:5,size=10,replace=TRUE)]
dt.cov
pk2 <- merge(pk,dt.cov,by="ID")
dim(pk2)
## now as expected
pk3 <- merge(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)

## -----------------------------------------------------------------------------
dt.cov2 <- pk[,.(ID=unique(ID))]
dt.cov2[,COV:=sample(1:5,size=.N,replace=TRUE)]
dt.cov2 <- dt.cov2[c(1,1:(.N-1))]
# dt.cov2 <- dt.cov2[c(1:(.N))]
pk4 <- merge(pk,dt.cov2,by="ID")
dim(pk4)

## -----------------------------------------------------------------------------
pk2 <- try(mergeCheck(pk,dt.cov,by="ID"))
## now as expected
pk3 <- mergeCheck(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)

## -----------------------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds", package="NMdata"))
dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,BLQ==1")

pk <- flagsAssign(pk,dt.flags)

## -----------------------------------------------------------------------------
tab.count <- flagsCount(data=pk,tab.flags=dt.flags)
print(tab.count)

## -----------------------------------------------------------------------------
pk <- NMorderColumns(pk)

## -----------------------------------------------------------------------------
NMwriteData(pk)

## -----------------------------------------------------------------------------
pk <- stampObj(pk,script="vignettes/DataCreate.Rmd")
objInfo(pk)

## -----------------------------------------------------------------------------
pk <- stampObj(pk,script="vignettes/DataCreate.Rmd",Description="A PK dataset used for examples.")
objInfo(pk)

