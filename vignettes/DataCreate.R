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
## NMdataConf(as.fun="data.table")
library(data.table)
library(ggplot2)
theme_set(theme_bw()+theme(legend.position="bottom"))


## -----------------------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
class(pk)

## ----include=FALSE------------------------------------------------------------
pk.reduced <- copy(pk)
pk.reduced <- pk.reduced[1:(.N%/%2)]
pk.reduced[,CYCLE:=NULL]
pk.reduced[,AMT:=as.character(AMT)]

## ----eval=FALSE---------------------------------------------------------------
#  compareCols(pk,pk.reduced)

## ----include=FALSE------------------------------------------------------------
pktmp <- copy(pk)
pktmp[,TRTACT:=NULL]

## -----------------------------------------------------------------------------
pk.renamed <- renameByContents(data=pktmp,fun.test=NMisNumeric,fun.rename = tolower, invert.test = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  compareCols(pktmp,pk.renamed)

## ----include=FALSE------------------------------------------------------------
dt.cov <- pk[,.(ID=unique(ID)[1:10])]
dt.cov[,COV:=sample(1:5,size=10,replace=TRUE)]

dt.cov2 <- pk[,.(ID=unique(ID))]
dt.cov2[,COV:=sample(1:5,size=.N,replace=TRUE)]
dt.cov2 <- dt.cov2[c(1,1:(.N-1))]

## -----------------------------------------------------------------------------
dim(pk)
## the number of unique values of ID in pk
pk[,uniqueN(ID)]
pk[,range(ID)]
## dt.cov has a covariate for some of the subjects
dt.cov
pk2 <- merge(pk,dt.cov,by="ID")
dim(pk2)
## now as expected
pk3 <- merge(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)

## -----------------------------------------------------------------------------
pk4 <- merge(pk,dt.cov2,by="ID")
dim(pk4)
## we now have twice as many rows for this subject
pk[ID==31,.N]
pk4[ID==31,.N]

## -----------------------------------------------------------------------------
pk2 <- try(mergeCheck(pk,dt.cov,by="ID"))
## now as expected
pk3 <- mergeCheck(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)

## ----include=F----------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds", package="NMdata"))
pk[,`:=`(FLAG=NULL,flag=NULL)]

## -----------------------------------------------------------------------------
dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,EVID==0&BLQ==1
100,Negative time,EVID==0&TIME<0")

pk <- flagsAssign(pk,tab.flags=dt.flags,subset.data="EVID==0")
pk[EVID==1,FLAG:=0]
pk[EVID==1,flag:="Dosing"]

## -----------------------------------------------------------------------------
tab.count <- flagsCount(data=pk[EVID==0],tab.flags=dt.flags)
print(tab.count)

## -----------------------------------------------------------------------------
pk <- NMorderColumns(pk)

## -----------------------------------------------------------------------------
NMwriteData(pk)

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection("run001.mod","INPUT","$INPUT ROW ID TIME EVID CMT AMT DV FLAG STUDY BLQ CYCLE DOSE FLAG2 NOMTIME PART PROFDAY PROFTIME WEIGHTB eff0")

## -----------------------------------------------------------------------------
text.nm <- NMwriteData(pk)

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection("run001.mod",list.sections=text.nm["INPUT"])

## -----------------------------------------------------------------------------
pk <- stampObj(pk,script="vignettes/DataCreate.Rmd")
objInfo(pk)

## -----------------------------------------------------------------------------
pk <- stampObj(pk,script="vignettes/DataCreate.Rmd",Description="A PK dataset used for examples.")
objInfo(pk)

## -----------------------------------------------------------------------------
res1 <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                   col.row="ROW",merge.by.row=TRUE,quiet=TRUE,as.fun="data.table")
res1$trtact <- reorder(res1$trtact,res1$DOSE)
## with data.table, create a new column representing ID-level Cmax
res1[,Cmax:=max(IPRED),by=.(ID)]
## findCovs picks the columns that do not vary within cols.id. One row
## per value of cols.id.
res1.id <- findCovs(res1,cols.id="ID")
dim(res1.id)
ggplot(res1.id,aes(WEIGHTB,Cmax/DOSE,colour=trtact))+
    geom_point()+
    labs(x="Bodyweight at baseline (kg)")

## -----------------------------------------------------------------------------
## we have no occasion variability in this data
## res1.id.occ <- findCovs(res1,cols.id=c("ID","OCC"))

## -----------------------------------------------------------------------------
findCovs(res1)

## -----------------------------------------------------------------------------
dim(res1.id)
head(res1.id,2)

## -----------------------------------------------------------------------------
res1.id2 <- findVars(res1.id)
dim(res1.id2)
head(res1.id2,2)

