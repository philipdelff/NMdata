## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
## library(devtools)
## load_all("~/working_copies/NMdata")
library(NMdata)

library(ggplot2)

## ----eval=TRUE-----------------------------------------------------------
res1 <- NMscanData(NMdata_filepath("examples/nonmem/xgxr001.lst"))
class(res1)

## ----eval=TRUE-----------------------------------------------------------
## trtact is a character. Make it a factor with levels ordered by numericaldose level.
res1[,trtact:=reorder(trtact,DOSE)]
## Derive another data.table with geometric mean pop predictions by treatment and nominal sample time. Only use sample records.
res1.mean <- res1[EVID==0,.(gmPRED=exp(mean(log(PRED)))),by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop predictions. Split by treatment.
ggplot(res1[EVID==0])+
geom_point(aes(TIME,DV))+
## stat_summary(aes(x=NOMTIME,y=PRED),fun.y=function(x)exp(mean(log(x))),geom="line")+
geom_line(aes(NOMTIME,gmPRED),data=res1.mean)+
scale_y_log10()+
facet_wrap(~trtact)+
labs(x="Hours since administration",y="Concentration (ng/mL)")


## ------------------------------------------------------------------------
res1[,Cmax:=max(PRED),by=.(ID)]
res1.id <- findCovs(res1,cols.id="ID")
dim(res1.id)
ggplot(res1.id,aes(WEIGHTB,Cmax/DOSE,colour=trtact))+
geom_point()

## ------------------------------------------------------------------------
## we have no occasion variability in this data
## res1.id.occ <- findCovs(res1,cols.id=c("ID","OCC"))

## ------------------------------------------------------------------------
findCovs(res1)

## ------------------------------------------------------------------------
dim(res1.id)
head(res1.id,2)

## ------------------------------------------------------------------------
res1.id2 <- findVars(res1.id,cols.id="model")
dim(res1.id2)
head(res1.id2,2)

