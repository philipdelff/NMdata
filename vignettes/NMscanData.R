## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=TRUE,include=FALSE--------------------------------------------------
library(devtools)
load_all("C:/Users/delff/working_copies/NMdata")

## ----setup,include=F----------------------------------------------------------
library(NMdata)
library(ggplot2)

theme_set(theme_bw()+theme(legend.position="bottom"))


## ----eval=TRUE----------------------------------------------------------------
res0 <- NMscanData(NMdata_filepath("examples/nonmem/xgxr001.lst"))
class(res0)

## ----eval=TRUE----------------------------------------------------------------
res1 <- NMscanData(NMdata_filepath("examples/nonmem/xgxr001.lst"),col.row="ROW")
all.equal(res0,res1)

## ----eval=TRUE----------------------------------------------------------------
## trtact is a character. Make it a factor with levels ordered by numericaldose level.
res1[,trtact:=reorder(trtact,DOSE)]
## Derive another data.table with geometric mean pop predictions by treatment and nominal sample time. Only use sample records.
res1.mean <- res1[EVID==0,.(gmPRED=exp(mean(log(PRED)))),by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop predictions. Split by treatment.
ggplot(res1[EVID==0])+
    geom_point(aes(TIME,DV))+
    geom_line(aes(NOMTIME,gmPRED),data=res1.mean,colour="red")+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## -----------------------------------------------------------------------------
res1[,Cmax:=max(IPRED),by=.(ID)]
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

## -----------------------------------------------------------------------------
res2 <- NMscanData(NMdata_filepath("examples/nonmem/xgxr014.lst"),col.row="ROW",recover.rows=TRUE)
## Derive another data.table with geometric mean pop predictions by treatment and nominal sample time. Only use sample records.
res2.mean <- res2[EVID==0&nmout==TRUE,
                  .(gmPRED=exp(mean(log(PRED)))),
                  by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop predictions. Split by treatment.
ggplot(res2[EVID==0])+
    geom_point(aes(TIME,DV,colour=flag))+
    geom_line(aes(NOMTIME,gmPRED),data=res2.mean)+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## -----------------------------------------------------------------------------
## this is just a long-format representation of
## with(res1,table(nmout,flag)) using data.table.
res1[,.N,by=.(nmout,flag)]

## -----------------------------------------------------------------------------
## notice fill is an option to rbind with data.table
res1.m <- NMscanData(NMdata_filepath("examples/nonmem/xgxr001.lst"),col.row="ROW")
res2.m <- NMscanData(NMdata_filepath("examples/nonmem/xgxr014.lst"),col.row="ROW",name="single-compartment")
res.mult <- rbind(res1.m,res2.m,fill=T)
res.mult.mean <- res.mult[EVID==0&nmout==TRUE,.(gmPRED=exp(mean(log(PRED)))),by=.(model,trtact,NOMTIME)]

ggplot(res.mult.mean,aes(NOMTIME,gmPRED,colour=model))+
    geom_line()+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")

## ----eval=FALSE---------------------------------------------------------------
#  out2in <- function(file) file.path(dirname(file),"input.txt")
#  res <- NMscanData("path/to/output.txt",file.mod=out2in)

