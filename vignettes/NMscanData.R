## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

## this change data.table syntax. I think we can do without.
## knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=TRUE,include=FALSE--------------------------------------------------
## library(devtools)
## load_all("C:/Users/delff/working_copies/NMdata")

## ----setup,include=F----------------------------------------------------------
library(NMdata)
library(data.table)
library(ggplot2)

theme_set(theme_bw()+theme(legend.position="bottom"))


## ----eval=TRUE----------------------------------------------------------------
res0 <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                   cbind.by.filters=TRUE)
class(res0)

## ----eval=TRUE----------------------------------------------------------------
res1 <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),col.row="ROW",quiet=TRUE)
res0 <- res0[,c(colnames(res1),setdiff(colnames(res0),colnames(res1)))]
all.equal(res0,res1,check.attributes=FALSE)

## ----eval=TRUE----------------------------------------------------------------
## trtact is a character. Make it a factor with levels ordered by
## numerical dose level.
res1$trtact <- reorder(res1$trtact,res1$DOSE)
## We are going to use data.table
res1.dt <- as.data.table(res1)
## Derive another data.table with geometric mean pop predictions by
## treatment and nominal sample time. Only use sample records.
res1.mean <- res1.dt[EVID==0,.(gmPRED=exp(mean(log(PRED)))),
                     by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop
## predictions. Split by treatment.
ggplot(subset(res1,EVID==0))+
    geom_point(aes(TIME,DV))+
    geom_line(aes(NOMTIME,gmPRED),data=res1.mean,colour="red")+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## -----------------------------------------------------------------------------
## with data.table, create a new column representing ID-level Cmax
res1.dt[,Cmax:=max(IPRED),by=.(ID)]
## findCovs picks the columns that do not vary within cols.id. One row
## per value of cols.id.
res1.id <- findCovs(res1.dt,cols.id="ID")
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
options(NMdata.as.fun="none")

## -----------------------------------------------------------------------------
res2 <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                   col.row="ROW",recover.rows=TRUE)
## now we have a data.table
class(res2)
## Derive another data.table with geometric mean pop predictions by
## treatment and nominal sample time. Only use sample records.
res2.mean <- res2[EVID==0&nmout==TRUE,
                  .(gmPRED=exp(mean(log(PRED)))),
                  by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop
## predictions. Split by treatment.
ggplot(res2[EVID==0])+
    geom_point(aes(TIME,DV,colour=flag))+
    geom_line(aes(NOMTIME,gmPRED),data=res2.mean)+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## -----------------------------------------------------------------------------
## this is just a long-format representation of
## with(res1,table(nmout,flag)) using data.table.
res2[,.N,by=.(nmout,flag)]

## -----------------------------------------------------------------------------
## notice fill is an option to rbind with data.table
res1.m <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                     col.row="ROW",
                     quiet=TRUE)
res2.m <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                     col.row="ROW",modelname="single-compartment",
                     quiet=TRUE)
res.mult <- rbind(res1.m,res2.m,fill=T)
res.mult.mean <- res.mult[EVID==0&nmout==TRUE,
                          .(gmPRED=exp(mean(log(PRED)))),
                          by=.(model,trtact,NOMTIME)]

ggplot(res.mult.mean,aes(NOMTIME,gmPRED,colour=model))+
    geom_line()+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")

## ----eval=FALSE---------------------------------------------------------------
#  out2in <- function(file) file.path(dirname(file),"input.txt")
#  res <- NMscanData("path/to/output.txt",file.mod=out2in)

## -----------------------------------------------------------------------------
print(attributes(res1.m)$meta$variables,topn=3)

