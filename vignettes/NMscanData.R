## ---- include = FALSE-------------------------------------
knitr::opts_chunk$set(
                      collapse = TRUE
                     ,comment = "#>"
                     ,fig.width=7
                     ,cache=FALSE
                  )

## this change data.table syntax. I think we can do without.
## knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----setup,include=T--------------------------------------
library(NMdata)
## not necessary for NMdata to run, but we use thse in the examples
library(data.table)
library(ggplot2)
theme_set(theme_bw()+theme(legend.position="bottom"))

## ----eval=TRUE,include=FALSE------------------------------
library(devtools)
load_all()

## ----file-shortcut,include=T------------------------------
file.NMdata <- function(...) system.file(file.path("examples/nonmem",...), package="NMdata")

## ----setup2,include=F-------------------------------------
NMdataConf(check.time=FALSE)

## ----eval=TRUE--------------------------------------------
res0 <- NMscanData(file.NMdata("xgxr017.lst"))

## ---------------------------------------------------------
class(res0)
dim(res0)

## ---------------------------------------------------------
head(res0,n=2)

## ----eval=TRUE--------------------------------------------
res0 <- NMscanData(file.NMdata("xgxr003.lst"),as.fun=tibble::as_tibble)

## ---------------------------------------------------------
class(res0)

## ---------------------------------------------------------
NMdataConf(as.fun="data.table")

## ---------------------------------------------------------
res3 <- NMscanData(file.NMdata("xgxr003.lst"),quiet=TRUE)

## ---------------------------------------------------------
class(res3)

## ----eval=TRUE--------------------------------------------
## trtact is a character. Make it a factor with levels ordered by
## numerical dose level. The := is a data.table assignment within
## res3. In dplyr, you could use mutate.
res3[,trtact:=reorder(trtact,DOSE)]
## Only use sample records. In dplyr, use filter
res3.obs <- res3[EVID==0]
## Derive geometric mean pop predictions by treatment and nominal
## sample time. In dplyr, use group_by and summarize.
res3.mean <- res3.obs[,.(gmPRED=exp(mean(log(PRED)))),
                     by=.(trtact,NOMTIME)]
## plot individual observations and geometric mean pop
## predictions. Split (facet) by treatment.  
ggplot(res3.obs)+
    geom_point(aes(TIME,DV))+
    geom_line(aes(NOMTIME,gmPRED),data=res3.mean,colour="red")+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## ----meanbydose-------------------------------------------
res2 <- NMscanData(file.NMdata("xgxr014.lst"),recover.rows=TRUE)
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

## ---------------------------------------------------------
## this is just a long-format representation of
## with(res1,table(nmout,flag)) using data.table.
res2[,.N,by=.(nmout,flag)]

## ---------------------------------------------------------
NMdataConf(as.fun="data.table")
NMdataConf(col.row="ROW")
NMdataConf(merge.by.row=TRUE)

## ---------------------------------------------------------
## notice fill is an option to rbind with data.table
res1.m <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                     quiet=TRUE)
res2.m <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                     modelname="single-compartment",
                     quiet=TRUE)
res.mult <- rbind(res1.m,res2.m,fill=T)
res.mult.mean <- res.mult[EVID==0&nmout==TRUE,
                          .(gmPRED=exp(mean(log(PRED)))),
                          by=.(model,trtact,NOMTIME)]

ggplot(res.mult.mean,aes(NOMTIME,gmPRED,colour=model))+
    geom_line()+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")

## ----eval=FALSE-------------------------------------------
#  out2in <- function(file) file.path(dirname(file),"input.txt")
#  res <- NMscanData("path/to/output.txt",file.mod=out2in)

## ----eval=FALSE-------------------------------------------
#  NMdataConf(file.mod=out2in)

## ---------------------------------------------------------
print(attributes(res1.m)$meta$variables,topn=3)

