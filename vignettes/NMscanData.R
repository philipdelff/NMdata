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

## ----eval=FALSE,include=FALSE-----------------------------
#  library(devtools)
#  load_all()

## ----file-shortcut,include=T------------------------------
file.NMdata <- function(...) system.file(file.path("examples/nonmem",...), package="NMdata")

## ----setup2,include=F-------------------------------------
NMdataConf(check.time=FALSE)

## ----eval=TRUE--------------------------------------------
res1 <- NMscanData(file.NMdata("xgxr018.lst"))

## ---------------------------------------------------------
class(res1)
dim(res1)

## ---------------------------------------------------------
head(res1,n=2)

## ----eval=TRUE--------------------------------------------
res1.tbl <- NMscanData(file.NMdata("xgxr003.lst"),as.fun=tibble::as_tibble)

## ---------------------------------------------------------
class(res1.tbl)

## ---------------------------------------------------------
NMdataConf(as.fun="data.table")

## ---------------------------------------------------------
res1.dt <- NMscanData(file.NMdata("xgxr003.lst"),quiet=TRUE)

## ---------------------------------------------------------
class(res1.dt)

## ---------------------------------------------------------
print(NMinfo(res1,info="columns"),nrows=20,topn=10)

## ----eval=TRUE--------------------------------------------
## trtact is a character. Make it a factor with levels ordered by
## numerical dose level. The := is a data.table assignment within
## res3. In dplyr, you could use mutate.
res1.dt[,trtact:=reorder(trtact,DOSE)]
## Derive geometric mean pop predictions by treatment and nominal
## sample time. In dplyr, use group_by, summarize, and ifelse?
res1.dt[EVID==0,gmPRED:=exp(mean(log(PRED))),
     by=.(trtact,NOMTIME)]

## ----plotres1.dt,echo=FALSE-------------------------------
## plot individual observations and geometric mean pop
## predictions. Split (facet) by treatment.  
ggplot(res1.dt[EVID==0])+
    geom_point(aes(TIME,DV))+
    geom_line(aes(NOMTIME,gmPRED),colour="red")+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",y="Concentration (ng/mL)")

## ----res2-------------------------------------------------
res2 <- NMscanData(file.NMdata("xgxr014.lst"),recover.rows=TRUE)

## ---------------------------------------------------------
res2[,.N,by=nmout]

## ----meanbydose-------------------------------------------
## add geometric mean pop predictions by treatment and nominal sample
## time. Only use sample records.
res2[EVID==0&nmout==TRUE,
     gmPRED:=exp(mean(log(PRED))),
     by=.(trtact,NOMTIME)]

## ----plot-res2,echo=FALSE---------------------------------
## plot individual observations and geometric mean pop
## predictions. Split by treatment.
ggplot(res2[EVID==0])+
    geom_point(aes(TIME,DV,colour=flag))+
    geom_line(aes(NOMTIME,gmPRED),data=function(x)x[EVID==0&nmout==TRUE])+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",
         y="Concentration (ng/mL)",
         subtitle="data: res2. Lines are gmPRED at output data observations.")

## ---------------------------------------------------------
NMdataConf(as.fun="data.table", ## already set above, repeated for completeness
           col.row="ROW",       ## This is default, included for completeness
           merge.by.row=TRUE    ## Require input and output data to be combined by merge
           )

## ---------------------------------------------------------
res1.m <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                     quiet=TRUE)
## using a custom modelname for this model
res2.m <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                     modelname="One compartment",
                     quiet=TRUE)
## notice fill is an option to rbind with data.table (like bind_rows in dplyr)
res.mult <- rbind(res1.m,res2.m,fill=T)
## Notice, the NMdata class disappeared
class(res.mult)
res.mult[EVID==0&nmout==TRUE,
         gmPRED:=exp(mean(log(PRED))),
         by=.(model,trtact,NOMTIME)]

## ----plot-resmult,echo=FALSE------------------------------
ggplot(res.mult,aes(NOMTIME,gmPRED,colour=model))+
    geom_line(data=function(x)x[EVID==0&nmout==TRUE])+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration",
         y="Concentration (ng/mL)",
         subtitle="data: res.mult. Lines are gmPRED at output observation times.")

## ---------------------------------------------------------
namefun <- function(path) sub("^[[:alpha:]0]+","",fnExtension(basename(path),""))
NMdataConf(modelname=namefun)
res1.m <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                     quiet=TRUE)
res2.m <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                     quiet=TRUE)
## notice fill is an option to rbind with data.table (like bind_rows in dplyr)
res.mult <- rbind(res1.m,res2.m,fill=T)
res.mult[,.N,by=model]
## resetting default
NMdataConf(modelname=NULL)

## ---------------------------------------------------------
res2[,class(trtact)]
res2[,levels(trtact)]

## ----eval=FALSE-------------------------------------------
#  out2in <- function(file) file.path(dirname(file),"input.txt")
#  res <- NMscanData("path/to/output.txt",file.mod=out2in)

## ----eval=FALSE-------------------------------------------
#  NMdataConf(file.mod=out2in)

