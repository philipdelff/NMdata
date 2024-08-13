## install.packages("NMcalc",repos="https://cloud.r-project.org")
library(NMdata)
library(NMcalc)
library(ggplot2)
library(data.table)
load_all("~/wdirs/NMdata")
NMdataConf(as.fun="data.table")

writeOutput <- TRUE

x2 <- readRDS("~/wdirs/NMdata/inst/examples/data/xgxr2.rds")


## res <- NMscanData("/data/home/philipde/wdirs/NMsim/inst/examples/nonmem/xgxr032.lst")
### using a very basic model that was run on all subjects to get EBEs for all subjects
res <- NMscanData("~/wdirs/NMsim/inst/examples/nonmem/xgxr014.lst")
pars.id <- findCovs(res,by="ID")
colnames(pars.id)

ggplot(pars.id,aes(WEIGHTB,CL))+
    geom_point()
ggplot(pars.id,aes(WEIGHTB,V))+
    geom_point()

ggplot(pars.id,aes(CL,V))+
    geom_point()

### adding  SEX - higher CL for men
head(pars.id)
pars.id[,prob.m:=invlogit((CL-mean(CL))/sd(CL))]
set.seed(33)
pars.id[,MALEN:=sample(c(0,1),size=.N,prob=c(1-prob.m,prob.m),replace=T),by=ID]

ggplot(pars.id,aes(factor(MALEN),CL))+
    geom_boxplot()


#### higher age -> lower CL
## CL=CL0*(AGE/AGE0)**th => AGE=(CL/CL0)^(1/th)*AGE0


pars.id[,AGE0:=55]
pars.id[,CL0:=median(CL)]
set.seed(330)
pars.id[,AGE:=(CL/CL0)^(1/2)*AGE0+rnorm(n=.N,0,sd=15)]

ggplot(pars.id,aes(AGE,CL))+
    geom_point()


x3 <- mergeCheck(x2,pars.id[,.(ID,MALEN,AGE)],by=cc(ID))

x3 <- NMorderColumns(x3)
head(x3)
NMwriteData(x3,file="~/wdirs/NMdata/inst/examples/data/xgxr2covs.rds",save=writeOutput,
            script="NMdata/devel/scripts/dsCreate_4_simCovs.R",args.rds=list(version=2))


melt(pars.id,measure.vars=cc(WEIGHTB,AGE,MALEN)) [,.(mean(value),median(value)),by="variable"]
