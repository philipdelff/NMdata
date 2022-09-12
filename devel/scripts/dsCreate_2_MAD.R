library(devtools)
load_all("~/wdirs/pmxtricks")
load_all("~/wdirs/NMdata")
library(xgxr)
library(data.table)
data(package="xgxr")
data(mad_missing_duplicates)
mad <- mad_missing_duplicates
setDT(mad)

mad[EVID==1,.N,by=.(ID)]

mad.pk <- mad[NAME%in%c("PK Concentration")]
mad.dos <- mad[NAME=="Dosing",lapply(.SD,first),by=.(ID)]
mad.dos[,`:=`(ADDL=5,II=24)]

mad.all <- rbind(mad.dos,mad.pk,fill=T)
setorder(mad.all,ID,TIME,-EVID)

mad.all
mad.all[,uniqueN(ID),by=.(DOSE)]
mad.all <- mad.all[DOSE==1600]

setnames(mad.all,"LIDV","DV")

ggIndProfs(mad.all,amt="AMT")

mad.all.exp <- NMexpandDoses(mad.all)

ggIndProfs(mad.all.exp,amt="AMT")

mad.all <- NMorderColumns(mad.all)

NMwriteData(mad.all,file="~/wdirs/NMdata/inst/examples/data/mad.rds",write.csv=FALSE,write.rds=TRUE,args.rds=list(version=2))
file.copy("~/wdirs/NMdata/inst/examples/data/mad.rds","~/wdirs/NMdata/tests/testthat/testData/data",overwrite=T)
