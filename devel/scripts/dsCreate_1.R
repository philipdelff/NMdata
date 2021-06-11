## install.packages("xgxr")
library(xgxr)
library(data.table)
library(ggplot2)

### pmxtricks is only used for individual plotting, so it's not necessary. 
## we need to ensure a specific version of pmxtricks is used. 0.0.7 is a candidate

library(pmxtricks)

## library(remotes)
## install_github("philipdelff/NMdata")
library(NMdata)

## library(devtools)
## load_all("c:/Users/delff/working_copies/NMdata")

pkpd <- as.data.table(case1_pkpd)
## we should save case1_pkpd somewhere so we don't get in trouble when xgxr change it.

pk <- pkpd[CMT %in% 1:2] 

pk <- pk[CYCLE==1]

pk <- pk[,!c("IPRED")]
pk[,trtact := factor(TRTACT, levels = sort(unique(TRTACT)))]

if(F){
    ggplot(data = pk, aes(x     = NOMTIME,
                          y     = LIDV,
                          group = DOSE
                         ,color = trtact)
           ) +
        xgx_geom_ci(conf_level = 0.95) +
        xgx_scale_y_log10() 


    ggplot(data = pk, aes(x     = TIME,
                          y     = LIDV,
                          group = ID
                         ,color = trtact)
           ) +
        geom_line()+geom_point()+
        xgx_scale_y_log10() +
        facet_wrap(~trtact)
}    

## rename
setnames(pk,
         old=c("LIDV","CENS"),
         new=c("DV","BLQ")
         )


pk[,table(CMT,EVID)]

## making up a predose sample just to use another exclusion flag
samples.predose <- pk[ID%in%c(100,127)&NOMTIME==0]
samples.predose[,`:=`(TIME=-1,DV=0.05,BLQ=1,EVID=0)]
pk <- rbind(pk,samples.predose)

indprofs <- ggIndProfs(pk,amt="AMT")
## ggwrite(indprofs,file="indprofs.pdf",onefile=T)

### handle LLOQ and set FLAGS

dt.flags <- fread(text="FLAG,flag,condition
    10,Below LLOQ,BLQ==1
100,Pre-dose sample,TIME<0")

pk[EVID==1,FLAG:=0]
pk[EVID==1,flag:="Dosing"]
load_all("c:/Users/delff/working_copies/NMdata")
### OK!
pk <- flagsAssign(pk,subset.data="EVID==0",tab.flags=dt.flags)
tab.count <- flagsCount(pk,dt.flags,by="EVID")
tab.count

pk[,any(is.na(FLAG)),by=.(EVID)]


if(F){
    ## checking increasing flags
### correct (but not what we want for this dataset) - predose are covered by LLOQ flag
    pk2 <- flagsAssign(pk,dt.flags,flags.increasing=T)
    tab.count2 <- flagsCount(pk2,dt.flags,flags.increasing=T,by="EVID")
    tab.count2
### forgetting EVID
    tab.count3 <- flagsCount(pk,dt.flags)

}


pk <- pk[order(ID,TIME,CMT)]
pk <- pk[DOSE>0]
pk[,ROW:=1:nrow(pk)]
pk <- NMorderColumns(pk)
colnames(pk)
dim(pk)

text <- NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),write.rds=F)
### with this one, we don't need to filter on FLAG
NMwriteData(pk[FLAG==0],file=file.path(NMdata_filepath(),"examples/data/xgxr1_flag0.csv"),write.rds=F)

## same, but with rds
text2 <- NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr2.csv"),write.rds=T,args.rds=list(version=2))
NMwriteData(pk[FLAG==0],file=file.path(NMdata_filepath(),"examples/data/xgxr2_flag0.csv"),write.rds=T,args.rds=list(version=2))

## a version with duplicated column names for testing
pk2 <- cbind(pk,pk[,.(DOSE)])
text3 <- NMwriteData(pk2,file=file.path(NMdata_filepath(),"examples/data/xgxr3.csv"),write.rds=T,args.rds=list(version=2))

## and a version without rds, and without ROW
text <- NMwriteData(pk[,!("ROW")],file=file.path(NMdata_filepath(),"examples/data/xgxr4.csv"),write.rds=F)

dt.files <- data.table(path=list.files(file.path(NMdata_filepath(),"examples/nonmem"),pattern="\\.mod$",full.names=T))
dt.files[,mod:=basename(path)]
dt.files[,ROW:=1:.N]
dt.files[,DATA:=paste(NMgetSection(path,section="DATA"),collapse=" "),by=.(ROW)]
dt.files[,file:={strings=strsplit(DATA," ")
    list(strings[[1]][grepl("\\.csv",strings[[1]])])
},by=.(ROW)]
dt.files

## lapply(dt.files[file=="../data/xgxr1.csv",path],
##        NMreplacePart,list.sections=text["INPUT"])

## lapply(dt.files[file=="../data/xgxr1.csv",path],
##        NMundoReplace)

## lapply(dt.files[file=="../data/xgxr2.csv",path],
##        NMreplacePart,list.sections=text2["INPUT"])

## lapply(dt.files[file=="../data/xgxr3.csv",path],
##        NMreplacePart,list.sections=text3["INPUT"])

