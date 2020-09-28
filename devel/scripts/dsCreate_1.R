## install.packages("xgxr")
library(xgxr)
library(data.table)
library(ggplot2)

### what is pmxtricks used for?
## we need to ensure a specific version of pmxtricks is used. 0.0.7 is a candidate

library(pmxtricks)

## library(remotes)
## install_github("philipdelff/NMdata")
library(NMdata)

## library(devtools)
## load_all("c:/Users/delff/working_copies/NMdata")

pkpd <- as.data.table(case1_pkpd)

pk <- pkpd[CMT %in% 1:2] 

pk <- pk[CYCLE==1]

pk <- pk[,!c("IPRED")]
pk[,trtact := factor(TRTACT, levels = unique(TRTACT))]


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
    

## rename
setnames(pk,
         old=c("LIDV","CENS"),
         new=c("DV","BLQ")
         )


pk
pk[,table(CMT,EVID)]

pk[ID==1]
indprofs <- ggIndProfs(pk,amt="AMT")
## ggwrite(indprofs,file="indprofs.pdf",onefile=T)

### handle LLOQ and set FLAGS
dt.flags <- data.table(
    FLAG=10,
    flag="Below LLOQ",
    condition=c("BLQ==1"))

pk <- flagsAssign(pk,dt.flags)
tab.count <- flagsCount(pk,dt.flags)
tab.count

pk <- pk[order(ID,TIME,CMT)]
pk <- pk[DOSE>0]
pk[,ROW:=1:nrow(pk)]
NMorderColumns(pk,by.ref=TRUE)
colnames(pk)
dim(pk)

NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr1.csv"),write.rds=F)

## same, but with rds
NMwriteData(pk,file=file.path(NMdata_filepath(),"examples/data/xgxr2.csv"),write.rds=T,args.rds=list(version=2))


## need to updata nonmem models
### $INPUT is independent of xgxr1 or xgxr2
### need to be modified. Some examples (xgxr002 and more?) have modified $INPUT for DROP and renaming.
## lapply(
##     list.files(
##         NMdata_filepath("examples/nonmem")
##        ,pattern=".*\\.mod$"
##        ,full.names=TRUE
##     ),
##     NMreplacePart,section="INPUT",
##     newlines="$INPUT ROW ID NOMTIME TIME EVID CMT AMT DV BLQ CYCLE DOSE FLAG PART PROFDAY PROFTIME STUDY WEIGHTB"
## )
