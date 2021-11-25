#### Section start: Initialization ####

## install.packages("xgxr")
library(xgxr)
library(data.table)
library(ggplot2)

### pmxtricks is only used for individual plotting, so it's not necessary. 
## we need to ensure a specific version of pmxtricks is used. 0.0.7 is a candidate
library(remotes)
## install_github("philipdelff/pmxtricks")
library(pmxtricks)

## library(remotes)
## install_github("philipdelff/NMdata")
## library(NMdata)

library(devtools)
## load_all("c:/Users/delff/working_copies/NMdata")
load_all("c:/Users/Philip Delff/wdirs/NMdata")

NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}
file.data <- function(...)file.path(NMdata_filepath(),"examples/data",...)
file.nm <- function(...)file.path(NMdata_filepath(),"examples/nonmem",...)


script.1 <- "dsCreate_1.R"
writeOutput <- TRUE

###  Section end: Initialization


pkpd <- as.data.table(case1_pkpd)
## we should save case1_pkpd somewhere so we don't get in trouble when xgxr change it.

pk <- pkpd[CMT %in% 1:2] 

pk <- pk[CYCLE==1]

pk <- pk[,!c("IPRED")]
pk[,trtact := reorder(TRTACT, DOSE)]

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
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing",tab.flags=dt.flags)

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
pk[,ROW:=.I]
pk <- NMorderColumns(pk)
## colnames(pk)[24] <- "ret.4"
## a dup col name
## colnames(pk)[22] <- "NAME"

dim(pk)

## pmxtricks:::NMcheckData(pk[FLAG==0])
load_all("c:/Users/Philip Delff/wdirs/NMdata")

NMcheckData(pk,col.row="ROW",debug=F)

#### Section start: Write data to files ####

dt.data <- fread(text="file.data,description
xgxr1.csv,1 csv only
xgxr1_flag0.csv,pk FLAG==0
xgxr2.csv,1 +rds +meta
xgxr2_flag0.csv,2 FLAG==0
xgxr3.csv,duplic column names
xgxr4.csv,1 without ROW
")


dt.data[file.data=="xgxr1.csv",
        nmCode:=list(list(
            NMwriteData(pk,file=file.data(file.data),write.csv=writeOutput,write.rds=F)
        ))]
dt.data[file.data=="xgxr1_flag0.csv",
        nmCode:=list(list(
            NMwriteData(pk[FLAG==0],file=file.data(file.data),write.csv=writeOutput,write.rds=F)
        ))]


dt.data[file.data=="xgxr2.csv",
        nmCode:=list(list(
            NMwriteData(pk,file=file.data(file.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2),script=script.1)
        ))]
dt.data[file.data=="xgxr2_flag0.csv",
        nmCode:=list(list(
            NMwriteData(pk[FLAG==0],file=file.data(file.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2),script=script.1)
        ))]


## a version with duplicated column names for testing
pk2 <- cbind(pk,pk[,.(DOSE)])
dt.data[file.data=="xgxr3.csv",
        nmCode:=list(list(
            NMwriteData(pk2,file=file.data(file.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2))
        ))]

## without ROW
dt.data[file.data=="xgxr4.csv",
        nmCode:=list(list(
            NMwriteData(pk[,!("ROW")],file=file.data(file.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2))
        ))]


###  Section end: Write data to files

#### Section start: Update $INPUT sections ####



#### Update $INPUT sections, depending on data set
## dt.runs <- data.table(path=list.files(file.nm(),pattern="\\.mod$",full.names=T))
dt.runs <- data.table(path=list.files(file.nm(),pattern="\\.mod$|^input.txt$",full.names=T,recursive = T))
dt.runs[,mod:=basename(path)]
stopifnot(dt.runs[,uniqueN(mod)==.N])
dt.runs[,ROW:=.I]
dt.runs[,path.data:={NMextractDataFile(path)$string},by=ROW]
dt.runs[,file.data:=basename(path.data)]

dt.runs <- mergeCheck(dt.runs,dt.data,by="file.data")

##### DO THE UPDATE
dt.runs[mod!="input.txt",{
    sec=nmCode[[1]]["INPUT"]
    NMwriteSection(file=path,list.sections=sec,backup=FALSE)
},
by=ROW
]

## the old lst
## dt.runs[,paste(NMreadSection(fnExtension(path,".lst"),section="INPUT"),collapse="\n"),by=ROW]


### xgxgr002: CYCLE=DROP, BBW for WEIGHTB
dt.runs[mod=="xgxr002.mod",{
    nmcode=NMwriteData(pk,file="xgxr2.csv",nm.drop="CYCLE",nm.rename=c(BBW="WEIGHTB"),write.csv=FALSE)
    NMwriteSection(file=path,list.sections=nmcode["INPUT"],
                   backup=FALSE,write=TRUE)
},
by=ROW
]

###### xgxr021 is quite special wuth a number of features. Please take
###### from old lst for now.

###  Section end: Update $INPUT sections

#### Section start: Create xgxr001dir ####

### copy 001 to 001dir list.files(file.nm())
list.files(file.nm("xgxr001dir"))

unlink(file.nm("xgxr001dir"),recursive=T)
dir.create(file.nm("xgxr001dir"))
file.copy(
    file.nm("xgxr001.mod")
   ,
    file.nm("xgxr001dir/input.txt")
)
## unlink("xgxr001dir/output.txt")
file.copy(file.nm("xgxr001.lst"),file.nm("xgxr001dir/output.txt"))
files.data <- list.files(file.nm(),pattern="^xgxr001_.*",full.names=T)
file.copy(files.data,file.nm("xgxr001dir"))
list.files(file.nm("xgxr001dir"))

## update path to data
sec.data <- NMreadSection(file.nm("xgxr001dir/input.txt"),section="DATA")
data.old <- NMextractDataFile(file.nm("xgxr001dir/input.txt"))
sec.data.new <- sub(data.old$string,paste0("../",data.old$string),sec.data)
NMwriteSection(file.nm("xgxr001dir/input.txt"),section="DATA",newlines=sec.data.new)


###  Section end: Create xgxr001dir

