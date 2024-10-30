#### Section start: Initialization ####

## install.packages("xgxr")
library(xgxr)
library(data.table)
library(ggplot2)

### pmxtricks is only used for individual plotting, so it's not necessary. 
## we need to ensure a specific version of pmxtricks is used. 0.0.7 is a candidate
## library(remotes)
## install_github("philipdelff/pmxtricks")
## library(pmxtricks)

## library(remotes)
## install_github("philipdelff/NMdata")
## library(NMdata)

setwd("~/wdirs/NMdata/devel/scripts/")
library(devtools)
load_all("~/wdirs/NMdata")

##### Please change working dir to the location of this script

NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}

### saving data to testDir first. Then copying to inst
file.data.test <- function(...)file.path("../../tests/testthat/testData/data",...)
file.data.inst <- function(...)file.path(NMdata_filepath(),"examples/data",...)
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

if(F){
    indprofs <- ggIndProfs(pk,amt="AMT")
    ## ggwrite(indprofs,file="indprofs.pdf",onefile=T)
}

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


pk <- setorder(pk,ID,TIME,CMT)
pk <- pk[DOSE>0]
pk[,ROW:=.I]
pk <- NMorderColumns(pk)
## colnames(pk)[24] <- "ret.4"
## a dup col name
## colnames(pk)[22] <- "NAME"
## pk[2,ret.4:="3,mg"]

dim(pk)

finds <- NMcheckData(pk)
finds <- NMcheckData(pk,col.flagn="FLAG")

#### Section start: Write data to files ####

dt.data <- fread(text="file.data,description
xgxr1.csv,1 csv only
xgxr1_flag0.csv,pk FLAG==0
xgxr2.csv,1 +rds +meta
xgxr2_flag0.csv,2 FLAG==0
xgxr2_flag0_nocolnames.csv,2 FLAG==0 no colnames in csv
xgxr3.csv,duplic column names
xgxr4.csv,1 without ROW
xgxr5.csv,2 char and .
")

## dir.testdata <- file.data.test()##"testData/data/"
fn.data <- "xgxr1.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk,file=file.data.test(fn.data),write.csv=writeOutput,write.rds=F)
        ))]

files <- list.files(path=file.data.test(),pattern="xgxr1.+",full.names=TRUE)
for(fn in files){
    file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
}

fn.data <- "xgxr1_flag0.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk[FLAG==0],file=file.data.test(fn.data),write.csv=writeOutput,write.rds=F)
        ))]
files <- list.files(path=file.data(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
for(fn in files){
    cat(fn,"\n")
    file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
}

### xgxr2 has rds and meta data
fn.data <- "xgxr2.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk,file=file.data.test(fn.data),formats=cc(csv,rds,fst),save=writeOutput,args.rds=list(version=2),script=script.1)
        ))]
files <- list.files(path=file.data.test(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
for(fn in files){
    file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
}

fn.data <- "xgxr2_flag0.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk[FLAG==0],file=file.data.test(fn.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2),script=script.1)
        ))]
files <- list.files(path=file.data.test(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
for(fn in files){
    file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
}

fn.data <- "xgxr2_flag0_nocolnames.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk[FLAG==0],file=file.data.test(fn.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2),script=script.1,args.fwrite=list(na=".",quote=FALSE,row.names=FALSE,scipen=0,col.names=FALSE))
        ))]


## a version with duplicated column names for testing
pk2 <- cbind(pk,pk[,.(DOSE)])
fn.data <- "xgxr3.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk2,file=file.data.test(fn.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2))
        ))]
###### not used in inst examples - only for testing
## files <- list.files(path=file.data(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
## for(fn in files){
##     file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
## }


## without ROW
fn.data <- "xgxr4.csv"
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk[,!("ROW")],file=file.data.test(fn.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2))
        ))]

files <- list.files(path=file.data.test(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
for(fn in files){
    file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
}

##### everything encoded as characters. NA's as .
fn.data <- "xgxr5.csv"
pk5 <- copy(pk)
sapply(pk5,class)
cols.not.char <- colnames(pk5)[!sapply(pk5,is.character)]
pk5[,(cols.not.char):=lapply(.SD,as.character),.SDcols=cols.not.char]
all.cols <- colnames(pk5)
pk5[,(all.cols):=lapply(.SD,function(x){x[is.na(x)] <- ".";x})]
dt.data[file.data==fn.data,
        nmCode:=list(list(
            NMwriteData(pk5,file=file.data.test(fn.data),write.csv=writeOutput,write.rds=writeOutput,args.rds=list(version=2))
        ))]

##### only for tests
## files <- list.files(path=file.data.test(),pattern=paste0(fnExtension(fn.data,ext=""),".+"))
## for(fn in files){
##     cat(fn,"\n")
##     file.copy(file.data.test(fn),file.data.inst(),overwrite=T)
## }



###  Section end: Write data to files

#### Section start: Update $INPUT sections ####


######## this part should be updated to use the recent functionality in NMwriteSection #######
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

#### Section start: Version with AMT in microgram ####

### xgxgr002: CYCLE=DROP, BBW for WEIGHTB
pk2 <- copy(pk)
pk2[,AMT:=AMT*1000]
nmcode <- NMwriteData(pk2,
                      file=file.data.test("xgxr12.csv"),
                      script=script.1,
                      args.stamp = list(description="AMT in micrograms"),
                      args.rds=list(version=2))


### Section end: Version with AMT in microgram

