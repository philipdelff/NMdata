##' Create an overview of number of retained and discarded datapoints.
##'
##' Generate an overview of number of observations disregarded due to
##' different reasons. And how many are left after each exclusion
##' flag.
##' 
##' @param data The dataset including both FLAG and flag columns.
##' @param tab.flags A data.frame containing at least these named columns: FLAG,
##'     flag, condition. Condition is disregarded for FLAG==0.
##' @param file A file to write the table of flag counts to. Will probably be
##'     removed and put in a separate function.
##' @param col.id The name of the subject ID column. Default is "ID".@param
##'     col.id The name of the subject ID column. Default is "ID".
##' @param by An optional column to group the counting by. This could be
##'     "STUDY", "DRUG", "EVID", or a combination of multiple columns.
##' @param as.fun The default is to return a data.table if input data is a
##'     data.table, and return a data.frame for all other input classes. Pass a
##'     function in as.fun to convert to something else. If data is not a
##'     data.table, default can be configured using NMdataConf.
##' @return A summary table with number of discarded and retained subjects and
##'     observations when applying each condition in the flag table. "discarded"
##'     means that the reduction of number of observations and subjects
##'     resulting from the flag, "retained" means the numbers that are left
##'     after application of the flag. The default is "both" which will report
##'     both.
##' @details Notice number of subjects in N.discarded mode can be
##'     misunderstood. If two is reported, it can mean that the remining one
##'     observation of these two subjects are discarded due to this flag. The
##'     majority of the samples can have been discarded by earlier flags.
##' @import data.table
##' @family DataCreate
##' @examples
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' dt.flags <- data.frame(
##'   flagn=10,
##'   flagc="Below LLOQ",
##' condition=c("BLQ==1"))
##' pk <- flagsAssign(pk,dt.flags,col.nflag="flagn",col.cflag="flagc")
##' unique(pk[,c("flagn","flagc","flagn")])
##' flagsCount(pk,dt.flags)
##' @export


flagsCount <- function(data,tab.flags,file,col.id="ID",
                       col.nflag= "FLAG", col.cflag="flag",
                       by=NULL,as.fun=NULL){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    FLAG <- NULL
    ID <- NULL
    N.discarded <- NULL
    N.left <- NULL
    Nobs.discarded <- NULL
    Nobs.left <- NULL
    flag <- NULL
    notAll <- NULL
    isFinal <- NULL
    . <- function() NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(file)) file <- NULL
    
    stopifnot(is.data.frame(data))
    stopifnot(is.data.frame(tab.flags))

    data.was.data.table <- TRUE
    if(is.data.table(data)) {
        data <- copy(data)
    } else {
        data <- as.data.table(data)
        ##  data.was.data.frame <- TRUE
        data.was.data.table <- FALSE
    }
    tab.flags.was.data.table <- TRUE
    if(is.data.table(tab.flags)) {
        tab.flags <- copy(tab.flags)
    } else {
        tab.flags <- as.data.table(tab.flags)
        ##  tab.flags.was.data.frame <- TRUE
        tab.flags.was.data.table <- FALSE
    }

### check that tab.flags contains col.nflag and col.cflag and that data contains col.nflag
    cnames.data <- copy(colnames(data))
    if(!col.nflag%in%cnames.data) messageWrap("data must contain a column with same name as value of col.nflag",fun.msg="stop")

    cnames.tab.flags <- copy(colnames(tab.flags))
    if(!col.nflag%in%cnames.tab.flags) messageWrap("tab.flags must contain a column with same name as value of col.nflag",fun.msg="stop")
    if(!col.cflag%in%cnames.tab.flags) messageWrap("tab.flags must contain a column with same name as value of col.cflag",fun.msg="stop")

###  rename columns to FLAG and flag
    tab.flags[,FLAG:=get(col.nflag)]
    tab.flags[,flag:=get(col.cflag)]
    data[,FLAG:=get(col.nflag)]


### if 0 and Inf are not in tab.flags, insert them
    if(!0%in%tab.flags[,FLAG]){
        tab.flags <- rbind(tab.flags,
                           data.table(FLAG=0,flag="Analysis set")
                          ,fill=TRUE
                           )
    }


    tab.flags.0 <- tab.flags[FLAG==0]
    tab.flags <- tab.flags[FLAG!=0]
    ## The smaller the number, the earlier the condition is
    ## applied. This must match what flagsAssign does.
    tab.flags <- tab.flags[order(FLAG)]


    allres.l <- lapply(1:tab.flags[,.N],function(I){
        resI <- data[FLAG==0|FLAG>tab.flags[I,FLAG],.(
                                                        ##        resI <- data[FLAG>tab.flags[I,FLAG],.(
                                                        N.left=uniqueN(ID),
                                                        Nobs.left=.N)
                    ,by=by]
        resI[,FLAG:=tab.flags[I,FLAG]]
        resI
    })
    allres <- rbindlist(allres.l)

### All data
    allres <- rbind(allres,
                    data[,.(N.left=uniqueN(ID)
                           ,Nobs.left=.N
                           ,FLAG=Inf
                            ),by=by]
                   ,fill=TRUE
                    )

    setorder(allres,-FLAG)
    allres[,N.discarded:=c(NA,-diff(N.left)),by=by]
    allres[,Nobs.discarded:=c(NA,-diff(Nobs.left)),by=by]

    
    allres <- rbind(allres,
                    data[FLAG==0,.(FLAG=0,N.left=uniqueN(ID),Nobs.left=.N,N.discarded=NA,Nobs.discarded=NA),by=by],
                    fill=T)
    
    ##  tab.flags <- rbind(tab.flags,data.table(FLAG=-Inf,flag="All data"),fill=TRUE)
### this is how many N/obs are left after the flags/conditions are applied
    
    allres <- mergeCheck(allres,rbind(tab.flags.0,tab.flags)[,.(FLAG,flag)],by="FLAG",all.x=T)
    allres[FLAG==Inf,flag:="All data"]
    allres[,notAll:=FLAG!=Inf]
    allres[,isFinal:=FLAG==0]
    setorderv(allres,c(by,"notAll","isFinal","FLAG"))

    ## tab.flags <- rbind(tab.flags.0,tab.flags,fill=TRUE)
    ## tab.report <- rbind(tab.report,
    ##                     tab.flags[-1,.(Data=paste("After exclusion due to",tolower(flag)),Nobs,NID)]
    ##                     )

### select columns to report, depending on argument
    allres[,`:=`(FLAG=NULL
                ,notAll=NULL
                ,isFinal=NULL)
           ]
    
    setcolorder(allres,c(by,"flag","N.left","Nobs.left","N.discarded","Nobs.discarded"))
    setnames(allres,"flag",col.cflag)
    
    if(!is.null(file)){
        ## write.csv(allres,file=file,quote=F,row.names=F)
        fwrite(allres,file=file,quote=F,row.names=F)
        cat(paste0("Table written to ",file,"\n"))
    }

    if(!data.was.data.table || !is.null(as.fun) ) {
        as.fun <- NMdataDecideOption("as.fun",as.fun)
        allres <- as.fun(allres)
    }
    
    return(allres)

}
