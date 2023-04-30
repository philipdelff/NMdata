##' Create an overview of number of retained and discarded datapoints.
##'
##' Generate an overview of number of observations disregarded due to
##' different reasons. And how many are left after each exclusion
##' flag.
##' 
##' @param data The dataset including both FLAG and flag columns.
##' @param tab.flags A data.frame containing at least these named
##'     columns: FLAG, flag, condition. Condition is disregarded for
##'     FLAG==0.
##' @param file A file to write the table of flag counts to. Will
##'     probably be removed and put in a separate function.
##' @param col.id The name of the subject ID column. Default is "ID".
##' @param col.flagn The name of the column containing the numerical
##'     flag values in tab.flags. This will be added to data. Use the
##'     same as when flagsAssign was called (if that was
##'     used). Default value is FLAG and can be configured using
##'     NMdataConf.
##' @param col.flagc The name of the column containing the character
##'     flag values in data and tab.flags. Use the same as when
##'     flagsAssign was called (if that was used). Default value is
##'     flag and can be configured using NMdataConf.
##' @param by An optional column to group the counting by. This could
##'     be "STUDY", "DRUG", "EVID", or a combination of multiple
##'     columns.
##' @param flags.increasing The flags are applied by either decreasing
##'     (default) or increasing value of col.flagn. By using
##'     decreasing order, you can easily adjust the Nonmem IGNORE
##'     statement from IGNORE(FLAG.NE.0) to say IGNORE(FLAG.GT.10) if
##'     BLQ's have FLAG=10, and you decide to include these in the
##'     analysis.
##' @param name.all.data What to call the total set of data before
##'     applying exclusion flags. Default is "All available data".
##' @param grp.incomp Column(s) that distinct incompatible subsets of
##'     data. Default is "EVID" meaning that if different values of
##'     EVID are found in data, the function will return an
##'     error. This is a safeguard not to mix data unintentionally
##'     when counting flags.
##' @param save Save file? Default is TRUE, meaning that a file will
##'     be written if file argument is supplied.
##' @param flagc.0 The character flag to assign to rows that are not
##'     matched by exclusion conditions (numerical flag 0).
##' @param as.fun The default is to return a data.table if input data
##'     is a data.table, and return a data.frame for all other input
##'     classes. Pass a function in as.fun to convert to something
##'     else. If data is not a data.table, default can be configured
##'     using NMdataConf.
##' @return A summary table with number of discarded and retained
##'     subjects and observations when applying each condition in the
##'     flag table. "discarded" means that the reduction of number of
##'     observations and subjects resulting from the flag, "retained"
##'     means the numbers that are left after application of the
##'     flag. The default is "both" which will report both. Class as
##'     defined by as.fun.
##' @details This function is used to count flags as assigned by the
##'     flagsAssign function.
##'
##' Notice that the character flags reported in the output table are
##' taken from tab.flags. The data column named by the value of
##' col.flagc (default is flag) is not used.
##' 
##' In the returned table, N.discarded is the difference in number of
##'     subjects since previous step. If two is reported, it can mean
##'     that the remaining one observation of these two subjects are
##'     discarded due to this flag. The majority of the samples can
##'     have been discarded by earlier flags.
##' @import data.table
##' @family DataCreate
##' @examples
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' dt.flags <- data.frame(
##'        flagn=10,
##'        flagc="Below LLOQ",
##'        condition=c("BLQ==1")
##' )
##' pk <- flagsAssign(pk,dt.flags,subset.data="EVID==0",col.flagn="flagn",col.flagc="flagc")
##' pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing",
##'         col.flagn="flagn",col.flagc="flagc")
##' unique(pk[,c("EVID","flagn","flagc","BLQ")])
##' flagsCount(pk[EVID==0],dt.flags,col.flagn="flagn",col.flagc="flagc")
##' @export


flagsCount <- function(data,tab.flags,file,col.id="ID",
                       col.flagn,col.flagc,
                       by=NULL, flags.increasing=FALSE,
                       flagc.0="Analysis set",
                       name.all.data="All available data",
                       grp.incomp="EVID",save=TRUE,as.fun=NULL){
    
#### Section start: Dummy variables, only not to get NOTE's in package checks ####

    FLAG <- NULL
    FLAGSORT <- NULL
    ID <- NULL
    N.discard <- NULL
    N.left <- NULL
    Nobs.discard <- NULL
    Nobs.left <- NULL
    N.disc.cum <- NULL
    N.discard.0 <- NULL
    Nobs.disc.cum <- NULL
    Nobs.discard.0 <- NULL
    alldata <- NULL
    flag <- NULL
    notAll <- NULL
    isFinal <- NULL
    . <- function() NULL
    
### Section end: Dummy variables, only not to get NOTE's in package checks

    if(missing(as.fun)) as.fun <- NULL
    as.fun.arg <- as.fun
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(file)) file <- NULL
    if(missing(col.flagn)) col.flagn <- NULL
    if(missing(col.flagc)) col.flagc <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    col.flagc <- NMdataDecideOption("col.flagc",col.flagc)
    
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
        tab.flags.was.data.table <- FALSE
    }

    cnames.data <- copy(colnames(data))
    
### check for incompatible groups (say doses and observations)
    check.incomp <- setdiff(intersect(cnames.data,grp.incomp),by)
    if(length(check.incomp)>0) {
        covs.incomp <- findCovs(data[,check.incomp,with=FALSE])
        if(length(check.incomp)>ncol(covs.incomp)){
            cols.incomp <- setdiff(check.incomp,colnames(covs.incomp))
            stop(sprintf("Incompatible data included. Column(s) %s is non-unique. Consider running on a subset of data or see argument grp.incomp.",paste(cols.incomp,sep=", ")))
        }
    }
    
### check that tab.flags contains col.flagn and col.flagc and that data contains col.flagn
    if(!col.flagn%in%cnames.data) messageWrap("data must contain a column with same name as value of col.flagn",fun.msg="stop")

### col.flagn must have unique values in tab.flags. Otherwise we can't rank them.
    if(any(duplicated(tab.flags[,col.flagn,with=FALSE]))){
        messageWrap("tab.flags must have unique numerical flags.")
    }
    
    cnames.tab.flags <- copy(colnames(tab.flags))
    if(!col.flagn%in%cnames.tab.flags)
        messageWrap("tab.flags must contain a column with same name as value of col.flagn",fun.msg="stop")
    if(!col.flagc%in%cnames.tab.flags)
        messageWrap("tab.flags must contain a column with same name as value of col.flagc",fun.msg="stop")

###  rename columns to FLAG and flag
    tab.flags[,FLAG:=get(col.flagn)]
    tab.flags[,flag:=get(col.flagc)]
    data[,FLAG:=get(col.flagn)]


### if 0 and Inf are not in tab.flags, insert them
    if(!0%in%tab.flags[,FLAG]){
        tab.flags <- rbind(tab.flags,
                           data.table(FLAG=0,flag=flagc.0)
                          ,fill=TRUE
                           )
    }


    tab.flags.0 <- tab.flags[FLAG==0]
    tab.flags <- tab.flags[FLAG!=0]
    ## The smaller the number, the earlier the condition is
    ## applied. This must match what flagsAssign does.

    if(flags.increasing){
        setorder(tab.flags,FLAG)

        allres.l <- lapply(1:tab.flags[,.N],function(I){
            resI <- data[FLAG==0|FLAG>tab.flags[I,FLAG],.(
                                                            N.left=uniqueN(get(col.id)),
                                                            Nobs.left=.N)
                        ,by=by]
            resI[,FLAG:=tab.flags[I,FLAG]]
            resI
        })
    } else {
        setorder(tab.flags,-FLAG)

        allres.l <- lapply(1:tab.flags[,.N],function(I){
            resI <- data[FLAG==0|FLAG<tab.flags[I,FLAG],.(
                                                            N.left=uniqueN(get(col.id)),
                                                            Nobs.left=.N)
                        ,by=by]
            resI[,FLAG:=tab.flags[I,FLAG]]
            resI
        })
    }
    

    allres0 <- rbindlist(allres.l)

### All data
    FLAG.alldata <- Inf
    if(flags.increasing){
        FLAG.alldata <- -FLAG.alldata
    }
    
    allres <- rbind(allres0,
                    ## this is all data - must be returned as first row.
                    data[,.(N.left=uniqueN(get(col.id))
                           ,Nobs.left=.N
                           ,FLAG=FLAG.alldata
                           ,alldata=TRUE
                            ),by=by]
                   ,fill=TRUE
                    )

    
    
    if(flags.increasing){
        setorder(allres,FLAG)
    } else {
        setorder(allres,-FLAG)
    }

    allres[,N.discard:=c(NA,-diff(N.left)),by=by]
    
    
    allres[,Nobs.discard:=c(NA,-diff(Nobs.left)),by=by]
    
    
    allres <- rbind(allres,
                    ## this is the analysis set
                    data[FLAG==0,.(FLAG=0,N.left=uniqueN(get(col.id)),Nobs.left=.N,N.discard=NA,Nobs.discard=NA),by=by],
                    fill=TRUE)
    

### this is how many N/obs are left after the flags/conditions are applied
    
    allres[is.na(alldata),alldata:=FALSE]
    allres <- mergeCheck(allres,rbind(tab.flags.0,tab.flags)[,.(FLAG,flag)],by="FLAG",all.x=TRUE,quiet=TRUE)
    allres[alldata==TRUE,`:=`(flag=name.all.data)]
    allres[,notAll:=alldata!=1]
    allres[,isFinal:=FLAG==0]

    allres[,FLAGSORT:=FLAG]
    if(!flags.increasing){
        allres[,FLAGSORT:=-FLAG]
    } 
    setorderv(allres,c(by,"notAll","isFinal","FLAGSORT"))

    allres[,N.discard.0:=N.discard]
    allres[is.na(N.discard),N.discard.0:=0]
    allres[,N.disc.cum:=cumsum(N.discard.0),by=by]
    allres[,Nobs.discard.0:=Nobs.discard]
    allres[is.na(Nobs.discard),Nobs.discard.0:=0]
    allres[,Nobs.disc.cum:=cumsum(Nobs.discard.0),by=by]
    allres[alldata==TRUE,`:=`(N.disc.cum=NA,Nobs.disc.cum=NA)]

### select columns to report, depending on argument
    allres[,`:=`(FLAG=NULL
                ,notAll=NULL
                ,isFinal=NULL
                ,alldata=NULL
                ,FLAGSORT=NULL
                ,N.discard.0=NULL
                ,Nobs.discard.0=NULL
                 )
           ]

    setcolorder(allres,c(by,"flag","N.left","Nobs.left","N.discard","N.disc.cum","Nobs.discard","Nobs.disc.cum"))
    setnames(allres,"flag",col.flagc)

    if(!is.null(file)&&save){
        fwrite(allres,file=file,quote=FALSE,row.names=F)
        cat(paste0("Table written to ",file,"\n"))
    }

    
    if(data.was.data.table && is.null(as.fun.arg)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    allres <- as.fun(allres)

    return(allres)
}
