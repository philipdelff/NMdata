##' Find and read all output data tables in Nonmem run
##' @param file the Nonmem file to read (normally .mod or .lst)
##' @param details If TRUE, metadata is added to output. In this case,
##'     you get a list. Typically, this is mostly useful if
##'     programming up functions which behavior must depend on
##'     properties of the output.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param rep.count Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. However, if
##'     rep.count is TRUE (not default), this will be carried forward
##'     and added as a column called NMREP. Even if NMREP is generated
##'     by NMscanTables, it is treated like any other table column in
##'     meta (?NMinfo) data.
##' @param col.id name of the subject ID column. Used for calculation
##'     of the number of subjects in each table.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param col.row The name of the row counter column. Optional and
##'     only used to check whether the row counter is in the data.
##' @param skip.absent Skip missing output table files with a warning?
##'     Default is FALSE in which case an error is thrown.
##' @param meta.only If TRUE, tables are not read, only a table is
##'     returned showing what tables were found and some available
##'     meta information. Notice, not all meta information (e.g.,
##'     dimensions) are available because the tables need to be read
##'     to derive that.
##' @return A list of all the tables as data.frames. If details=TRUE,
##'     this is in one element, called data, and meta is another
##'     element. If not, only the data is returned.
##' @examples
##' tabs1 <- NMscanTables(system.file("examples/nonmem/xgxr001.lst", package="NMdata"))
##' @family DataRead
##' @import data.table
##' @export

NMscanTables <- function(file,as.fun,quiet,rep.count=FALSE,col.id="ID",col.row,details,skip.absent=FALSE,meta.only=FALSE){
    
#### Section start: Dummy variables, only not to get NOTE's in package checks ####

    DV <- NULL
    PRED <- NULL
    RES <- NULL
    WRES <- NULL
    has.col.id <- NULL
    has.col.row <- NULL
    filetype <- NULL
    maxLength <- NULL
    full.length <- NULL
    firstlastonly <- NULL
    firstonly <- NULL
    lastonly <- NULL
    level <- NULL
    name <- NULL
    nid <- NULL
    noheader <- NULL
    scope <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(col.row)||(!is.null(col.row)&&is.na(col.row))||(is.character(col.row)&&any(col.row==""))) {
        col.row <- NULL
    }    
    col.row <- NMdataDecideOption("col.row",col.row)

    ## if(!missing(details)) message("NMscanTables: argument details is deprecated.")
    args <- getArgs()
    deprecatedArg(oldarg="details",args=args)
    
    dir <- dirname(file)
    extract.info <- function(x,NAME,default){
        r1 <- regexpr(paste0(NAME," *= *[^ ]*"),x,ignore.case=TRUE)
        rm1 <- regmatches(x,r1)
        info <- sub(paste0(NAME," *= *"),"",rm1,ignore.case=TRUE)
        if(length(info)==0&&!missing(default)) {
            info <- default
        }
        info
    }
    
    lines.table <- NMreadSection(file,section="TABLE",keep.name=FALSE,
                                 keep.comments=FALSE,keep.empty=FALSE,
                                 as.one=FALSE, simplify=FALSE)
    if(length(lines.table)==0) {
        if(!quiet) messageWrap("No TABLE sections found in control stream. Returning NULL",
                               fun.msg=message)
        return(NULL)
    }
    
    tab.files <- lapply(lines.table,function(x) {
        tab <- data.table(file=filePathSimple(dir,extract.info(x,"FILE"))
                         ,name=extract.info(x,"FILE")
                         ,firstonly=any(grepl("FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY",x))
                         ,lastonly=any(grepl("LASTONLY",x))
                         ,firstlastonly=any(grepl("FIRSTLASTONLY",x))
                         ,format=extract.info(x,"FORMAT",default=" ")
                         ,noheader=any(grepl("NOHEADER",x)))
        tab
    })

    meta <- rbindlist(tab.files)
    ## seperator is not being used, so left out from metadata
    ##sep=ifelse(grepl(",",format),","," "),
    meta[,`:=`(nrow=NA_real_
              ,ncol=NA_real_
               )]

    meta.tabs.strange <- meta[(firstonly+lastonly+firstlastonly)>1]
    if(nrow(meta.tabs.strange)){
        warning("Table(s) seems to have more than one of the firstonly, lastonly and firstlastonly options. Does this make sense? Look at table(s): ",paste(meta.tabs.strange[,name],collapse=", "))
    }
    tables <- list()

    meta[,exists:=file.exists(file)]
    meta[,filetype:="output"]    
    meta[,file.mtime:=file.mtime(file)]

    if(meta.only){
        setcolorder(meta,intersect(c("source","name","nrow","ncol","nid","level","scope","has.col.row","has.col.id","full.length","filetype","format",
                                     "file.mtime","file"),colnames(meta)))
        return(as.fun(meta))
    }

######## read data
    for(I in 1:nrow(meta)){
        if(meta[I,!exists]) {
            if(skip.absent) {
                messageWrap(paste0("NMscanTables: File not found: ",meta[I,file],". Skipping."),fun.msg=message)
                next
            }
            stop(paste0("NMscanTables: File not found: ",meta[I,file],". Did you copy the lst file but forgot table file?"))
        }
        
        tables[[I]] <- NMreadTab(meta[I,file],quiet=TRUE,rep.count=rep.count,showProgress=FALSE,as.fun=identity,header=meta[I,!noheader])
### to not include NMREP when counting columns
        ## dim.tmp <- dim(tables[[I]][,!colnames(tables[[I]])=="NMREP",with=FALSE])
        dim.tmp <- dim(tables[[I]])
        meta[I,nrow:=dim.tmp[1]]
        meta[I,ncol:=dim.tmp[2]]
        
        if(meta[I,noheader]) {
            messageWrap("Using NOHEADER option in $TABLE is only experimentally supported in NMdata. Please double check the resuling column names. NMdata functions can handle the recurring headers in Nonmem tables so the NOHEADER option should not be needed.",fun.msg=message)
            cnames.text <- lines.table[[I]]
            cnames.text <- gsub(","," ",cnames.text)
            cnames.text <- sub("^ *","",cnames.text)
            cnames.text <- sub(" +"," ",cnames.text)
            cnames.all <- strsplit(cnames.text," ")[[1]]

            cnames.extra <- cc(DV,PRED,RES,WRES)
            cnames.extra <- setdiff(cnames.extra,cnames.all)
            ncol.I <- ncol(tables[[I]])
            nce <- length(cnames.extra)
            
            if(nce){
                ## Adding 1 to indeces because NMreadTab adds NMREP
                cnames.all[(ncol.I-nce+1):(ncol.I)] <- cnames.extra
            }
            cnames <- cnames.all[1:ncol.I]
            
            setnames(tables[[I]],1:length(cnames),cnames)
        }
        
        cnames.tab.I <- colnames(tables[[I]])

        if(col.id%in%cnames.tab.I){
            meta[I,nid:=tables[[I]][,uniqueN(get(col.id))]]
        } else {
            meta[I,nid:=NA_real_]
        }
        
        if(!is.null(col.row)){
            meta[I,has.col.row:=col.row%in%cnames.tab.I]
        }
        if(!is.null(col.id)){
            meta[I,has.col.id:=col.id%in%cnames.tab.I]
        }
    }

    
    has.row.level <- meta[,any((firstonly+lastonly+firstlastonly)==0)]
    ## level is the observed level. If a first only table has as many
    ## rows as a row-level table, it's row label.
    if(has.row.level){
        meta[,full.length:=(nrow==max(nrow,na.rm=TRUE))]
        meta[full.length==TRUE,level:="row"]
        meta[full.length==FALSE,level:="id"]
    } else {
        meta[,full.length:=FALSE]
        meta[!is.na(nrow),level:="id"]
    }
    ## not supported
    meta[firstlastonly==TRUE,level:=NA_character_]
    
    meta[exists==TRUE,scope:="all"]
    meta[firstonly==TRUE,scope:="firstonly"]
    meta[lastonly==TRUE,scope:="lastonly"]
    meta[firstlastonly==TRUE,scope:="firstlastonly"]
    meta[,`:=`(firstonly=NULL,
               lastonly=NULL,
               firstlastonly=NULL
               )]


    ## sep not used so omitted 
    setcolorder(meta,intersect(c("source","name","nrow","ncol","nid","level","scope","has.col.row","has.col.id","full.length","filetype","format",
                                 "file.mtime","file"),colnames(meta)))
    
    
    if(!quiet){
        msg <- paste0("Number of output tables read: ",meta[exists==TRUE,.N])
        NIDL <- meta[exists==TRUE&level=="id",.N]
        NFLO <- meta[exists==TRUE&scope=="firstlastonly",.N]
        mNIDL <- NULL
        if(NIDL>0) mNIDL <- paste0(NIDL, " idlevel")
        mNFLO <- NULL
        if(NFLO>0) mNFLO <- paste0(NFLO, " FIRSTLASTONLY")
        msg.extra <- paste0(c(mNIDL,mNFLO),collapse=" and ")
        if(nchar(msg.extra)) {
            msg <- paste0(msg," (",msg.extra,").")
        } else {
            msg <- paste0(msg,".")
        }
        
        message(msg)
    }
    
    names(tables) <- meta[,name]


    tables <- lapply(tables,as.fun)
    
    writeNMinfo(tables,list(tables=meta),byRef=TRUE)
    
    return(tables)

}
