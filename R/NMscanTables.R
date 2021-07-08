##' read all output data tables in nonmem run
##' @param file the nonmem file to read (normally .mod or .lst)
##' @param details If TRUE, metadata is added to output. In this case,
##'     you get a list. Typically, this is mostly useful if
##'     programming up functions which behavior must depend on
##'     properties of the output.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param tab.count Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. However, if
##'     tab.count is TRUE (not default), this will be carried forward
##'     and added as a column called TABLENO.
##' @param col.id name of the subject ID column. Used for calculation
##'     of the number of subjects in each table.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param col.row The name of the row counter column. Optional and only
##'     used to check whether the row counter is in the data.
##' @return A list of all the tables as data.frames. If details=TRUE,
##'     this is in one element, called data, and meta is another
##'     element. If not, only the data is
##'     returned.
##' @family DataRead
##' @import data.table
##' @export

NMscanTables <- function(file,details=FALSE,as.fun,quiet,tab.count=FALSE,col.id="ID",col.row){

#### Section start: Dummy variables, only not to get NOTE's in package checks ####

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
    pastes <- NULL
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
    
    lines.table <- NMreadSection(file,section="TABLE",keepName=FALSE,
                                 keepComments=FALSE,keepEmpty=FALSE,
                                 asOne=FALSE, simplify=FALSE)
    if(length(lines.table)==0) {
        messageWrap("No TABLE sections found in control stream. Please inspect the control stream.",
                    fun.msg=stop)
    }
    
    tab.files <- lapply(lines.table,function(x) {
        tab <- data.table(file=filePathSimple(dir,extract.info(x,"FILE"))
                         ,name=extract.info(x,"FILE")
                         ,firstonly=any(grepl("FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY",x))
                         ,lastonly=any(grepl("LASTONLY",x))
                         ,firstlastonly=any(grepl("FIRSTLASTONLY",x))
                         ,format=extract.info(x,"FORMAT",default=" ")
                         ,stringsAsFactors=FALSE)
        tab
    })

    meta <- rbindlist(tab.files)
    meta[,`:=`(sep=ifelse(grepl(",",format),","," ")
              ,nrow=NA_real_
              ,ncol=NA_real_
               )]

    meta.tabs.strange <- meta[(firstonly+lastonly+firstlastonly)>1]
    if(nrow(meta.tabs.strange)){
        warning("Table(s) seems to have more than one of the firstonly, lastonly and firstlastonly options. Does this make sense? Look at table(s): ",pastes(meta.tabs.strange[,name],collapse=", "))
    }

    tables <- list()

    for(I in 1:nrow(meta)){
        if(!file.exists(meta[I,file])) stop(paste0("NMscanTables: File not found: ",meta[I,file],". Did you copy the lst file but forgot table
file?"))
        tables[[I]] <- NMreadTab(meta[I,file],quiet=TRUE,tab.count=tab.count,showProgress=FALSE,as.fun=identity)
        dim.tmp <- dim(tables[[I]])
        meta[I,nrow:=dim.tmp[1]]
        meta[I,ncol:=dim.tmp[2]]

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
    meta[,full.length:=(nrow==max(nrow))]
    if(has.row.level){   
        meta[full.length==TRUE,level:="row"]
        meta[full.length==FALSE,level:="id"]
    } else {
        meta[,level:="id"]
    }
    ## not supported
    meta[firstlastonly==TRUE,level:=NA_character_]
    
    meta[,scope:="all"]
    meta[firstonly==TRUE,scope:="firstonly"]
    meta[lastonly==TRUE,scope:="lastonly"]
    meta[firstlastonly==TRUE,scope:="firstlastonly"]
    meta[,`:=`(firstonly=NULL,
               lastonly=NULL,
               firstlastonly=NULL
               )]


    meta[,filetype:="output"]
##
##    meta[,full.length:=nrow==max(nrow)]
## test if all  have same length within level. 
    
    meta[,file.mtime:=file.mtime(file)]
    setcolorder(meta,intersect(c("source","name","nrow","ncol","nid","level","scope","has.col.row","has.col.id","full.length","filetype","format","sep","file.mtime","file"),colnames(meta)))
    
    
    if(!quiet){
        msg <- paste0("Number of output tables read: ",meta[,.N])
        NIDL <- meta[level=="id",.N]
        NFLO <- meta[firstlastonly==TRUE,.N]
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

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    tables <- lapply(tables,as.fun)
    meta <- as.fun(meta)

    if(details){
        out <- list(data=tables,meta=meta)
    } else {
        out <- tables
    }
    
    return(out)

}
