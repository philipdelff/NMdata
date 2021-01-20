##' read all output data tables in nonmem run
##' @param file the nonmem file to read (normally .mod or .lst)
##' @param details If TRUE, metadata is added to output. In this case,
##'     you get a list. Typically, this is mostly useful if
##'     programming up functions which behavior must depend on
##'     properties of the output. 
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use.
##' @param tab.count Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. However, if
##'     tab.count is TRUE (default), this will be carried forward and
##'     added as a column called TABLENO.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun=identity. See ?runAsFun.
##' @return A list of all the tables as data.frames. If details=TRUE,
##'     this is in one element, called data, and meta is another
##'     element. If not, only the element corresponding to data is
##'     returned.
##' @family DataRead
##' @import data.table
##' @export

NMscanTables <- function(file,details=F,as.fun=NULL,quiet=FALSE,tab.count=TRUE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    firstlastonly <- NULL
    firstonly <- NULL
    idlevel <- NULL
    lastonly <- NULL
    name <- NULL
    pastes <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks ####

    dir <- dirname(file)
    extract.info <- function(x,NAME,default){
        r1 <- regexpr(paste0(NAME," *= *[^ ]*"),x,ignore.case=T)
        rm1 <- regmatches(x,r1)
        info <- sub(paste0(NAME," *= *"),"",rm1,ignore.case=T)
        if(length(info)==0&&!missing(default)) {
            info <- default
        }
        info
    }
    
    lines.table <- NMgetSection(file,section="TABLE",keepName=F,
                                keepComments=F,keepEmpty=F,asOne=F,
                                simplify=F)
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
                         ,stringsAsFactors=F)
        tab
    })

    meta <- rbindlist(tab.files)
    meta[,`:=`(sep=ifelse(grepl(",",format),","," ")
              ,nrow=NA_real_
              ,ncol=NA_real_
               )]

    meta.tabs.strange <- meta[firstonly+lastonly+firstlastonly>1]
    if(nrow(meta.tabs.strange)){
        warning("Table(s) seems to have more than one of the firstonly, lastonly and firstlastonly options. Does this make sense? Look at table(s): ",pastes(meta.tabs.strange[,name],collapse=", "))
    }

    tables <- list()
    for(I in 1:nrow(meta)){
        if(!file.exists(meta[I,file])) stop(paste0("NMscanTables: File not found: ",meta[I,file],". Did you copy the lst file but forgot table
file?"))
        tables[[I]] <- NMreadTab(meta[I,file],quiet=T,tab.count=tab.count,showProgress=FALSE,as.fun=identity)
        dim.tmp <- dim(tables[[I]])
        meta[I,nrow:=dim.tmp[1]]
        meta[I,ncol:=dim.tmp[2]]
    }
    
    meta[,idlevel:=firstonly|lastonly]
    meta[,file.mtime:=file.mtime(file)]
    
    if(!quiet){
        msg <- paste0("Number of output tables read: ",meta[,.N])
        NIDL <- meta[idlevel==TRUE&firstlastonly==FALSE,.N]
        NFLO <- meta[idlevel==FALSE&firstlastonly==TRUE,.N]
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
        
        ## message(paste0("Read ",nrow(meta)," output table(s)."))
        ## message(paste0("Number of output tables read: ",meta[,.N], " (",NFO," idlevel and ",NFLO," FIRSTLASTONLY)"))
        message(msg)
        

    }
    
    names(tables) <- meta[,name]

    
    ## tables <- lapply(tables,runAsFun,as.fun=as.fun)
    ## meta <- runAsFun(meta,as.fun=as.fun)
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
