##' read all output data tables in nonmem run
##' @param file the nonmem file to read (normally .mod or .lst)
##' @param details If TRUE, metadata is added to output. In this case, you get a
##'     list. I'd say, enable if doing programming.
##' @param quiet The default is to give some information along the way on what
##'     data is found. But consider setting this to TRUE for non-interactive
##'     use.
##' @param as.dt return data.tables? Tables will not be keyed.
##' @param debug Start by calling browser()?
##' @return A list of all the tables as data.frames. If details=TRUE, this is in
##'     one element, called data, and meta is another element. If not, only the
##'     element corresponding to data is returned.
##' @family Nonmem
##' @import data.table
##' @export
NMscanTables <- function(file,details=F,as.dt=TRUE,quiet=FALSE,debug=FALSE){
    if(debug) browser()
    
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
    
    lines.table <- NMgetSection(file,section="TABLE",keepName=F,keepComments=F,keepEmpty=F,asOne=F,simplify=F)


    tab.files <- lapply(lines.table,function(x) {
        tab <- data.frame(file=filePathSimple(dir,extract.info(x,"FILE"))
                         ,name=extract.info(x,"FILE")
                         ,firstonly=any(grepl("FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY",x))
                         ,format=extract.info(x,"FORMAT",default=" ")
                         ,stringsAsFactors=F)
        tab <- within(tab,{sep=ifelse(grepl(",",tab$format),","," ")})
        tab
    })

    ## if(!all(tab.files$sep=",")) stop("Not all tables are comma separated. Only comma separated tables are supported right now.")
    
    meta <- do.call(rbind,tab.files)
    meta$nrow <- NA_real_
    meta$ncol <- NA_real_
    tables <- list()
    for(I in 1:nrow(meta)){
        if(!file.exists(meta[I,"file"])) stop(paste0("NMscanTables: File not found: ",meta[I,'file'],". Did you copy the lst file but forgot table file?"))
        tables[[I]] <- NMreadTab(meta[I,"file"],sep=meta[I,"sep"],silent=T,showProgress=FALSE)
        dim.tmp <- dim(tables[[I]])
        meta[I,"nrow"] <- dim.tmp[1]
        meta[I,"ncol"] <- dim.tmp[2]
    }

    if(!quiet){
        message(paste0("Read ",nrow(meta)," table(s)."))
    }

    names(tables) <- meta$name
    if(!as.dt) {
        tables <- lapply(tables,as.data.frame)
        meta <- as.data.frame(meta)
    } else {
        meta <- as.data.table(meta)
    }
    
    if(details){
        out <- list(data=tables,meta=meta)
    } else {
        out <- tables
    }
    
    return(out)

}
