##' Write dataset for use in Nonmem (and R)
##'
##' Instead of trying to remember the arguments to pass to write.csv,
##' use this wrapper. It tells you what to write in $DATA and $INPUT
##' in nonmem, and it exports an rds file as well which is highly
##' preferable for use in R. It never edits the data before writing
##' the datafile.
##'
##' @param data The dataset to write to Nonmem.
##' @param file The file to write to. The extension (everything after
##'     and including last ".") is dropped. csv, rds and other
##'     standard file name extensions are added.
##' @param write.csv Write to csv file?
##' @param write.RData In case you want to save to .RData object. Not
##'     recommended. Use write.rds instead.
##' @param write.rds write an rds file?
##' @param force.row Ensure that data contains a ROW column counting
##'     the rows in the dataset, and add one if none exists. Defaults
##'     to FALSE (default is not editing the dataset at all).
##' @param script If provided, the object will be stamped with this
##'     script name before saved to rds.
##' @param args.stamp A list of arguments to be passed to stampObj.
##' @param args.rds A list of arguments to be passed to saveRDS.
##' @param col.flag Name of a numeric column with zero value for rows
##'     to include in Nonmem run, non-zero for rows to skip. The
##'     argument is only used for the $DATA. To skip this feature, use
##'     col.flag=NULL.
##' @param nmdrop Columns to drop in Nonmem $DATA. This has two
##'     implications. One being that the proposed $DATA indicased
##'     =DROP after the given column names. The other that in case it
##'     is a non-numeric column, succeeding columns can still be
##'     included.
##' @param nmdir.data For the $DATA text proposal only. The path to
##'     the input datafile to be used in the Nonmem $DATA
##'     section. Often, a relative path to the actual Nonmem run is
##'     wanted here.
##' @return Text for inclusion in Nonmem control stream, invisibly.
##' @details When writing csv files, the file will be
##'     comma-separated. Because Nonmem does not support quoted
##'     fields, you must avoid commas in character fields. At the
##'     moment, no check for this is being done.
##'
##' The user is provided with text to use in Nonmem. This lists names
##' of the data columns. Once a column is reached that Nonmem will not
##' be able to read as a numeric and column is not in nmdrop, the list is stopped.
##' 
##' @family Nonmem
##' @export



NMwriteData <- function(data,file,write.csv=TRUE,write.RData=F,
                        write.rds=write.csv,force.row=FALSE,script,
                        args.stamp,args.rds,nmdrop,nmdir.data,
                        col.flag="FLAG"){

    stopifnot(is.data.frame(data)) ## data.out <- as.data.frame(data)
    

#### Section start: Process arguments ####

### stamp arguments
    doStamp <- TRUE
    if(missing(args.stamp)) {
        args.stamp <- list()
    } else {
        if(!is.list(args.stamp)){
            stop("args.stamp must be a list of arguments.")
        }
    }
    if(missing(script)){
        doStamp <- FALSE
    } else {
        args.stamp$script <- script
        doStamp <- TRUE
    }
    
### rds arguments
    if(!missing(args.rds) && !write.rds ){
        warning("args.rds supplied, but write.rds is FALSE. rds file will not be written.")
    }
    if(missing(args.rds)) {
        args.rds <- list()
    } else {
        if(!is.list(args.rds)){
            stop("args.rds must be a list of arguments.")
        }
    }

    if(missing(nmdrop)) {
        nmdrop <- NULL
    } else {
        if(!is.null(nmdrop) && !is.character(nmdrop) ) {
            stop("If supplied, nmdrop must be of type character.")
        }
        if(any(is.na(nmdrop)|nmdrop=="")){
            stop("nmdrop cannot contain empty strings and NA's.")
        }
    }
    
###  Section end: Process arguments


### this function is used to replace .csv or whatever ending is used
### to .rds, .RData etc. file is path, ext is extension without .,
### e.g. "rds".
    transFileName <- function(file,ext){
        file.new <- sub("\\.[^\\.]+$",paste0(".",ext),file)
        file.new
    }
    
    
    ## we must not quote. ID is often a character. If quoted, nonmem
    ## will not be able to read. So avoid commas in strings.
    quote <- FALSE
    
    ## only report numerics to user. But this is not good enough. Only report
    ## until first character. Moreover, it's not this easy. Variables may be
    ## character but still be interpretable as numeric. Often ID is like this.
### we use data.table. Remember to transform back and forth.
    data.dt <- copy(as.data.table(data))

    ## Check if character variables contain commas
    ## This would cause trouble when writing csv
    
    
    has.no.comma <- data.dt[,lapply(.SD,function(x){is.numeric(x)||!any(grepl(",",as.character(x)))})]

    
    ## OK if numeric, or all but "" interprets as numeric
    as.num.ok <- data.dt[,lapply(.SD,function(x)
        is.numeric(x) ||
        suppressWarnings(!any(is.na(as.numeric(as.character(x)[as.character(x)!=""])))))]
    ## Allow TIME even if non-numeric. 
    if("TIME"%in%colnames(data.dt) &&
       as.num.ok[,TIME==FALSE]) {
        as.num.ok[,TIME:=TRUE]
    }
    
    dt.num.ok <- data.table(
        col=colnames(as.num.ok)
       ,numeric.ok=as.logical(as.num.ok[1])
       ,comma.ok=as.logical(has.no.comma[1])
    )
    dt.num.ok[,name.nm:=toupper(col)]

    if(dt.num.ok[,any(!comma.ok)]){
        messageWrap(paste("You must avoid commas in data values. They will curropt the csv file, so get rid of them before saving data. Comma found in column(s):",paste(dt.num.ok[comma.ok==FALSE,col],sep=", ")),
                    fun.msg=stop)
    }

    
    ## if any repetetions, give warning, and add numbers
    
### this is input column names. 
    if(dt.num.ok[,any(duplicated(col))]) {
        warning(paste("Duplicated column name(s) in data:",
                      paste0(dt.num.ok[duplicated(col),unique(col)],collapse=", ")
                      ))
    }


    ## apply DROP
    dt.num.ok[,drop:=FALSE]
    if(!is.null(nmdrop)){
        dt.num.ok[col%in%nmdrop,name.nm:=paste0(name.nm,"=DROP")]
        dt.num.ok[col%in%nmdrop,drop:=TRUE]
        drops.not.used <- nmdrop[!nmdrop%in%dt.num.ok[,col]]
        if(length(drops.not.used)){
            warning("Elements in nmdrop not found as columns in data:",paste(drops.not.used,collapse=", "))
        }
    }

    dt.num.ok[,include:=cumsum(!numeric.ok&!drop)<1]

    dt.num.ok[include==TRUE,occ.cum:=1:.N,by=name.nm]
    if(dt.num.ok[occ.cum>1,.N]>0) {
        warning(paste("Duplicated column name(s) in data after transforming to upper case for Nonmem:\n",
                      paste0(dt.num.ok[occ.cum>1,unique(name.nm)],collapse=", "),"\n",
                      "Names have been numbered in $INPUT proposal."
                      ))
    }
    dt.num.ok[occ.cum>1,name.nm:=paste0(name.nm,occ.cum)]

    colnames.nm <- dt.num.ok[include==TRUE,name.nm]

    nmfile <- file
    if(!missing(nmdir.data)){
        nmfile <- file.path(nmdir.data,basename(nmdir.data))
    }

    text.nm <- c(
        strwrap(
            paste0("$INPUT ",paste(colnames.nm,collapse=" "))
        )
       ,paste0("$DATA ", nmfile)
       ,paste0("IGN=@")
    )
    if(!is.null(col.flag)&&col.flag%in%colnames.nm){
        text.nm <- c(text.nm,
                     paste0("IGNORE=(",col.flag,".NE.0)")
                     )
    }

    message(
        paste0("Nonmem data file:",file,"\n",
               "For NonMem:\n",
               paste(text.nm,collapse="\n")
               ))
    
    written <- FALSE
    if(write.csv){
        opt.orig <- options(scipen=15)
        file.csv <- transFileName(file,"csv")
        write.csv(data,na=".",quote=quote,row.names=FALSE,file=file.csv)
        options(opt.orig)
        written <- TRUE
    }
    if(write.RData){
        name.data <- deparse(substitute(data))
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .RData. Choose a .csv file name.")
        file.RData <- transFileName(file,"RData")
        if(doStamp) data <- do.call(stampObj,append(list(data=data,writtenTo=file.RData),args.stamp))
        assign(name.data,data)
        save(list=name.data,file=file.RData)
        written <- TRUE
    }
    if(write.rds){
        ## A dot and then something is needed in the name for us to be able to
        ## translate
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .rds. Choose a .csv file name.")
        file.rds <- transFileName(file,"rds")
        if(doStamp) data <- do.call(stampObj,append(list(data=data,writtenTo=file.rds),args.stamp))
        do.call(saveRDS,append(list(object=data,file=file.rds),args.rds))
        written <- TRUE
    }
    if(written){
        cat("\nData file(s) written.\n")
    } else {
        cat("\nData returned but not written to file(s).\n")
    }
    invisible(text.nm)

}
