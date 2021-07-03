##' Write dataset for use in Nonmem (and R)
##'
##' Instead of trying to remember the arguments to pass to write.csv,
##' use this wrapper. It tells you what to write in $DATA and $INPUT
##' in nonmem, and it (additionally) exports an rds or Rdata file as
##' well which is highly preferable for use in R. It never edits the
##' data before writing the datafile. The filenames for csv, rds
##' etc. are derived by replacing the extension to the filename given
##' in the file argument.
##'
##' @param data The dataset to write to file for use in Nonmem.
##' @param file The file to write to. The extension (everything after
##'     and including last ".") is dropped. csv, rds and other
##'     standard file name extensions are added.
##' @param write.csv Write to csv file?
##' @param write.rds write an rds file?
##' @param write.RData In case you want to save to .RData object. Not
##'     recommended. Use write.rds instead.
##' @param script If provided, the object will be stamped with this
##'     script name before saved to rds or Rdata. See ?NMstamp.
##' @param args.stamp A list of arguments to be passed to NMstamp.
##' @param args.fwrite List of arguments passed to fwrite. Notice that
##'     except for "x" and "file", you need to supply all arguments to
##'     fwrite if you use this argument. Default values can be
##'     configured using NMdataConf.
##' @param args.rds A list of arguments to be passed to saveRDS.
##' @param args.RData A list of arguments to be passed to save.
##' @param col.flag Name of a numeric column with zero value for rows
##'     to include in Nonmem run, non-zero for rows to skip. The
##'     argument is only used for generating the proposed text to
##'     paste into the Nonmem control stream. To skip this feature,
##'     use col.flag=NULL.
##' @param nmdir.data For the $DATA text proposal only. The path to
##'     the input datafile to be used in the Nonmem $DATA
##'     section. Often, a relative path to the actual Nonmem run is
##'     wanted here.
##' @param nm.copy For the $DATA text proposal only. If you plan to
##'     rename columns in Nonmem $DATA, NMwriteData can adjust the
##'     suggested $DATA text. If you plan to use CONC as DV in Nonmem,
##'     you can include nm.rename=c(DV="CONC").
##' @param nm.rename For the $DATA text proposal only. If you plan to
##'     rename columns in Nonmem $DATA, NMwriteData can adjust the
##'     suggested $DATA text. If you plan to use BBW instead of BWBASE
##'     in Nonmem, consider nm.rename=c(BWBASE="BBW"). The result is
##'     different from nm.copy since the nm.copy syntax is only
##'     allowed by Nonmem for certain standard column names such as
##'     DV.
##' @param nm.drop Only used for generation of proposed text for
##'     Nonmem control stream. Columns to drop in Nonmem $DATA. This
##'     has two implications. One is that the proposed $DATA indicates
##'     =DROP after the given column names. The other that in case it
##'     is a non-numeric column, succeeding columns can still be
##'     included.
##' @param nm.capitalize For the $DATA text proposal only. If TRUE,
##'     the suggested text for Nonmem will only contain capital
##'     letters in column names.
##' @param allow.char.TIME For the $DATA text proposal only. Assume
##'     Nonmem can read TIME even if it can't be translated to
##'     numeric. This is necessary if using the 00:00 format. Default
##'     is TRUE.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @return Text for inclusion in Nonmem control stream, invisibly.
##' @details When writing csv files, the file will be
##'     comma-separated. Because Nonmem does not support quoted
##'     fields, you must avoid commas in character fields. An error is
##'     returned if commas are found in strings.
##'
##' The user is provided with text to use in Nonmem. This lists names
##' of the data columns. Once a column is reached that Nonmem will not
##' be able to read as a numeric and column is not in nm.drop, the list
##' is stopped. Only exception is TIME which is not tested for whether
##' character or not.
##' 
##' @family Nonmem
##' @export


NMwriteData <- function(data,file,write.csv=TRUE,write.rds=write.csv,
                        write.RData=FALSE,script,args.stamp,
                        args.fwrite, args.rds,args.RData,nm.drop,
                        nmdir.data,col.flag="FLAG", nm.rename,nm.copy,
                        nm.capitalize=FALSE,allow.char.TIME=TRUE,
                        quiet){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    TIME <- NULL 
    name.nm <- NULL
    comma.ok <- NULL
    include <- NULL
    name.copy <- NULL
    numeric.ok <- NULL
    occ.cum <- NULL
    name.rename <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
#### Section start: Process arguments ####

    if(missing(args.fwrite)) args.fwrite <- NULL
    args.fread <- NMdataDecideOption("args.fwrite",args.fwrite)

    if(missing(nmdir.data)) nmdir.data <- NULL
    
    stopifnot(is.data.frame(data)) 
    if(missing(file)||is.null(file)){
        file <- NULL
    } else {
#### check file name for compatibility with replacing extension
        if(!grepl("\\..+$",file)) stop("Cannot replace extension on filename. Choose a file name that ends in an extension, like \"file.csv\" or \"file.rds\".")
    }
    
    if(is.null(file)) {
        write.csv=FALSE
        write.RData=FALSE
        write.rds=FALSE
    }

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    
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
### RData arguments
    if(!missing(args.RData) && !write.RData ){
        warning("args.RData supplied, but write.RData is FALSE. RData file will not be written.")
    }
    if(missing(args.RData)) {
        args.RData <- list()
    } else {
        if(!is.list(args.RData)){
            stop("args.RData must be a list of arguments.")
        }
    }

    if(missing(nm.drop)) {
        nm.drop <- NULL
    } else {
        if(!is.null(nm.drop) && !is.character(nm.drop) ) {
            stop("If supplied, nm.drop must be of type character.")
        }
        if(any(is.na(nm.drop)|nm.drop=="")){
            stop("nm.drop cannot contain empty strings and NA's.")
        }
    }
    
###  Section end: Process arguments

    data.dt <- copy(as.data.table(data))

    ## Check if character variables contain commas
    ## This would cause trouble when writing csv    
    has.no.comma <- data.dt[,lapply(.SD,function(x){is.numeric(x)||!any(grepl(",",as.character(x)))})]

    ## only report numerics to user.
    ## Only report until first not interpretable as numeric. 
    as.num.ok <- data.dt[,lapply(.SD,NMisNumeric)]

    
    ## Allow TIME even if non-numeric. 
    if(allow.char.TIME){
        if("TIME"%in%colnames(data.dt) &&
           as.num.ok[,TIME==FALSE]) {
            as.num.ok[,TIME:=TRUE]
        }
    }
    
    dt.num.ok <- data.table(
        col=colnames(as.num.ok)
       ,numeric.ok=as.logical(as.num.ok[1])
       ,comma.ok=as.logical(has.no.comma[1])
    )

    ## if wanted, use only capital letters for column names in Nonmem
    dt.num.ok[,name.nm:=col]
    if(nm.capitalize){
        dt.num.ok[,name.nm:=toupper(name.nm)]
    }
    
    ## drop .'s from names since they are not allowed in nonmem
    ##    dt.num.ok[,name.nm:=gsub("\\.","",name.nm)]
    
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
    if(!is.null(nm.drop)){
        dt.num.ok[col%in%nm.drop,name.nm:=paste0(name.nm,"=DROP")]
        dt.num.ok[col%in%nm.drop,drop:=TRUE]
        drops.not.used <- nm.drop[!nm.drop%in%dt.num.ok[,col]]
        if(length(drops.not.used)){
            warning("Elements in nm.drop not found as columns in data:",paste(drops.not.used,collapse=", "))
        }
    }
    
    ## apply nm.copy
    if(!missing(nm.copy)){
        names.copy <- names(nm.copy)
        dt.num.ok[,name.copy:=names.copy[match(dt.num.ok[,col],nm.copy)]]
        dt.num.ok[!is.na(name.copy),name.nm:=paste0(name.copy,"=",col)]
    }

    ## apply nm.rename
    if(!missing(nm.rename)){
        names.rename <- names(nm.rename)
        dt.num.ok[,name.rename:=names.rename[match(dt.num.ok[,col],nm.rename)]]
        dt.num.ok[!is.na(name.rename),name.nm:=name.rename]
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

    if(!is.null(nmdir.data)&&!is.null(nmfile)){
        nmfile <- file.path(nmdir.data,basename(nmfile))
    } else {
        nmfile <- "<data file>"
    }

    text.nm.input <- strwrap(
        paste0("$INPUT ",paste(colnames.nm,collapse=" "))
    )
    text.nm.data <- c(paste0("$DATA ", nmfile)
                     ,paste0("IGN=@")
                      )

    if(!is.null(col.flag)&&col.flag%in%colnames.nm){
        text.nm.data <- c(text.nm.data,
                          paste0("IGNORE=(",col.flag,".NE.0)")
                          )
    }
    
    text.nm <- c(
        text.nm.input
       ,text.nm.data
    )

    files.written=c()
    if(write.csv){
        file.csv <- fnExtension(file,".csv")

        do.call(fwrite,append(list(x=data,file=file.csv),args.fwrite))
        ## fwrite      (data,na=".",quote=FALSE,row.names=FALSE,scipen=0,file=file.csv)
        files.written <- c(files.written,file.csv)
        if(doStamp){
            
            data <- do.call(NMstamp,append(list(data=data,writtenTo=file.csv),args.stamp))
            data.meta.csv <- NMinfo(data,"dataCreate")
            data.meta.csv <- data.table(parameter=names(data.meta.csv)
                                       ,value=unlist(lapply(data.meta.csv,as.character)))
            file.csv.meta <- paste0(fnExtension(file.csv,ext=""),"_meta.txt")
            fwrite(data.meta.csv,file=file.csv.meta,quote=TRUE,row.names=FALSE,sep=",")
        }
    }
    
    if(write.RData){
        name.data <- deparse(substitute(data))
        file.RData <- fnExtension(file,".RData")
        if(doStamp) data <- do.call(NMstamp,append(list(data=data,writtenTo=file.RData),args.stamp))
        assign(name.data,data)
        save(list=name.data,file=file.RData)
        do.call(save,append(list(list=name.data,file=file.RData),args.RData))
        files.written <- c(files.written,file.RData)
    }
    if(write.rds){
        file.rds <- fnExtension(file,".rds")
        if(doStamp) data <- do.call(NMstamp,append(list(data=data,writtenTo=file.rds),args.stamp))
        do.call(saveRDS,append(list(object=data,file=file.rds),args.rds))
        files.written <- c(files.written,file.rds)
    }
    written <- length(files.written)>0
    if(!quiet){
        if(written){
            message(
                paste0("Data written to file(s):\n",paste(files.written,collapse="\n"))
            )
        } else {
            message(
                paste0("Data _not_ witten to any files.")
            )

        }
        
        message("For NonMem:\n",
                paste(text.nm,collapse="\n"))
    }
    
    invisible(list(INPUT=text.nm.input,DATA=text.nm.data))

}
