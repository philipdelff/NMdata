##' Write dataset for use in Nonmem (and R)
##'
##' Instead of trying to remember the arguments to pass to write.csv,
##' use this wrapper. It tells you what to write in $DATA and $INPUT
##' in Nonmem, and it (additionally) exports an rds file as
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
##' @param write.RData In case you want to save to .RData
##'     object. Deprecated and not recommended. Use write.rds instead.
##' @param script If provided, the object will be stamped with this
##'     script name before saved to rds or RData. See ?NMstamp.
##' @param args.stamp A list of arguments to be passed to NMstamp.
##' @param args.fwrite List of arguments passed to fwrite. Notice that
##'     except for "x" and "file", you need to supply all arguments to
##'     fwrite if you use this argument. Default values can be
##'     configured using NMdataConf.
##' @param args.rds A list of arguments to be passed to saveRDS.
##' @param args.RData A list of arguments to be passed to save. Please
##'     note that writing RData is deprecated.
##' @param col.flagn Name of a numeric column with zero value for rows
##'     to include in Nonmem run, non-zero for rows to skip. The
##'     argument is only used for generating the proposed $DATA text
##'     to paste into the Nonmem control stream. To skip this feature,
##'     use col.flagn=NULL.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param genText Run and report results of NMgenText? Default is
##'     TRUE. You may want to disable this if data set is not for
##'     Nonmem.
##' @param args.NMgenText List of arguments to pass to NMgenText - the
##'     function that generates text suggestion for INPUT and DATA
##'     sections in the Nonmem control stream. You can use these
##'     arguments to get a text suggestion you an use directly in
##'     Nonmem - and NMwriteSection can even update multiple Nonmem
##'     control streams based on the result. This will update your
##'     control streams to match your new data file with just one
##'     command.
##' @param csv.trunc.as.nm If TRUE, csv file will be truncated
##'     horizontally (columns will be dropped) to match the $INPUT
##'     text generated for Nonmem (genText must be TRUE for this
##'     option to be allowed). This can be a great advantage when
##'     dealing with large datasets that can create problems in
##'     parallellization. Combined with write.rds=TRUE, the full data
##'     set will still be written to an rds file, so this can be used
##'     when combining output and input data when reading model
##'     results. This is done by default by NMscanData. This means
##'     writing a lean (narrow) csv file for Nonmem while keeping
##'     columns of non-numeric class like character and factor for
##'     post-processing.
##' @param nmdir.data Deprecated, use
##'     args.NMgenText=list(dir.data="your/path") instead.
##' @param nm.copy Deprecated, use
##'     args.NMgenText=list(copy=c(newname="existing")) instead.
##' @param nm.rename Deprecated, use
##'     args.NMgenText=list(rename=c(newname="existing")) instead.
##' @param nm.drop Deprecated, use
##'     args.NMgenText=list(drop=c("column")) instead.
##' @param nm.capitalize Deprecated, use
##'     args.NMgenText=list(capitalize=TRUE) instead.
##' @param allow.char.TIME Deprecated, use
##'     args.NMgenText=list(allow.char.TIME=TRUE) instead.
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
##' @family DataCreate
##' @export


NMwriteData <- function(data,file,write.csv=TRUE,write.rds=write.csv,
                        write.RData=FALSE,script,args.stamp,
                        args.fwrite, args.rds,args.RData,
                        quiet,args.NMgenText,csv.trunc.as.nm=FALSE,
                        genText=TRUE,
### deprecated NMgenText arguments
                        nm.drop,
                        nmdir.data,col.flagn, nm.rename,nm.copy,
                        nm.capitalize,allow.char.TIME){
    
    
#### Section start: Process arguments ####

    if(missing(args.fwrite)) args.fwrite <- NULL
    args.fwrite <- NMdataDecideOption("args.fwrite",args.fwrite)

    stopifnot(is.data.frame(data)) 
    if(missing(file)||is.null(file)){
        file <- NULL
    } else {
#### check file name for compatibility with replacing extension
        ## if(!grepl("\\..+$",file)) stop("Cannot replace extension on filename. Choose a file name that ends in an extension, like \"file.csv\" or \"file.rds\".")
    }
    
    if(is.null(file)) {
        write.csv=FALSE
        write.RData=FALSE
        write.rds=FALSE
    }
    name.data <- deparse(substitute(data))
    
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    if(missing(col.flagn)) col.flagn <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)

    
    args.text.depr <- c(
        "nm.drop",
        "nmdir.data",
        "col.flagn",
        "nm.rename",
        "nm.copy",
        "nm.capitalize",
        "allow.char.TIME")

    ## if args.NMgenText is used, the deprecated ags are not allowed
    all.args <- as.list(match.call(expand.dots=FALSE))
    if(missing(args.NMgenText)) {
        args.NMgenText <- NULL
    } else {
        if(any(args.text.depr%in%names(all.args))){
            messageWrap(paste0("Please only use args.NMgenText and not the deprecated arguments ",paste(args.text.depr,collapse=", "),". Those are only accepted if you aren't using the args.NMgenText argument yet.")
                       ,fun.msg=stop)
        }
    }
    used.args.depr <- all.args[names(all.args)%in%args.text.depr]
    if(length(used.args.depr)>0){
        names.used.ad <- names(used.args.depr)
        names.used.ad[names.used.ad=="nm.drop"] <- "drop"
        names.used.ad[names.used.ad=="nmdir.data"] <- "dir.data"
        ##    names.used.ad[names.used.ad=="col.flagn"] <- "col.flagn"
        names.used.ad[names.used.ad=="nm.rename"] <- "rename"
        names.used.ad[names.used.ad=="nm.copy"] <- "copy"
        names.used.ad[names.used.ad=="nm.capitalize"] <- "capitalize"
        ##    names.used.ad[names.used.ad=="allow.char.TIME"] <- "allow.char.TIME"
        names(used.args.depr) <- names.used.ad
        args.NMgenText <- used.args.depr
    }
### allow overwriting quiet
    if(!"quiet"%in%names(args.NMgenText))  args.NMgenText$quiet <- quiet
    if(!"col.flagn"%in%names(args.NMgenText))  args.NMgenText$col.flagn <- col.flagn
    ## do not allow setting data and file. If that is wanted, use NMgenText instead.
    if(any(c("data","file")%in%names(args.NMgenText))){
        messageWrap("data and file are not allowed in args.NMgenText. If you want to set those, use NMgenText directly instead.",fun.msg=stop)
    }

    
    
### stamp arguments
    doStamp <- TRUE
    if(missing(args.stamp)) {
        args.stamp <- list()
    } else {
        if(!is.list(args.stamp)){
            stop("args.stamp must be a list of arguments.")
        }
    }
    
    if(missing(script)||is.null(script)){
        doStamp <- FALSE
    } else {
        args.stamp$script <- script
    }
    if(!doStamp&&!is.null(NMinfo(data))){
        ## we are not stamping new info to data, but data may already have
        ## some. We don't want to inherit when and where it was saved from
        ## where it was previously read.
        data <- copy(data)
        nminfo <- NMinfoDT(data)
        try(nminfo$dataCreate <- NULL)
        writeNMinfo(data,meta=nminfo,append=FALSE)
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


### csv.trunc.as.nm
    if(csv.trunc.as.nm && !genText){
        messageWrap("when csv.trunc.as.nm==TRUE, genText must be TRUE too. Use quiet=TRUE to avoid text in console.",fun.msg=stop)
    }
    
###  Section end: Process arguments
    
    data.dt <- copy(as.data.table(data))

    ## Check if character variables contain commas
    ## This would cause trouble when writing csv
    
    has.no.comma <- data.dt[,lapply(.SD,function(x){is.numeric(x)||!any(grepl(",",as.character(x)))})]

    comma.ok=as.logical(has.no.comma[1])

    if(any(!comma.ok)){
        messageWrap(paste("You must avoid commas in data values. They will corrupt the csv file, so get rid of them before saving data. Comma found in column(s):",paste(colnames(data.dt)[comma.ok==FALSE],sep=", ")),
                    fun.msg=stop)
    }

    
    ## if any repetetions, give warning, and add numbers
    
### this is input column names.
    cnames <- colnames(data.dt)
    if(any(duplicated(cnames))) {
        warning(paste("Duplicated column name(s) in data:",
                      paste0(unique(cnames[duplicated(cnames)]),collapse=", ")
                      ))
    }

    ## Nonmem text
    NMtext <- NULL
    if(genText || (write.csv && csv.trunc.as.nm)){
        NMtext <- try(do.call(NMgenText,
                              append(
                                  list(data=data.dt,file=file)
                                 ,args.NMgenText)
                              ))
}

    files.written=c()
    if(write.csv){
        file.csv <- fnExtension(file,".csv")
        data.csv <- data
        if(csv.trunc.as.nm){
            
            string.trunc <- sub(" *\\$INPUT *","",paste(NMtext$INPUT,collapse=" "))
            n.cols.trunc <- length(strsplit(string.trunc,split=" ")[[1]])
            data.csv <- data[,1:n.cols.trunc]
        }
        do.call(fwrite,append(list(x=data.csv,file=file.csv),args.fwrite))
        ## fwrite      (data,na=".",quote=FALSE,row.names=FALSE,scipen=0,file=file.csv)
        files.written <- c(files.written,file.csv)
        if(doStamp){
            
            do.call(NMstamp,append(list(data=data,writtenTo=file.csv),args.stamp))
            data.meta.csv <- NMinfo(data,"dataCreate")
            data.meta.csv <- data.table(parameter=names(data.meta.csv)
                                       ,value=unlist(lapply(data.meta.csv,as.character,usetz=T)))
            file.csv.meta <- paste0(fnExtension(file.csv,ext=""),"_meta.txt")
            fwrite(data.meta.csv,file=file.csv.meta,quote=TRUE,row.names=FALSE,sep=",")
        }
    }
    
    if(write.RData){
        messageWrap("Writing to RData files is deprecated and this option will be removed from NMwriteData. Please use write.rds instead.")
        
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
        
    }

    if(is.null(NMtext)) return(invisible(NULL))
    
    if("try-error"%in%class(NMtext)){
        stop("NMgenText failed.") 
    } else {
        return(invisible(list(INPUT=NMtext$INPUT,DATA=NMtext$DATA)))
    }

    
}
