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
##' @param formats.write character vector of formats.write. Default is
##'     c("csv","rds"). "fst" is possible too. Default can be modified
##'     with \code{NMdataConf()}.
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
##' @param args.write_fst An optional list of arguments to be passed
##'     to write_fst.
##' @param col.flagn Name of a numeric column with zero value for rows
##'     to include in Nonmem run, non-zero for rows to skip. The
##'     argument is only used for generating the proposed $DATA text
##'     to paste into the Nonmem control stream. To skip this feature,
##'     use col.flagn=NULL.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param write.csv Write to csv file? Deprecated, use
##'     `formats.write` instead.
##' @param write.rds write an rds file? Deprecated, use
##'     `formats.write` instead.
##' @param write.RData Deprecated and not recommended - will be
##'     removed. RData is not a adequate format for a dataset (but is
##'     for environments). Please use write.rds instead.
##' @param genText Run and report results of NMgenText? Default is
##'     TRUE. You may want to disable this if data set is not for
##'     Nonmem.
##' @param save Save defined files? Default is TRUE. If a variable is
##'     used to control whether a script generates outputs (say
##'     \code{writeOutputs=TRUE/FALSE)}, if you use
##'     \code{save=writeOutputs} to comply with this.
##' @param args.NMgenText List of arguments to pass to NMgenText - the
##'     function that generates text suggestion for INPUT and DATA
##'     sections in the Nonmem control stream. You can use these
##'     arguments to get a text suggestion you an use directly in
##'     Nonmem - and \code{NMwriteSection} can even update multiple
##'     Nonmem control streams based on the result. This will update
##'     your control streams to match your new data file with just one
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
##' @importFrom data.table fwrite
##' @importFrom fst write_fst
##' @family DataCreate
##' @export


NMwriteData <- function(data,file,formats.write=c("csv","rds"),
                        script,args.stamp,
                        args.fwrite, args.rds, args.RData, args.write_fst,
                        quiet,args.NMgenText,csv.trunc.as.nm=FALSE,
                        genText=TRUE,
                        save=TRUE,
### deprecated write.xxx arguments
                        write.csv,write.rds,
                        write.RData,
### deprecated NMgenText arguments
                        nm.drop,
                        nmdir.data,col.flagn, nm.rename,nm.copy,
                        nm.capitalize,allow.char.TIME){
    
    
#### Section start: Process arguments ####

    if(missing(args.fwrite)) args.fwrite <- NULL
    args.fwrite <- NMdataDecideOption("args.fwrite",args.fwrite)
    if(missing(formats.write)) formats.write <- NULL
    formats.write <- NMdataDecideOption("formats.write",formats.write)

    
    stopifnot(is.data.frame(data)) 
    if(missing(file)||is.null(file)){
        file <- NULL
    }

    if(is.null(file) || !save) {
        formats.write <- c()
        write.csv=FALSE
        write.RData=FALSE
        write.rds=FALSE
    }
    ## args <- getArgs()
    
    args <- getArgs(sys.call(),parent.frame())
    
    args.write.depr <- c("write.rds","write.csv","write.RData")
    if(any(args.write.depr %in% names(args))) {
        message("arguments in the format write.xxx are deprecated. Use the `formats.write` argument instead. Example: formats.write=c(\"csv\",\"rds\")")
        if(missing(write.csv)) write.csv <- NULL
        if(missing(write.rds)) write.rds <- NULL
        if(missing(write.RData)) write.RData <- NULL

        if(!is.null(write.csv)){
            if(write.csv) {
                formats.write <- c(formats.write,"csv")
            } else {
                formats.write <- setdiff(formats.write,"csv")
            }
        }
        if(!is.null(write.rds)){
            if(write.rds) {
                formats.write <- c(formats.write,"rds")
            } else {
                formats.write <- setdiff(formats.write,"rds")
            }
        }
        if(!is.null(write.RData)){
            if(write.RData) {
                formats.write <- c(formats.write,"RData")
            } else {
                formats.write <- setdiff(formats.write,"RData")
            }
        }
    }
    formats.write <- unique(tolower(gsub(" ","",formats.write)))
    write.rds <- "rds" %in% formats.write
    write.csv <- "csv" %in% formats.write
    write.RData <- "rdata" %in% formats.write
    write.fst <- "fst" %in% formats.write
    
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
    ## all.args <- as.list(match.call(expand.dots=FALSE))
    ## These were deprecated way before 2023-06-13
    ## all.args <- getArgs()
    if(missing(args.NMgenText)) {
        args.NMgenText <- NULL
    } else {
        if(any(args.text.depr%in%names(args))){
            messageWrap(paste0("Please only use args.NMgenText and not the deprecated arguments ",paste(args.text.depr,collapse=", "),". Those are only accepted if you aren't using the args.NMgenText argument yet.")
                       ,fun.msg=stop)
        }
    }
    used.args.depr <- args[names(args)%in%args.text.depr]
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
        warning("args.rds supplied, but rds is not a requested format. rds file will not be written.")
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
        warning("args.RData supplied, but RData is not a requested format. RData file will not be written.")
    }
    if(missing(args.RData)) {
        args.RData <- list()
    } else {
        if(!is.list(args.RData)){
            stop("args.RData must be a list of arguments.")
        }
    }

### fst arguments
    ## if("fst"%in%formats.write){
    ## if (!requireNamespace("fst", quietly = TRUE)) {
    ##     stop("fst package could not be loaded. Either install fst or drop fst from formats.write.")
    ## }
    ## }
    if(missing(args.write_fst)) {
        args.write_fst <- list()
    } else {
        if(!is.list(args.write_fst)){
            stop("args.write_fst must be a list of arguments.")
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
    if(write.csv){    
        has.no.comma <- data.dt[,lapply(.SD,function(x){is.numeric(x)||!any(grepl(",",as.character(x)))})]
        comma.ok <- as.logical(has.no.comma[1])

        if(any(!comma.ok)){

            messageWrap(paste("When writing csv, You must avoid commas in data values. They will corrupt the csv file. Either 1) avoid csv using `formats.write=\"rds\"` or 2) drop columns containing commas or 3) remove or replace commas in data values before saving data. For 3) see `?editCharCols`.\nComma found in column(s):",paste(colnames(data.dt)[comma.ok==FALSE],sep=", ")),
                        fun.msg=stop)
        }
        
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


### create stamp if needed
    if(doStamp){
        do.call(NMstamp,append(list(data=data,writtenTo=fnExtension(file,formats.write)),args.stamp))

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

        files.written <- c(files.written,file.csv)
    }

    if(write.RData){
        messageWrap("Writing to RData files is deprecated and this option will be removed from NMwriteData. Please use rds instead.")
        
        file.RData <- fnExtension(file,".RData")
        ## if(doStamp) data <- do.call(NMstamp,append(list(data=data,writtenTo=file.RData),args.stamp))
        
        assign(name.data,data)
### explicitly doing base::save because otherwise do.call will
### find the save variable controling whether to save or not.
        do.call(base::save,append(list(list=name.data,file=file.RData),args.RData))
        files.written <- c(files.written,file.RData)
    }
    if(write.rds){
        file.rds <- fnExtension(file,".rds")
        ## if(doStamp) data <- do.call(NMstamp,append(list(data=data,writtenTo=file.rds),args.stamp))
        do.call(saveRDS,append(list(object=data,file=file.rds),args.rds))
        files.written <- c(files.written,file.rds)
    }
    if("fst"%in%formats.write){

        
        file.fst <- fnExtension(file,".fst")
        ##     if(doStamp) data <- do.call(NMstamp,append(list(data=data,writtenTo=file.fst),args.stamp))
        do.call(write_fst,append(list(x=data,path=file.fst),args.write_fst))
        files.written <- c(files.written,file.fst)
    }
    
    ## write meta data for csv and fst
    if(doStamp && any(c("fst","csv")%in%formats.write)){
        
        data.meta <- NMinfo(data,"dataCreate")
        data.meta <- data.table(parameter=names(data.meta)
                               ,value=unlist(lapply(data.meta,function(x){
                                   paste(as.character(x,usetz=T),collapse=" ")
                               }))
                               )
        file.meta <- fnAppend(fnExtension(file,".txt"),"meta")
        fwrite(data.meta,file=file.meta,quote=TRUE,row.names=FALSE,sep=",")
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
