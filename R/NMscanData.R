##' Automatically find Nonmem input and output tables and organize data
##'
##' This is a very general solution to automatically identifying, reading, and  merging all output and input data in a Nonmem model. The most important
##' steps are
##' \itemize{
##'  \item{Read and combine output tables,}
##'  \item{If wanted, read input data and restore variables that were not output from the Nonmem model}
##'  \item{If wanted, also restore rows from input data that were disregarded in
##' Nonmem (e.g. observations or subjects that are not part of the analysis)}
##' }
##' 
##' @param file A Nonmem control stream or output file from Nonmem
##'     (.mod or .lst)
##' @param file.mod The input control stream. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. The default behavior can be configured using
##'     NMdataConf. See dir.data too.
##' @param col.id The name of the subject ID variable, default is
##'     "ID".
##' @param use.input Should the input data be added to the output
##'     data. Only column names that are not found in output data will
##'     be retrieved from the input data. Default is TRUE which can be
##'     modified using NMdataConf. See merge.by.row too.
##' @param merge.by.row If use.input=TRUE, this argument determines
##'     the method by which the input data is added to output
##'     data. The default method (merge.by.row=FALSE) is to interpret
##'     the Nonmem code to imitate the data filtering (IGNORE and
##'     ACCEPT statements), but the recommended method is
##'     merge.by.row=TRUE which means that data will be merged by a
##'     unique row identifier. The row identifier must be present in
##'     input and at least one full length output data table. See
##'     argument col.row too.
##' @param col.row A column with a unique value for each row. Such a
##'     column is recommended to use if possible. See merge.by.row and
##'     details as well. Default ("ROW") can be modified using
##'     NMdataConf.
##' @param recover.rows Include rows from input data files that do not
##'     exist in output tables? This will be added to the $row dataset
##'     only, and $run, $id, and $occ datasets are created before this
##'     is taken into account. A column called nmout will be TRUE when
##'     the row was found in output tables, and FALSE when
##'     not. Default is FALSE and can be configured using NMdataConf.
##' @param col.model A column of this name containing the model name
##'     will be included in the returned data. The default is to store
##'     this in a column called "model". See argument "modelname" as
##'     well. Set to NULL if not wanted. Default can be configured
##'     using NMdataConf.
##' @param col.nmout A column of this name will be a logical
##'     representing whether row was in output table or not. Default
##'     can be modified using NMdataConf.
##' @param modelname The model name to be stored if col.model is not
##'     NULL. If not supplied, the name will be taken from the control
##'     stream file name by omitting the directory/path and deleting
##'     the .lst extension (path/run001.lst becomes run001). This can
##'     be a character string or a function which is called on the
##'     value of file (file is another argument to NMscanData). The
##'     function must take one character argument and return another
##'     character string. As example, see NMdataConf()$modelname. The
##'     default can be configured using NMdataConf.
##' @param use.rds If an rds file is found with the exact same name
##'     (except for .rds instead of say .csv) as the input data file
##'     mentioned in the Nonmem control stream, should this be used
##'     instead? The default is yes, and NMwriteData will create this
##'     by default too. Default can be configured using NMdataConf.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output control stream, use dir.data to
##'     tell in which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param file.data Specification of the data file path. When this is
##'     used, the control streams are not used at all.
##' @param translate.input Default is TRUE, meaning that input data
##'     column names are translated according to $INPUT section in
##'     Nonmem listing file.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param args.fread List of arguments passed to when reading _input_
##'     data. Notice that except for "input" and "file", you need to
##'     supply all arguments to fread if you use this
##'     argument. Default values can be configured using NMdataConf.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param rep.count Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. Especially for
##'     NMscanData output it can be meaningless because multiple
##'     tables can be combined so this information is not unique
##'     across those source tables. However, if rep.count is TRUE (not
##'     default), this will be carried forward and added as a column
##'     called NMREP. The argument is passed to NMscanTables.
##' @param order.columns If TRUE (default), NMorderColumns is used to
##'     reorder the columns before returning the data. NMorderColumns
##'     will be called with alpha=FALSE, so columns are not sorted
##'     alphabetically. But standard Nonmem columns like ID, TIME, and
##'     other will be first. If col.row is used, this will be passed
##'     to NMorderColumns too.
##' @param check.time If TRUE (default) and if input data is used,
##'     input control stream and input data are checked to be newer
##'     than output control stream and output tables. These are
##'     important assumptions for the way information is merged by
##'     NMscanData. However, if data has been transferred from another
##'     system where Nonmem was run, these checks may not make sense,
##'     and you may not want to see these warnings. The default can be
##'     configured using NMdataConf. For the output control stream,
##'     the time stamp recorded by Nonmem is used if possible, and if
##'     the input data is created with NMwriteData, the recorded
##'     creation time is used if possible. If not, and for all other
##'     files, the file modification times are used.
##' @param tz.lst If supplied, the timezone to be used when reading
##'     the time stamp in the output control stream. Please supply
##'     something listed in OlsonNames(). Can be configured using
##'     NMdataConf() too.
##' @param tab.count Deprecated. Use rep.count.
##'
##' @details This function makes it very easy to collect the data from
##'     a Nonmem run.
##'
##' A useful feature of this function is that it can automatically
##' combine "input" data (the data read by Nonmem in $INPUT or
##' $INFILE) with "output" data (tables written by Nonmem in
##' $TABLE). There are two implemented methods for doing so. One (the
##' default but not recommended) relies on interpretation of filter
##' (IGNORE and ACCEPT) statements in $INPUT. This will work in most
##' cases, and checks for consistency with Nonmem results. However,
##' the recommended method is using a unique row identifier in both
##' input data and at least one output data file (not a FIRSTONLY or
##' LASTONLY table). Supply the name of this column using the col.row
##' argument.
##'
##' Limitations. A number of Nonmem features are not supported. Most
##' of this can be overcome by using merge.by.row=TRUE. Incomplete
##' list of known limitations:
##'
##' \itemize{
##'  \item{character TIME}{If Nonmem is used to translate DAY and a character TIME column, TIME has to be available in an output table. NMscanData does not do the translation to numeric.}
##'  \item{RECORDS}{The RECORDS option to limit the part of the input data being used is not searched for. Using merge.by.row=TRUE will work unaffectedly.}
##'  \item{NULL}{The NULL argument to specify missing value string in input data is not respected. If delimited input data is read (as opposed to rds files), missing values are assumed to be represented by dots (.).}
##' }
##' @examples
##' res1 <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"))
##' @return A data set of class 'NMdata'.
##' @family DataRead
##' @import data.table
##' @export


NMscanData <- function(file, col.row, use.input, merge.by.row,
                       recover.rows,file.mod,dir.data,file.data,
                       translate.input=TRUE, quiet, use.rds,
                       args.fread, as.fun, col.id="ID", modelname,
                       col.model, col.nmout,rep.count,
                       order.columns=TRUE, check.time, tz.lst,
                       tab.count) {

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    . <- NULL
    COLNUM <- NULL
    DV <- NULL
    ID.jump <- NULL
    N <- NULL
    NMREP <- NULL
    ## firstlastonly <- NULL
    ## firstonly <- NULL
    ## lastonly <- NULL
    dup <- NULL
    full.length <- NULL
    has.col.row <- NULL
    included <- NULL
    level <- NULL
    maxLength <- NULL
    name <- NULL
    nid <- NULL
    nmout <- NULL
    nonmem <- NULL
    result <- NULL
    scope <- NULL
    type <- NULL
    var <- NULL
    variable <- NULL

    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    
#### Section start: Process arguments  ####

    file <- filePathSimple(file)
    if(!file.exists(file)) messageWrap(paste0("Model file ",file," does not exist."),fun.msg=stop)
    dir <- dirname(file)

    
    
    ## for easier passing of the argument
    if(missing(dir.data)) dir.data <- NULL
    if(missing(file.mod)) file.mod <- NULL
    if(missing(check.time)) check.time <- NULL
    if(missing(tz.lst)) tz.lst <- NULL
    if(missing(as.fun)) as.fun <- NULL
    if(missing(quiet)) quiet <- NULL
    if(missing(use.rds)) use.rds <- NULL
    if(missing(args.fread)) args.fread <- NULL

    check.time <- NMdataDecideOption("check.time",check.time)
    tz.lst <- NMdataDecideOption("tz.lst",tz.lst)
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    modelname <- NMdataDecideOption("modelname",modelname)
    if(missing(recover.rows)) recover.rows <- NULL
    recover.rows <- NMdataDecideOption("recover.rows",recover.rows)
    quiet <- NMdataDecideOption("quiet",quiet)
    use.rds <- NMdataDecideOption("use.rds",use.rds)
    args.fread <- NMdataDecideOption("args.fread",args.fread)
    ## if null, rep.count will later be set to TRUE if NMREP varies
    if(missing(rep.count)) rep.count <- NULL
    if(!missing(tab.count)) .Deprecated("rep.count",old="tab.count")
    if(!is.null(rep.count)&&!missing(tab.count)) stop("Use rep.count, not tab.count.")
    if(!missing(tab.count)) rep.count <- tab.count
    
    runname <- modelname(file)
    ## file.mod is treated later if we need the input control stream
    
### combination of arguments
    if(!is.null(dir.data)&&!is.null(file.mod)){
        messageWrap("Only use one of dir.data and file.mod. The aim is to find the input data file, so either give the directory (dir.data) in which it is, and the filename will be taken from the lst file, or help finding the .mod file using the file.mod argument. Using both is redundant.",fun.msg=stop)
    }

### specification of merging method
    if(missing(use.input)) use.input <- NULL
    use.input <- NMdataDecideOption("use.input",use.input)

    ## a few different ways are allowed for NULL - missing, NA, "" all do the same
    if(missing(col.row)||(!is.null(col.row)&&is.na(col.row))||(is.character(col.row)&&all(col.row==""))) {
        col.row <- NULL
    }
    col.row <- NMdataDecideOption("col.row",col.row)
    col.row.merge <- col.row
    
    if(missing(merge.by.row)) merge.by.row <- NULL
    if(missing(file.data)) file.data <- NMdataDecideOption("file.data",file.data)

    if(!is.null(merge.by.row)&&isTRUE(merge.by.row)&&!use.input){
        stop("merge.by.row cannot be TRUE when use.input is FALSE.")
    }
    
    merge.by.row.arg <- merge.by.row
    merge.by.row <- NMdataDecideOption("merge.by.row",merge.by.row)

    ## For now, searching for a row identifier is disabled. This may belong in a separate function. 
    search.col.row <- FALSE
### notice, this can't be evaluated if merge.by.row=="ifAvailable"
    if(is.null(merge.by.row.arg) && is.character(merge.by.row) && merge.by.row=="ifAvailable"){
        search.col.row <- TRUE
    }

### merging method found
### now code must use search.col.row, cbind.by.filters and merge.by.row

###  Section end: Process arguments 
    

#### Section start: read all output tables and add to meta data ####
    
    ## tables <- NMscanTables(file,quiet=TRUE,as.fun="data.table",col.row=col.row,col.id=col.id,rep.count=rep.count)
    tables <- NMscanTables(file,quiet=TRUE,as.fun="data.table",col.row=col.row,col.id=col.id,rep.count=TRUE)
    meta.output <- copy(NMinfoDT(tables)$tables)

    
### combine full tables into one
    col.row.in.output <- meta.output[level=="row",any(has.col.row)]
    
    if(use.input && is.logical(merge.by.row) && merge.by.row) {
        if(!col.row.in.output) {
            messageWrap("Only output data will be returned. Output cannot be merged with input. col.row not found in any full-length (not firstonly) output tables. To include input data, add col.row to the tables, or disable merge.by.row.",fun.msg=warning)
        }
    }


### Section end: read all output tables and add to meta data


#### Section start: read input data ####
    file.info.mod <- NULL
    if(use.input && is.null(dir.data)) {
        
        file.mod <- NMdataDecideOption("file.mod",file.mod)
        file.mod <- file.mod(file)
        if(file.exists(file.mod)) file.info.mod <- file.info(file.mod)

        if(!file.exists(file.mod)) {
            
            messageWrap("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",
                        fun.msg=warning)
            use.input <- FALSE }
    }

    nminfo.input <- NULL
    if(use.input){
        
        data.input.full <- NMscanInput(file
                                      ,file.mod=file.mod
                                      ,dir.data=dir.data
                                      ,file.data=file.data
                                      ,quiet=TRUE
                                      ,translate=translate.input
                                      ,use.rds=use.rds
                                      ,applyFilters=FALSE
                                      ,args.fread=args.fread
                                      ,as.fun="data.table"
                                      ,col.id=col.id
                                      ,details=TRUE)
        
        nminfo.input <- NMinfoDT(data.input.full)
        
    }

### Section end: read input data

#### Section start: col.nmout and col.model ####
    cnames.input.result <- nminfo.input$colnames[,result]
    outnames  <- unlist(lapply(tables,colnames))
    allnames <- c(outnames,cnames.input.result)

    if(missing(col.model)||!is.null(col.model)) {
        if(missing(col.model)) {
            col.model <- NULL
        } 
        col.model <- NMdataDecideOption("col.model",col.model)
    }
    
    
    if(!is.null(col.model) && col.model%in%allnames){
        messageWrap(paste0("column",col.model," (value of col.model) existed and was overwritten. To avoid this, use argument col.model. To skip, use col.model=NULL."),fun.msg=warning)
    }

    ## col.nmout
    if(missing(col.nmout)) col.nmout <- NULL
    col.nmout <- NMdataDecideOption("col.nmout",col.nmout)
    
    use.nmout <- TRUE
    if(is.null(col.nmout)) {
        
        col.nmout <- tmpcol(names=allnames,base="nmout",prefer.plain=TRUE)
        if(recover.rows){
            messageWrap(paste0("col.nmout is NULL, but this is not allowed for recover.rows=TRUE. col.nmout is set to ",col.nmout),fun.msg=warning)
        } else {
            use.nmout <- FALSE
        }
    }
    if(col.nmout%in%allnames){
        messageWrap(paste0("column",col.nmout," found in existing data and is overwritten. To avoid, use col.nmout argument.",fun.msg=warning))
    }

### Section end: col.nmout and col.model
    

#### Section start:  merge output to max one idlevel and max one row ####
    
    
    object.tables <- reduceTables(tables,col.nmout=col.nmout)
    tab.row <- object.tables$tab.row
    tab.idlevel <- object.tables$tab.idlevel
    dt.vars <- object.tables$dt.vars
    dt.vars.id <- object.tables$dt.vars.id
    
    
    if(is.null(rep.count)&&!is.null(tab.row)) rep.count <- tab.row[,uniqueN(NMREP)>1]
    
    ## use.rows means if to use row-data from output tables
    use.rows <- TRUE
    if(!any(meta.output$level=="row")) {
        use.rows <- FALSE
        col.row.in.output <- meta.output[full.length==FALSE,any(has.col.row)]
    }

###  Section end:  merge to max one idlevel and max one row
    
    

#### Section start: Check file modification times ####
    if(is.null(tz.lst)){
        logtime.lst <- NA
    } else {
        logtime.lst <- lstExtractTime(file,tz.lst)
    }
    mtime.lst <- file.mtime(file)
    time.method.lst <- NA
    time.method.inp <- NA
    time.ok <- "Not checked"
    if(use.input){
        logtime.inp <- max(nminfo.input$tables$file.logtime)
        mtime.inp <- max(nminfo.input$tables$file.mtime)
    }

    if(check.time){
        time.method.lst <- "log"
        testtime.lst <- logtime.lst
        ## time.method.lst <- "mtime"
        ## testtime.lst <- mtime.lst
        if(is.na(logtime.lst)){
            testtime.lst <- mtime.lst
            time.method.lst <- "mtime"
        }
        
        time.ok <- c()
        if(!is.null(file.mod) &&
           file.exists(file.mod) &&
           filePathSimple(file.mod)!=filePathSimple(file)) {
            mtime.mod <- file.info.mod$mtime
            
            if(mtime.mod>testtime.lst){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output control stream (",file,"). Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"mod > lst")
            }
            if(mtime.mod>min(meta.output[,file.mtime])){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output tables. Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"mod > output")
            }
        }
        
        if(use.input) {
            
            time.method.inp <- "log"
            testtime.inp <- logtime.inp
            if(is.na(logtime.inp)){
                testtime.inp <- mtime.inp
                time.method.inp <- "mtime"
            }

            
            if(testtime.inp > testtime.lst){
                messageWrap(paste0("input data (",nminfo.input$tables$file,") is newer than output control stream (",file,") Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"input > lst")
            }
            if(testtime.inp > min(meta.output[,file.mtime])){
                messageWrap(paste0("input data file (",nminfo.input$tables$file,") is newer than output tables. Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"input > output")
            }
        }
        
        time.ok <-
            if(length(time.ok)>0) {
                paste("Warning(s):", paste(time.ok,collapse=", "))
            } else {
                "All OK"
            }
        
    }

### Section end: Check file modification times


#### Section start: Determining whether we are merging or not ####
    cbind.by.filters <- FALSE
    if(use.input){   
        cnames.input.nonmem  <- nminfo.input$input.colnames[,nonmem]
        col.row.in.input <- !is.null(col.row) && col.row %in% cnames.input.nonmem 
### in case merge.by.row=="ifAvailable", we need to check if
### col.row is avilable in both input and output
        if(merge.by.row=="ifAvailable"){
            merge.by.row <- col.row.in.input && col.row.in.output
        }
        cbind.by.filters <- !merge.by.row

        ## if cbind.by.filters, we have to filter input data now.
        if(!cbind.by.filters){
            data.input <- data.input.full
        }
        if(cbind.by.filters){
            ## add counter to merge by
            
            col.row.merge <- tmpcol(names=unique(c(colnames(tab.row),colnames(data.input.full))),base="col.row")
            
            data.input.full[,(col.row.merge):=.I]
### we need to know what input rows passed the filters. It's not enough to filter and end up with a subset of data. This should be done above where we for now filter the data. Add the row counter up there. Apply filter, then cbind the rows counter column to output. In case !recover.rows, only keep filtered input.
            data.input <- NMapplyFilters(data.input.full,file=file,as.fun="data.table",quiet=TRUE)
### Even though we include all rows from input, the filters have been
### used to merge with output data, so we attach the filters to
### NMinfo.
            writeNMinfo(data.input,meta=NMinfoDT(data.input)["input.filters"],append=TRUE)
            
### here we need to check that we got the correct number of lines
            nrow.data.input <- nrow(data.input)
            
#### This needs to be by NMREP. Would be good to have this info in tables metadata
            nrow.tab.row <- 0
            if(!is.null(tab.row)){
                nrow.tab.row <- tab.row[,.N,by=(NMREP)][,unique(N)]
                
                if( nrow.data.input!=nrow.tab.row) {
### we have a tab.row and the number of rows doesn't match what's found in input.
                    messageWrap(sprintf("After applying filters to input data, the resulting number of rows (%d) differs from the number of rows in output data (%d). Please check that input data hasn't changed since Nonmem was run, and that $INPUT section matches columns in input data. Also, NMdata may not be able to interpret your IGNORE/ACCEPT statements correctly (see ?NMapplyFilters). Please consider including a unique row identifier in both input and output data if possible. Another reason for the error could be that the model is a simulation that returns a multiple of the input events in output data.",nrow.data.input,nrow.tab.row),fun.msg=stop)

                }

                
                tab.row[,(col.row.merge):=data.input[,get(col.row.merge)],by=.(NMREP)]
                ## if(!recover.rows) data.input <- data.input

            }


        }
###  Section end: Determining whether we are merging or not

        if(merge.by.row){
            search.col.row <- FALSE
        }

        
        if(use.input&&!any(meta.output$full.length)) {
            ## copying so we can modify tab.row        
            tab.row <- copy(data.input)
            

            dt.vars <- rbind(dt.vars,
                             data.table(
                                 variable=colnames(tab.row)
                                ,file=nminfo.input$tables[,name]
                                ,included=TRUE
                                ,source="input"
                                ,level="row")
                             )
            
            tab.row[,(col.nmout):=FALSE]
            dt.vars <- rbind(dt.vars,
                             data.table(variable=col.nmout
                                       ,file=NA_character_
                                       ,included=TRUE 
                                       ,source="NMscanData"
                                       ,level="row"
                                        ))
        }

        if(use.input&&any(meta.output$full.length)) {
            if(recover.rows && tab.row[,uniqueN(NMREP)]>1) messageWrap("Output tables seem repeated (is this a simulation with multiple subproblems?). recover.rows=TRUE is not supported in this case.",fun.msg=stop)
            ## if(!quiet) messageWrap("Searching for input data.")

            ## if no method is specified, search for possible col.row to help the user
            if(search.col.row){
                msg0 <- searchColRow(file,file.mod=file.mod,dir.data,file.data,translate.input,use.rds,args.fread,col.id,tab.row)
                msg <- paste0(msg0,"\n",
                              "To skip this check, please use merge.by.row=TRUE or merge.by.row=FALSE.")
                messageWrap(msg,fun.msg=message)
                cat("\n")

            }
            
        }

        
#### Section start: Checks of col.row if we are merging by it ####
        if(merge.by.row){
### merging by col.row

            ## checking for available value and for whether it's being modified in Nonmem
            checkColRow(col.row,file)


            ##            col.row.in.input <- FALSE
            ## Has this check already been done?
            
            if(col.row.in.input) {
                if(data.input[,any(duplicated(get(col.row)))]) {
                    messageWrap("use.input=TRUE and merge.by.row=TRUE. Hence, input data and output data must be merged by a unique row identifier (col.row), but col.row has duplicate values in _input_ data. col.row must be a unique row identifier when use.input=TRUE and merge.by.row=TRUE.",fun.msg=stop)
                }
                ##              col.row.in.input <- TRUE
            } else {
                warning("merge.by.row is TRUE, but col.row not found in _input_ data. Only output data used.")
                use.input <- FALSE
            }
            
            col.row.in.output <- FALSE
            if(col.row%in%colnames(tab.row)) {
                
                col.by <- NULL
                if("NMREP"%in%colnames(tab.row)) col.by <- "NMREP"
                found.dups <- tab.row[,.(dup=duplicated(get(col.row))),by=col.by][,any(dup)]
                if( found.dups ) {
                    messageWrap("merge.by.row is TRUE, but col.row has duplicate values (within NMREP) in _output_ data. col.row must be a unique row identifier. It is unique in input data, so how did rows get repeated in output data? Has input data been edited since the model was run?",fun.msg=stop)
                    ## warning("skipping a check")
                }
                col.row.in.output <- TRUE
            } else {
                warning("merge.by.row is TRUE, but col.row not found in _output_ data. Only output data used.")
                use.input <- FALSE
            }
            if(use.input && col.row.in.input && col.row.in.output ){
                ## check that we are not getting new values of
                ## col.row from input to output.
                if(!all( tab.row[,get(col.row)] %in% data.input[,get(col.row)])){
                    
                    messageWrap("values of unique row identifier found in output data that are not present in input data. Please use another row identifier or don't use any (not recommended).",fun.msg=stop)
                }
            }
        }
    }

###  Section end: Checks of col.row if we are merging by it

    
    
    


### if recover, mergeCheck(input,output,all.x=T)
### else, mergeCheck(output,input,all.x=T)

#### before implementation of tableno
    ## tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=TRUE,as.fun="data.table",quiet=TRUE)

    ## after implementation of tableno
    if(use.input & any(meta.output$full.length)){
        cols.exist <- copy(colnames(tab.row))
        
        ## if(recover.rows){
        ##     cols.exist <- copy(colnames(tab.row))
        
        ##     tab.row <- mergeCheck(data.input[,c(col.row.merge,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],tab.row,by=col.row.merge,all.x=TRUE,as.fun="data.table",quiet=TRUE)
        ##     setcolorder(tab.row,cols.exist)
        ## } else {
        tab.row <- mergeCheck(tab.row,data.input[,c(col.row.merge,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row.merge,all.x=TRUE,as.fun="data.table",quiet=TRUE)
        ## }
        tab.row[is.na(get(col.nmout)),(col.nmout):=FALSE]

        
        dt.vars1 <- data.table(
            variable=colnames(data.input)
           ,file=nminfo.input$tables[,name]
           ,source="input"
           ,level="row"
        )
        
        dt.vars1[,
                 included:=variable%in%setdiff(colnames(data.input),cols.exist)
                 ]

        dt.vars <- rbind(dt.vars,dt.vars1)
    }
    
    
###  Section end: handle input data


#### Section start: Add idlevel data ####
    ## if merge.by.row==TRUE, col.row is the prefered col to merge by. col.row or col.id must be present.

    ## If cbind.by.filters, we merge by col.id.

    ## col.row is only acceptable to
    ## merge by if merge.by.row==TRUE

    skip.idlevel <- is.null(tab.idlevel)
    if(!skip.idlevel) {
        
        ## If we use input or row-level output, we will not use DV from idlevel
        if( (use.input || use.rows) && "DV"%in%colnames(tab.idlevel)){
            tab.idlevel[,DV:=NULL]
        }
    }
    
    if(!skip.idlevel && use.rows) {
        ## preparing merge of idlevel onto row level
        
        cols.merge.idlevel <- col.id
        if(merge.by.row=="ifAvailable") {
            if(col.row.merge %in% colnames(tab.idlevel) && col.row.merge %in% colnames(tab.row)){
                merge.by.row <- TRUE
            } else {
                merge.by.row <- FALSE
            }
        }
        if(merge.by.row){
            cols.merge.idlevel <- c(col.id,col.row.merge)
        }
        if(merge.by.row) cols.merge.idlevel <- c(col.id,col.row.merge)
        
        cols.common.row.id <- intersect(intersect(colnames(tab.row),colnames(tab.idlevel)),cols.merge.idlevel)
        
        if(!length (cols.common.row.id)){
            ## if(!any(cols.merge.idlevel%in%cols.common.row.id)){
            messageWrap(paste0("subject-level output data (i.e. firstonly or lastonly) cannot be combined with other data. merge.by.row=",merge.by.row,". To include subject-level output: If merge.by.row=FALSE, col.id must be in subject-level output data. If merge.by.row=TRUE, col.id or col.row must be in subject-level output data."),fun.msg=warning)
            skip.idlevel <- TRUE
        }
    }

    ## if col.row is in cols.merge.row.id, merge by col.row only.

    ## We want everything that is not in output row-data. We want
    ## it even if in input data. But give a warning if it varies
    ## in input.
    if(!skip.idlevel) {
        
        if(!use.input && !use.rows) {
            ## The very special case where we don't use input and
            ## there is no row-level data.
            
            ## there's nothing else - so just return idlevel data
            tab.row <- tab.idlevel

            dt.vars <- rbind(dt.vars,dt.vars.id)
            tab.row[,(col.nmout):=TRUE]
            
        } else {
            ## there is row-level data to combine with
            id.cols.not.new <- col.id
            if(merge.by.row && all(col.row%in%colnames(tab.idlevel))){
                ## fetch new ID column, merging by col.row. Then we will merge by
                ## ID.
                if(col.id%in%colnames(tab.idlevel)){
                    tab.idlevel[,(col.id):=NULL]
                }
                
                tab.idlevel <- mergeCheck(tab.idlevel,unique(tab.row[,c(col.row.merge,col.id),with=FALSE]),by=col.row.merge,quiet=TRUE)
                tab.idlevel[,(col.row.merge):=NULL]
                id.cols.not.new <- c(col.row.merge,col.id)
                
                meta.output[level=="id",nid:=tab.idlevel[,uniqueN(get(col.id))]]
            }
            ## For now, we don't support disjoint ID's in combination with idlevel tables. 
            if(col.id%in%colnames(tab.idlevel)) {
                idjumps <- tab.idlevel[,.(ID.jump=c(0,diff(.I))),by=col.id]
                if(any(idjumps[,ID.jump>1])){
                    messageWrap("col.id is disjoint. For the moment, this is not supported in combination with ID-level output (firstonly and lastonly). ID-level tables will be skipped.",fun.msg=warning)
                    skip.idlevel <- TRUE
                }
            }
            
            ## use tab.vars for the subset
            if(!skip.idlevel){
                
                cols.to.use <- unique(c(col.id,setdiff(colnames(tab.idlevel),dt.vars[included==TRUE,variable])))
                tab.idlevel.merge <- tab.idlevel[,cols.to.use,with=FALSE]
                tab.row <- mergeCheck(tab.row,tab.idlevel.merge,by=col.id,as.fun="data.table",quiet=TRUE)
                ## repeating in case there was no full-length tables
                if(is.null(rep.count)&&!is.null(tab.row)) rep.count <- tab.row[,uniqueN(NMREP)>1]           
                dt.vars.id[,included:=FALSE]
                dt.vars.id[variable%in%setdiff(cols.to.use,id.cols.not.new),included:=TRUE]
                dt.vars.id[included==TRUE,included:=!duplicated(variable)]
                
                dt.vars <- rbind(dt.vars,dt.vars.id)
            }
        }
    }

    

### Section end: Add idlevel data

    if(!use.rows && skip.idlevel) {
        messageWrap("No output data could be used. If enabled, try disabling use.input.",fun.msg=stop)
    }


#### Section start: Recover rows ####

    if( use.input && recover.rows ) {
        skip.recover <- FALSE
        data.recover <- data.input.full[!get(col.row.merge)%in%tab.row[,get(col.row.merge)]]
        data.recover[,(col.nmout):=FALSE]
        tab.row <- rbind(tab.row,data.recover,fill=TRUE)
        setorderv(tab.row,col.row.merge)
    }

###  Section end: Recover rows
    
### clean up col.row if cbind.by.filters
    if(cbind.by.filters){
        tab.row[,(col.row.merge):=NULL]
        dt.vars <- dt.vars[variable!=col.row.merge]
    }
    
### clean up NMREP if not wanted
    if(!is.null(rep.count) && !rep.count){
        tab.row[,NMREP:=NULL]
        dt.vars <- dt.vars[variable!="NMREP"]
    }

    
#### Section start: Format output ####

    ## add column with model name
    if(!is.null(col.model)) {
        tab.row[,c(col.model):=runname]
        
        dt.vars <- rbind(dt.vars,
                         data.table(variable=col.model
                                   ,file=NA_character_
                                   ,included=TRUE 
                                   ,source="NMscanData"
                                   ,level="model"
                                    ))
    }


### order columns in returned data
    if(order.columns){
        tab.row <- NMorderColumns(tab.row, col.row=col.row, as.fun="data.table",
                                  last=c(col.model,col.nmout),
                                  alpha=FALSE, quiet=TRUE)
    }

    if(!use.nmout){
        tab.row[,(col.nmout):=NULL]
    }

### order columns in variable table accordingly.
    
    dt.vars[included==TRUE,COLNUM:=match(variable,colnames(tab.row))]
    setorder(dt.vars,-included,COLNUM)
    dt.vars[,included:=NULL]    

    meta.output[,source:="output"]
    if(use.input){
        ## data.input$meta$tables[,source:="input"]
        tables.meta <- rbind(meta.output,nminfo.input$tables,fill=TRUE)
    } else {
        tables.meta <- meta.output
    }
    setcolorder(tables.meta,c("source","name","nrow","ncol"))


    details <- list(
        ## name of model
        model=runname,
        ## call
        call=deparse(sys.call()),
        ## time of NMscanData call
        time.NMscanData=Sys.time(),
        ## path to lst
        file.lst=file,
        ## path to mod
        file.mod=file.mod,
        time.ok=time.ok,
        dir.data=dir.data,
        ## was input used?
        input.used=use.input,
        ## was input recovered?
        rows.recovered=recover.rows,
        ## input and output merged? (or cbind after filters?)
        merge.by.row=merge.by.row,
        col.row=col.row,
        ## if available: path to input data
        file.input=NA_character_,
        ## file info on lst
        logtime.lst=logtime.lst,
        mtime.lst=mtime.lst,
        method.time.lst=time.method.lst,
        ## file info on mod
        mtime.mod=file.info.mod$mtime,
        ## if available: mtime of input data
        mtime.input=NA_character_,
        logtime.input=NA_character_,
        method.time.inp=time.method.inp
    )

    ## if available: file info for input data
    if(use.input){
        details$file.input <- nminfo.input$tables[,file]
        details$mtime.input <- mtime.inp
        details$logtime.input <- logtime.inp
    }



### more meta information needed.
    ## meta <- list(details=details)
    writeNMinfo(tab.row,list(details=details),append=FALSE)


    if(use.input){
        meta.input <- NULL
        if(exists("data.input")){
            meta.input <- NMinfoDT(data.input)
        }
        meta.input$tables <- NULL
        
        writeNMinfo(tab.row,meta.input,append=TRUE)
    }

    writeNMinfo(tab.row,list(tables=tables.meta),append=TRUE)
    writeNMinfo(tab.row,list(columns=dt.vars),append=TRUE)

    ## setattr(tab.row,"NMdata",meta)
    tab.row <- as.fun(tab.row)
    ##    writeNMinfo(tab.row,meta,byRef=TRUE)

###  Section end: Format output

    setattr(tab.row,"class",c("NMdata",class(tab.row)))

    if(!quiet){
        print(summary(tab.row))
    }

    tab.row
}
