##' Automatically find Nonmem tables and organize data
##'
##' This is a very general solution to automatically identifying, reading, and  merging all output and input data in a Nonmem model. The most important
##' steps are
##' \itemize{
##'  \item{Read and combine output tables,}
##'  \item{If wanted, read input data and restore variables that were not output from the nonmem model}
##'  \item{If wanted, also restore rows from input data that were disregarded in
##' Nonmem (e.g. observations or subjects that are not part of the analysis)}
##' }
##' 
##' @param file A nonmem control stream or output file from nonmem
##'     (.mod or .lst)
##' @param file.mod The input control stream. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. The default behavior can be configured using
##'     NMdataConf. See dir.data too.
##' @param col.id The name of the subject ID variable, default is
##'     "ID".
##' @param col.row A column with a unique value for each row. Such a
##'     column is recommended to use if possible. See cbind.by.filters
##'     and details as well.
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
##'     by default too.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output control stream, use dir.data to
##'     tell in which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param cbind.by.filters If TRUE, Nonmem data filtering is
##'     interpreted from lst file (restrictions apply), and after an
##'     imitated selection of rows, data columns will be appended to
##'     output data. This method relies on interpretation of Nonmem
##'     code, and it will not work in advanced use of IGNORE and
##'     ACCEPT statements in $INPUT. Consider using col.row instead,
##'     if possible. Default is TRUE if col.row is either missing or
##'     NULL. However, explicitly specifying cbind.by.filters is
##'     recommended if that is the intended behaviour. If not,
##'     NMscanData will search for potential columns to merge by and
##'     print information about it. See col.row (recommended method)
##'     as well.
##' @param tab.count Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. Especially for
##'     NMscanData output it can be meaningless because multiple
##'     tables can be combined so this information is not unique
##'     across those source tables. However, if tab.count is TRUE (not
##'     default), this will be carried forward and added as a column
##'     called TABLENO.
##' @param order.columns If TRUE (default), NMorderColumns is used to
##'     reorder the columns before returning the data. NMorderColumns
##'     will be called with alpha=FALSE, so columns are not sorted
##'     alphebetically. But standard Nonmem columns like ID, TIME, and
##'     other will be first. If col.row is used, this will be passed
##'     to NMorderColumns too.
##' @param check.time If TRUE (default) and if input data is used,
##'     input control stream and input data are checked to be newer
##'     than output control stream and output tables. These are
##'     important assumptions for the way information is merged by
##'     NMscanData. However, if data has been transfered from another
##'     system where Nonmem was run, these checks may not make sense,
##'     and you may not want to see these warnings. The default can be
##'     configured using NMdataConf.
##'
##' @details This function makes it very easy to collect the data from
##'     a Nonmem run.
##'
##' A useful feature of this function is that it can automatically
##' combine "input" data (the data read by nonmem in $INPUT or
##' $INFILE) with "output" data (tables written by nonmem in
##' $TABLE). There are two implemented methods for doing so. One (the
##' default but not recommended) relies on interpretation of filter
##' (IGNORE and ACCEPT) statements in $INPUT. This will work in most
##' cases, and checks for consistency with Nonmem results. However,
##' the recommended method is using a unique row identifier in both
##' input data and at least one output data file (not a FIRSTONLY or
##' LASTONLY table). Supply the name of this column using the col.row
##' argument.
##' @family DataRead
##' @import data.table
##' @export

## method.combine should be empty by default to look for col.row if none
## supplied. To disable, use method.combine="none"

## rm use.input from NMdataConf

## NMdecideOption define method.combine and remove chk in NMscanData

## if merge by row, got to make sure that col.row can be used.

NMscanData <- function(file, col.row, method.combine,
                       recover.rows,
                       col.model="model", modelname, file.mod,
                       dir.data, quiet=FALSE, use.rds=TRUE,
                       as.fun, col.id="ID", tab.count=FALSE,
                       order.columns=TRUE, check.time) {

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    COLNUM <- NULL
    DV <- NULL
    firstlastonly <- NULL
    firstonly <- NULL
    has.row <- NULL
    full.length <- NULL
    idlevel <- NULL
    included <- NULL
    lastonly <- NULL
    level <- NULL
    maxLength <- NULL
    name <- NULL
    nmout <- NULL
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
    if(missing(as.fun)) as.fun <- NULL
    check.time <- NMdataDecideOption("check.time",check.time)
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    modelname <- NMdataDecideOption("modelname",modelname)
    if(missing(recover.rows)) recover.rows <- NULL
    recover.rows <- NMdataDecideOption("recover.rows",recover.rows)

    if(missing(col.model)) {
        include.model <- TRUE
        col.model <- NMdataDecideOption("col.model",NULL)
    } else if(is.null(col.model)) {
        include.model <- FALSE
    } else {
        include.model <- TRUE
        col.model <- NMdataDecideOption("col.model",NULL)
    }
    
    if(is.null(col.model)){
        include.model <- TRUE
    } else {
        include.model <- FALSE
    }
    
    runname <- modelname(file)
    ## file.mod is treated later if we need the input control stream
    
### combination of arguments
    if(!is.null(dir.data)&&!is.null(file.mod)){
        messageWrap("Only use one of dir.data and file.mod. The aim is to find the input data file, so either give the directory (dir.data) in which it is, and the filename will be taken from the lst file, or help finding the .mod file using the file.mod argument. Using both is redundant.",fun.msg=stop)
    }

    if(missing(col.row)||(!is.null(col.row)&&is.na(col.row))||(is.character(col.row)&&any(col.row==""))) {
        col.row <- NULL
    }

    
### specification of merging method
    search.col.row <- FALSE
    cbind.by.filters <- FALSE
    merge.by.row <- FALSE

    if(missing(col.row)) col.row <- NULL
    col.row <- NMdataDecideOption("col.row",col.row)

    if(missing(method.combine)) method.combine <- NULL
    
    if(is.null(method.combine)){
        ## method not specified
        search.col.row <- TRUE
    }
    method.combine <- NMdataDecideOption("method.combine",method.combine)

    if(method.combine=="filters"){
        use.input <- TRUE
        cbind.by.filters <- TRUE
    }
    if(method.combine=="col.row"){
        use.input <- TRUE
        cbind.by.filters <- FALSE
        merge.by.row <- TRUE
    }
    if(method.combine=="none"){
        use.input <- FALSE
        cbind.by.filters <- FALSE
        merge.by.row <- FALSE
    }
    
### merging method found
### now code must use search.col.row, cbind.by.filters and merge.by.row

###  Section end: Process arguments 


#### Section start: read all output tables and merge to max one firstonly and max one row ####

    ## if(!quiet) messageWrap("Scanning for output tables.")
    tables <- NMscanTables(file,details=T,tab.count=tab.count,quiet=TRUE,as.fun=identity)

    rows.flo <- tables$meta[firstlastonly==TRUE]
    if(rows.flo[,.N]>0) {
        warning("One or more output tables with FIRSTLASTONLY option detected. This is not supported, and the table will be disregarded. Use a combination of NMscanTables, NMscanInput, and merge manually.")
        k <- tables$meta[,which(firstlastonly==TRUE)]
        tables$data <- tables$data[-k]
        tables$meta <- tables$meta[-k]
    }
    data <- tables$data
    overview.tables <- tables$meta


    fun.has.row <- function(names) do.call(c,lapply(names,function(name)col.row%in%colnames(data[[name]])))
    overview.tables[,has.row:=fun.has.row(name)]
    overview.tables[,maxLength:=nrow==max(nrow)]
    overview.tables[,full.length:=!idlevel&maxLength]
    NrowFull <- overview.tables[full.length==TRUE,unique(nrow)]

### combine full tables into one
    tabs.full <- which(overview.tables$full.length)


    if(use.input && merge.by.row) {
        
        if(any(overview.tables[,full.length])&&!overview.tables[,sum(has.row)]) {
            messageWrap("col.row not found in any full-length (not firstonly) output tables. Correct or disable.",fun.msg=stop)
        }
    }

    tab.row <- NULL
    dt.vars <- NULL

    if(any(overview.tables[,full.length])) {
        
        ## there might be a little bit to save by reducing the columns before cbind.
        tab.row <- Reduce(cbind,data[which(overview.tables$maxLength)])
### get all names from this and then select unique to get a table with the included variables
        list.vars <- lapply(data[which(overview.tables$maxLength)],names)
        list.vars <- lapply(list.vars,as.data.table)
        list.vars <- lapply(seq_along(list.vars),function(n)list.vars[[n]][,table:=names(list.vars)[n]])
        dt.vars1 <- rbindlist(list.vars)
        setnames(dt.vars1,c("V1"),"variable")
        ## notice the selection of names in dt.vars and tab.row must be identical
        dt.vars1[,included:=!duplicated(variable)]
        dt.vars1[,`:=`(source="output",level="row")]
        
        tab.row <- tab.row[,!duplicated(colnames(tab.row)),with=FALSE]

        
        tab.row[,nmout:=TRUE]

        dt.vars <- rbind(dt.vars,dt.vars1)
        
        dt.vars <- rbind(dt.vars,
                         data.table(variable="nmout"
                                   ,table=NA_character_
                                   ,included=TRUE 
                                   ,source="NMscanData"
                                   ,level="row"
                                    ))
    } 


### combine idlevel tables into one
    tab.idlevel <- NULL
    if(any(overview.tables$idlevel)) {
        tab.idlevel <- Reduce(cbind,data[which(overview.tables$idlevel)])
        tab.idlevel <- tab.idlevel[,unique(colnames(tab.idlevel)),with=FALSE]

### get all names from this and then select unique to get a table with the included variables
        list.vars.id <- lapply(data[which(overview.tables$idlevel)],names)
        list.vars.id <- lapply(list.vars.id,as.data.table)
        list.vars.id <- lapply(seq_along(list.vars.id),function(n)list.vars.id[[n]][,table:=names(list.vars.id)[n]])
        dt.vars.id1 <- rbindlist(list.vars.id)
        setnames(dt.vars.id1,c("V1"),"variable")
        ## notice the selection of names in dt.vars.id and tab.row must be identical
        ## dt.vars.id1[,included:=!duplicated(variable)]
        dt.vars.id1[,`:=`(source="output",level="id")]

    }

###  Section end: read all output tables and merge to max one idlevel and max one row


#### Section start: handle input data ####
    ## use.input.row means if we will merge row-wise output data onto
    ## input data. Even if FALSE, we can still merge idlevel data
    ## onto input dataif no row-wise output exists.

    ## use.input.row <- use.input

    if(use.input && is.null(dir.data)) {
        
        file.mod <- NMdataDecideOption("file.mod",file.mod)
        file.mod <- file.mod(file)

        if(!file.exists(file.mod)) {
            messageWrap("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",
                        fun.msg=warning)
            use.input <- FALSE }
    }

    ## use.rows means if to use row-data from output tables
    use.rows <- TRUE
    if(!any(tables$meta$full.length)) {
        use.rows <- FALSE
    }


    if(use.input&&!any(tables$meta$full.length)) {
        ## data.input
        ## meta.data 
        data.input <- NMscanInput(file,file.mod=file.mod,
                                  dir.data=dir.data,quiet=TRUE,
                                  use.rds=use.rds,
                                  applyFilters=cbind.by.filters,
                                  as.fun=identity,
                                  col.id=col.id,
                                  details=TRUE)
        
        tab.row <- copy(data.input$data)
        setattr(tab.row,"file",NULL)
        setattr(tab.row,"type.file",NULL)
        setattr(tab.row,"mtime.file",NULL)

        dt.vars <- rbind(dt.vars,
                         data.table(
                             variable=colnames(tab.row)
                            ,table="input"
                            ,included=TRUE
                            ,source="input"
                            ,level="row")
                         )
        
        tab.row[,nmout:=FALSE]
    }

    if(use.input&&any(tables$meta$full.length)) {
        ## if(!quiet) messageWrap("Searching for input data.")
        
        data.input <- NMscanInput(file,file.mod=file.mod,
                                  dir.data=dir.data, quiet=TRUE,
                                  use.rds=use.rds,
                                  applyFilters=cbind.by.filters,
                                  as.fun=identity,
                                  col.id=col.id,
                                  details=TRUE)
        
        cnames.input <- colnames(data.input$data)

        ## if no method is specified, search for possible col.row to help the user
        if(search.col.row){
            
            dia <- suppressWarnings(NMscanInput(file,file.mod=file.mod,
                                                dir.data=dir.data,
                                                quiet=TRUE,use.rds=use.rds,
                                                applyFilters=FALSE,
                                                details=TRUE,
                                                col.id=col.id,
                                                as.fun=identity))
            
            cols.row.input <- colnames(dia$data)[dia$data[,unlist(lapply(.SD,function(x)uniqueN(x)==.N))]]

            cols.row.output <- colnames(tab.row)[tab.row[,unlist(lapply(.SD,function(x)uniqueN(x)==.N))]]

            cols.row.both <- intersect(cols.row.input,cols.row.output)
            if(length(cols.row.both)){
                msg0 <- paste("\nInput data columns will be appended to output data. However, column(s) were identified as unique identifiers, present in both input and output data. If this column or one of these columns is not modified by the Nonmem run, consider using this in col.row for a robust merge of input and output data. Candidate columns:",paste(cols.row.both,collapse=", "))
            } else if(length(cols.row.input)) {
                msg0 <- paste("\nInput data columns will be appended to output data. However, column(s) were identified as unique identifiers, present in input data. If this column or one of these columns is not modified by the Nonmem run, consider adding it to a row-level output table and using this in col.row for a robust merge of input and output data. Candidate columns:",paste(cols.row.input,collapse=", "))
            } else {
                msg0 <- "\nInput data columns will be appended to output data. However, it is recommended to use a unique row identifier (typically a counter but only required to be unique for each row) for a robust merge of input and output data. See argument col.row."
            }
            msg <- paste0(msg0,"\n",
                          "To skip this check, please specify a value in method.combine argument.")
            messageWrap(msg,fun.msg=message)
            cat("\n")
        }
        
        if(cbind.by.filters) {
            
            if(!is.null(tab.row)&nrow(data.input$data)!=nrow(tab.row)) {
### we have a tab.row and the number of rows doesn't match what's found in input.                
                messageWrap("After applying filters to input data, the resulting number of rows differ from the number of rows in output data. This is most likely because the filters implemented in the control stream are not correctly interpreted. For info on what limitations of this function, see ?NMtransFilters. At this point, all you can do to merge with input data is either adding a row identifier (always highly recommended) or manually merge output from NMscanTables() and NMscanInput().",fun.msg=stop)
            }

            
            dt.vars1 <- data.table(
                variable=colnames(data.input$data)
               ,table="input"
               ,source="input"
               ,level="row")

            dt.vars1[,
                     included:=!variable%in%colnames(tab.row)
                     ]

            dt.vars <- rbind(dt.vars,
                             dt.vars1
                             )
            
            
            ## tab.vars <- rbind(tab.vars,data.table(var=setdiff(colnames(data.input$data),colnames(tab.row)),source="input",level="row"))
            tab.row <- cbind(
                tab.row,
                data.input$data[,!colnames(data.input$data)%in%colnames(tab.row),with=F]
            )

            
        } else {
            ## !cbind.by.filters
            if(is.null(col.row)) {
                messageWrap("when use.input=TRUE and cbind.by.filters=FALSE, col.row cannot be NULL, NA, or empty.",fun.msg=stop)
            }
### merging by col.row
            ## Has this check already been done?
            if(col.row%in%cnames.input) {
                if(data.input$data[,any(duplicated(get(col.row)))]) {
                    messageWrap("use.input=TRUE and cbind.by.filters=FALSE. Hence, input data and output data must be merged by a unique row identifier (col.row), but col.row has duplicate values in _input_ data. col.row must be a unique row identifier when use.input=TRUE and cbind.by.filters=FALSE.",fun.msg=stop)
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _input_ data. Only output data used.")
                use.input <- FALSE
            }
            
            if(col.row%in%colnames(tab.row)) {
                if( tab.row[,any(duplicated(get(col.row)))]) {
                    messageWrap("use.input is TRUE, but col.row has duplicate values in _output_ data. col.row must be a unique row identifier. It is unique in input data, so how did rows get repeated in output data? Has input data been edited since the model was run?",fun.msg=stop)
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _output_ data. Only output data used.")
                use.input <- FALSE
            }
            

            if(use.input) {
                
                dt.vars1 <- data.table(
                    variable=colnames(data.input$data)
                   ,table="input"
                   ,source="input"
                   ,level="row"
                )
                
                dt.vars1[,
                         included:=variable%in%setdiff(colnames(data.input$data),colnames(tab.row))
                         ]

                dt.vars <- rbind(dt.vars,dt.vars1)
                ## tab.vars <- rbind(tab.vars,
                ## data.table(var=setdiff(colnames(data.input$data),colnames(tab.row)),source="input",level="row"))
                
                tab.row <- mergeCheck(tab.row,data.input$data[,c(col.row,setdiff(colnames(data.input$data),colnames(tab.row))),with=FALSE],by=col.row,all.x=T,as.fun=identity)
                
            }
            
        }
    }

###  Section end: handle input data


#### Section start: Add idlevel data ####
    ## if merge.by.row==TRUE, col.row is the prefered col to merge by. col.row or col.id must be present.

    ## If cbind.by.filters, we merge by col.id.

    ## col.row is only acceptable to
    ## merge by if merge.by.row==TRUE

    skip.idlevel <- FALSE
    if(!is.null(tab.idlevel)) {

        ## If we use input or row-level output, we will not use DV from idlevel
        if( (use.input || use.rows) && "DV"%in%colnames(tab.idlevel)){
            tab.idlevel[,DV:=NULL]
        }
        
        cols.merge.idlevel <- col.id
        if(merge.by.row) cols.merge.idlevel <- c(col.id,col.row)
        
        cols.common.row.id <- intersect(colnames(tab.row),colnames(tab.idlevel))
        if(!any(cols.merge.idlevel%in%cols.common.row.id)){
            warning("subject-level output data cannot be combined with other data. To make use of subject-level output: If method.combine=filters, col.id must be in row-specific input or output data. If method.combine=row, col.id or col.row must be in row-specific input or output data.")
            skip.idlevel <- TRUE
        }

        ## if col.row is in cols.merge.row.id, merge by col.row only.
        
        ## We want everything that is not in output row-data. We want it even if in input data. But give a warning if it varies in input.
        if(!skip.idlevel) {
            ## The very special case where we don't use input and
            ## there is no row-level data.
            if(!use.input && !use.rows) {
                ## there's nothing else - so just return idlevel data
                tab.row <- tab.idlevel

                dt.vars.id1[,included:=TRUE]
                dt.vars <- rbind(dt.vars,dt.vars.id1)
                ## tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="output",level="idlevel"))
                tab.row[,nmout:=TRUE]
                
            } else {

                id.cols.not.new <- col.id
                if(merge.by.row && all(col.row%in%colnames(tab.row))){
                    ## fetch new ID column, merging by col.row. Then we will merge by
                    ## ID.
                    if(col.id%in%colnames(tab.idlevel)){
                        tab.idlevel[,(col.id):=NULL]
                    }
                    
                    tab.idlevel <- mergeCheck(tab.idlevel,unique(tab.row[,c(col.row,col.id),with=FALSE]),by=col.row)
                    tab.idlevel[,(col.row):=NULL]
                    id.cols.not.new <- c(col.row,col.id)
                }
                
                ## use tab.vars for the subset
                cols.to.use <- unique(c(col.id,setdiff(colnames(tab.idlevel),dt.vars[source=="output",variable])))
                tab.idlevel.merge <- tab.idlevel[,cols.to.use,with=F]
                tab.row <- mergeCheck(tab.row,tab.idlevel.merge,by=col.id,as.fun="data.table")
                
                dt.vars.id1[,included:=FALSE]
                dt.vars.id1[variable%in%setdiff(cols.to.use,id.cols.not.new),included:=TRUE]

                dt.vars <- rbind(dt.vars,dt.vars.id1)

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
        if(cbind.by.filters) {
            data.recover <- NMscanInput(file,quiet=TRUE,use.rds=use.rds,
                                        applyFilters=cbind.by.filters,
                                        invert=T,as.fun=identity,
                                        details=FALSE)
        } else {
            data.recover <- data.input$data[!get(col.row)%in%tab.row[,get(col.row)]]
        }
        data.recover[,nmout:=FALSE]
        tab.row <- rbind(tab.row,data.recover,fill=T)
    }

###  Section end: Recover rows

#### Section start: Check file modification times ####

    if(check.time){    
        if(!is.null(file.mod)) {
            mtime.mod <- file.mtime(file.mod)
            if(mtime.mod>file.mtime(file)){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output control stream (",file,") Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
            }
            if(mtime.mod>max(tables$meta[,file.mtime])){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output tables. Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
            }
        }

        if(use.input) {
            mtime.inp <- max(data.input$meta$file.mtime)
            if(mtime.inp > file.mtime(file)){
                messageWrap(paste0("input data (",data.input$meta$file,") is newer than output control stream (",file,") Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
            }
            if(mtime.inp > max(tables$meta[,file.mtime])){
                messageWrap(paste0("input data file (",data.input$meta$file,") is newer than output tables. Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
            }
        }
    }

### Section end: Check file modification times


#### Section start: Format output ####

    if(!is.null(col.model)) {
        
        tab.row[,c(col.model):=runname]
        
        dt.vars <- rbind(dt.vars,
                         data.table(variable=col.model
                                   ,table=NA_character_
                                   ,included=TRUE 
                                   ,source="NMscanData"
                                   ,level="model"
                                    ))
    }


### order columns in returned data
    if(order.columns){
        tab.row <- NMorderColumns(tab.row, col.row=col.row, as.fun=identity,
                                  alpha=FALSE, quiet=TRUE)
    }

### order columns in variable table accordingly.
    dt.vars[included==TRUE,COLNUM:=match(variable,colnames(tab.row))]
    setorder(dt.vars,-included,COLNUM)
    dt.vars[,included:=NULL]    

    tables$meta[,source:="output"]
    if(use.input){
        data.input$meta[,source:="input"]
        tables.meta <- rbind(tables$meta,data.input$meta,fill=TRUE)
    } else {
        tables.meta <- tables$meta
    }
    setcolorder(tables.meta,c("source","name","nrow","ncol"))


    ## tab.row <- runAsFun(tab.row,as.fun)
    ## tables.meta <- runAsFun(tables.meta,as.fun)
    tab.row <- as.fun(tab.row)
    tables.meta <- as.fun(tables.meta)


### more meta information needed.
    meta <- list(
        ## call
        call=deparse(sys.call()),
        ## time of NMscanData call
        time.call=Sys.time(),
        ## name of model
        model=runname,
        ## path to lst
        file.lst=file,
        ## file info on lst
        mtime.lst=file.info(file)$mtime,
        ## was input used?
        input.used=use.input,
        ## was input recovered?
        rows.recovered=recover.rows,
        ## if available: path to input data
        file.input=NA_character_,
        ## if available: mtime of input data
        mtime.input=NA_character_,
        variables=as.fun(dt.vars),
        tables=tables.meta
    )

    ## if available: file info for input data
    if(use.input){
        meta$file.input <- data.input$meta[,file]
        meta$mtime.input <- data.input$meta[,file.mtime]
    }
    setattr(tab.row,"meta",meta)

###  Section end: Format output

    setattr(tab.row,"class",c("NMdata",class(tab.row)))

    if(!quiet){
        print(summary(tab.row))
    }

    tab.row
}
