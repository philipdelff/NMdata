##' automatically find Nonmem tables and organize data
##'
##' @param file A nonmem control stream or output file from nonmem
##'     (.mod or .lst)
##' @param file.mod The input control stream. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. The default behavior is controlled by the
##'     "NMdata.file.mod" option. See dir.data too.
##' @param col.id The name of the subject ID variable, default is
##'     "ID".
##' @param col.row A column with a unique value for each row. Such a
##'     column is recommended to use if possible. See cbind.by.filters
##'     and details as well.
##' @param use.input Merge with columns in input data? Using this, you
##'     don't have to worry about remembering including all relevant
##'     variables in the output tables.
##' @param recover.rows Include rows from input data files that do not
##'     exist in output tables? This will be added to the $row dataset
##'     only, and $run, $id, and $occ datasets are created before this
##'     is taken into account. A column called nmout will be TRUE when
##'     the row was found in output tables, and FALSE when not. This
##'     is still experimental. More testing is needed.
##' @param add.name If a character string, a column of this name will
##'     be included in all tables containing the model name. The
##'     default is to store this in a column called "model". See
##'     argument "name" as well. Set to NULL if not wanted.
##' @param modelname The model name to be stored if add.name is not
##'     NULL and not set by options(NMdata.modelname=...). If not
##'     supplied, the name will be taken from the control stream file
##'     name by omitting the directory/path and deleting the .lst
##'     extension. If using the NMdata.modelname option, please submit
##'     a function taking file as argument and returning the modelname
##'     (by extracting it from file name or path).
##' @param use.rds If an rds file is found with the exact same name
##'     (except for .rds instead of say .csv) as the input data file
##'     mentioned in the Nonmem control stream, should this be used
##'     instead? The default is yes, and NMwriteData will create this
##'     by default too.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output file, use dir.data to tell in
##'     which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="none". See ?runAsFun.
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



#### change log
## adding possibility to stack with discarded lines from input data.
#### end change log


## when col.row and cbind.by.filters are missing, do cbind.by.filters but look for a row identifier. Explain and tell user to provide col.row or cbind.by.filters to get less messages.

NMscanData <- function(file,col.row,cbind.by.filters,use.input=TRUE,
                       recover.rows=FALSE,add.name="model",modelname,
                       file.mod,dir.data,quiet=FALSE,use.rds=TRUE,
                       as.fun=NULL,col.id="ID",tab.count=FALSE) {

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    firstonly <- NULL
    has.row <- NULL
    type <- NULL
    maxLength <- NULL
    full.length <- NULL
    nmout <- NULL
    DV <- NULL
    var <- NULL
    firstlastonly <- NULL
    idlevel <- NULL
    lastonly <- NULL
    level <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


#### Section start: Process arguments  ####

    file <- filePathSimple(file)
    if(!file.exists(file)) messageWrap(paste0("Model file ",file," does not exist."),fun.msg=stop)
    dir <- dirname(file)

    if(!missing(cbind.by.filters) && !is.logical(cbind.by.filters)){
        stop("If supplied, cbind.by.filters must be logical.")
    }

    ## for easier passing of the argument
    if(missing(dir.data)) dir.data <- NULL
    if(missing(file.mod)) file.mod <- NULL

    if(!is.null(dir.data)&&!is.null(file.mod)){
        messageWrap("Only use one of dir.data and file.mod. The aim is to find the input data file, so either give the directory (dir.data) in which it is, and the filename will be taken from the lst file, or help finding the .mod file using the file.mod argument. Using both is redundant.",fun.msg=stop)
    }

    
### cbind.by.filters and col.row - specification of merging method
    ## use.input <- TRUE
    search.col.row <- FALSE
    do.cbind.by.filters <- FALSE
    merge.by.row <- FALSE

    if(use.input){
### method not specified
        ## simplest function call - default
        if( missing(cbind.by.filters) && missing(col.row) ){
            do.cbind.by.filters <- TRUE
            search.col.row <- TRUE
            col.row <- NULL
        } else if(missing(cbind.by.filters) && !missing(col.row) && is.null(col.row) ){
            do.cbind.by.filters <- TRUE


### method specified
            ## cbind.by.filters specified - col.row can be NULL too
        } else if(!missing(cbind.by.filters) && cbind.by.filters && (missing(col.row) || is.null(col.row))){
            do.cbind.by.filters <- TRUE
        } else if(!missing(cbind.by.filters) && !cbind.by.filters && (missing(col.row) || is.null(col.row))){
            use.input <- FALSE
            ## col.row specified
        } else if(missing(cbind.by.filters) && !missing(col.row) && !is.null(col.row) ){
            merge.by.row <- TRUE
            ## cbind.by.filters and col.row specified
        } else if(!cbind.by.filters && !missing(col.row) && !is.null(col.row) ){
            merge.by.row <- TRUE

### redundant specification
        } else if(cbind.by.filters && !missing(col.row) && !is.null(col.row) ){
            stop("cbind.by.filters cannot be TRUE and col.row non-NULL at the same time.")
        } else {
            messageWrap("A non-recognized combination of cbind.by.filters and col.row. Please see the documenation of those two arguments.",fun.msg=stop)
        }

        cbind.by.filters <- do.cbind.by.filters
        rm(do.cbind.by.filters)
        if(cbind.by.filters && merge.by.row) {
            stop("This is a bug. Please report.")
        }
    }

### merging method found
### now code must use search.col.row, cbind.by.filters and merge.by.row

    if(missing(col.row)||(!is.null(col.row)&&is.na(col.row))||(is.character(col.row)&&any(col.row==""))) {
        col.row <- NULL
    }

    modelname.by.option <- FALSE
    if(missing(modelname)) {
        modelname <- getOption("NMdata.modelname")
        modelname.by.option <- TRUE
    }
    if(!is.null(modelname) && !is.function (modelname) && !(is.character(file)&&length(file)==1)) {
        stoptext <- "If provided, modelname must be either a function or a character of length 1."
        if(modelname.by.option) {
            stoptext <- paste0(stoptext,"modelname was taken from getOption(\"NMdata.modelname\"). ",stoptext)
        }
        messageWrap(stoptext,fun.msg=stop)
    }
    if(is.null(modelname)) {
        modelname <- function(file) sub("\\.lst$","",basename(sub(" $","",file)))
    } 
    if(is.function(modelname)){
        runname <- modelname(file)
    } else {
        runname <- modelname
    }

    if(!is.null(add.name)) {
        if(!is.character(add.name) || length(add.name)!=1 ||  add.name=="" ) {
            messageWrap("If not NULL, add.name must be a character name of the column to store the run name. The string cannot be empty.",fun.msg=stop)
        }

        include.model <- TRUE
    } else {
        include.model <- FALSE
    }

###  Section end: Process arguments 


#### Section start: read all output tables and merge to max one firstonly and max one row ####

    ## if(!quiet) messageWrap("Scanning for output tables.")
    tables <- NMscanTables(file,details=T,tab.count=tab.count,quiet=TRUE,as.fun="none")

    rows.flo <- tables$meta[firstlastonly==TRUE]
    if(rows.flo[,.N]>0) {
        warning("One or more output tables with FIRSTLASTONLY option detected. This is not supported, and the table will be disregarded. Use a combination of NMscanTables, NMtransInput, and merge manually.")
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
            ## warning("col.row not found in any full-length (not firstonly) output tables. Input data will not be used. See arguments col.row and cbind.by.filters.")
            ## use.input <- FALSE
            messageWrap("col.row not found in any full-length (not firstonly) output tables. Correct or disable.",fun.msg=stop)
        }
    }
    
    tab.row <- NULL
    ##    tab.vars <- NULL
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
        
        ## tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="output",level="row"))
        ## tab.vars[var=="nmout",source:="NMscanData"]
        ## tab.vars[var=="nmout",level:=NA_character_]
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
        file.mod <- getFileMod(file.lst=file,file.mod=file.mod)

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
        data.input <- NMtransInput(file,file.mod=file.mod,dir.data=dir.data,quiet=TRUE,use.rds=use.rds,applyFilters=cbind.by.filters,as.fun="none")
        tab.row <- copy(data.input)
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
        ## tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="input",level="row"))

        tab.row[,nmout:=FALSE]
    }
    
    if(use.input&&any(tables$meta$full.length)) {
        ## if(!quiet) messageWrap("Searching for input data.")
        data.input <- NMtransInput(file,file.mod=file.mod,dir.data=dir.data,quiet=TRUE,use.rds=use.rds,applyFilters=cbind.by.filters,as.fun="none")
        cnames.input <- colnames(data.input)

        ## if no method is specified, search for possible col.row to help the user
        if(search.col.row){
            
            data.input.all <- NMtransInput(file,file.mod=file.mod,dir.data=dir.data,quiet=TRUE,use.rds=use.rds,applyFilters=FALSE,as.fun="none")
            cols.row.input <- colnames(data.input.all)[data.input.all[,unlist(lapply(.SD,function(x)uniqueN(x)==.N))]]

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
                          "To skip this check, please specify either col.row (recommended) or cbind.by.filters.")
            messageWrap(msg,fun.msg=message)
        }
        
        if(cbind.by.filters) {

            if(!is.null(tab.row)&nrow(data.input)!=nrow(tab.row)) {
### we have a tab.row and the number of rows doesn't match what's found in input.                
                messageWrap("After applying filters to input data, the resulting number of rows differ from the number of rows in output data. This is most likely because the filters implemented in the control stream are not correctly interpreted. For info on what limitations of this function, see ?NMtranFilters. At this point, all you can do to merge with input data is either adding a row identifier (always highly recommended) or manually merge output from NMscanTables() and NMtransInput().",fun.msg=stop)
            }

            
            dt.vars1 <- data.table(
                variable=colnames(data.input)
               ,table="input"
               ,source="input"
               ,level="row")

            dt.vars1[,
                     included:=!variable%in%colnames(tab.row)
                     ]

            dt.vars <- rbind(dt.vars,
                             dt.vars1
                             )
            
            
            ## tab.vars <- rbind(tab.vars,data.table(var=setdiff(colnames(data.input),colnames(tab.row)),source="input",level="row"))
            tab.row <- cbind(
                tab.row,
                data.input[,!colnames(data.input)%in%colnames(tab.row),with=F]
            )

            
        } else {
            ## !cbind.by.filters
            if(is.null(col.row)) {
                messageWrap("when use.input=TRUE and cbind.by.filters=FALSE, col.row cannot be NULL, NA, or empty.",fun.msg=stop)
            }
### merging by col.row
            ## Has this check already been done?
            if(col.row%in%cnames.input) {
                if(data.input[,any(duplicated(get(col.row)))]) {
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
                    variable=colnames(data.input)
                   ,table="input"
                   ,source="input"
                   ,level="row"
                )
                
                dt.vars1[,
                         included:=variable%in%setdiff(colnames(data.input),colnames(tab.row))
                         ]

                dt.vars <- rbind(dt.vars,dt.vars1)
                ## tab.vars <- rbind(tab.vars,
                ## data.table(var=setdiff(colnames(data.input),colnames(tab.row)),source="input",level="row"))
                
                tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T,as.fun="none")
                
            }
            
        }
    }

###  Section end: handle input data


#### Section start: Add idlevel data ####
    
    skip.idlevel <- FALSE
    if(!is.null(tab.idlevel)) {
        ## If we use input or row-level output, we will not use DV from idlevel
        if( (use.input || use.rows) && "DV"%in%colnames(tab.idlevel)){
            tab.idlevel[,DV:=NULL]
        }
        ## col.id must be in tab.row, or we can't do this
        if(!is.null(tab.row)&&!all(col.id%in%colnames(tab.row))) {
            warning("col.id not found in row-specific input or output data. Idlevel output data cannot be combined with other data.")
            skip.idlevel <- TRUE
        }
        ## remember all(NULL) is TRUE. So if col.id and/or col.row are
        ## used, all their values must be in tab.row.
        
        if(!skip.idlevel && !is.null(tab.row) && all(!col.id%in%colnames(tab.idlevel)) && all(!col.row%in%colnames(tab.idlevel))) {
            
            warning("Idlevel output data found. But the table(s) contains neither col.id nor col.row. Merging is not supported in this case, so the idlevel table(s) will not be used.")
            skip.idlevel <- TRUE
        }
### here, merge by those of col.row and col.id that are present in both tab.row and tab.idlevel
        ## col.id is in tab.row (known from above)
        if(!skip.idlevel && !is.null(tab.row) && (!all(col.id%in%colnames(tab.row)))) {
            warning("idlevel table is found but col.id is not found in input or all-rows output tables, so the idlevel data cannot be merged. Anyway, how does this make sense, is a idlevel table written for a non-population model?")
            skip.idlevel <- TRUE
        }
        
        ## if col.id is not in tab.idlevel but col.row is, get col.id and discard row.
        if(!skip.idlevel && !is.null(tab.row) && !all(col.id%in%colnames(tab.idlevel))) {
            
            tab.idlevel <- mergeCheck(tab.idlevel[,setdiff(colnames(tab.idlevel),col.id),with=F],tab.row[,c(col.row,col.id),with=F],by=col.row,as.fun="none")
            tab.idlevel[,(col.row):=NULL]
        }

        ## so col.id is in both tab.row and tab.idlevel. merge by col.id
        ## We want everything that is not in output row-data. We want it even if in input data. But give a warning if it varies in input.
        if(!skip.idlevel) {
            ## The very special case where we don't use input and
            ## there is no row-level data.
            if(!use.input && !use.rows) {
                ##  cbind of idlevel and that's it")
                tab.row <- Reduce(cbind,tables$data[which(overview.tables$idlevel)])
                tab.row <- tab.row[,unique(colnames(tab.row)),with=FALSE]
                skip.idlevel <- FALSE

                dt.vars.id1[,included:=TRUE]
                dt.vars <- rbind(dt.vars,dt.vars.id1)
                ## tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="output",level="idlevel"))
                tab.row[,nmout:=TRUE]
                
            } else {
                
                ## use tab.vars for the subset
                cols.to.use <- unique(c(col.id,setdiff(colnames(tab.idlevel),dt.vars[source=="output",variable])))
                tab.idlevel.merge <- tab.idlevel[,cols.to.use,with=F]
                tab.row <- mergeCheck(tab.row,tab.idlevel.merge,by=col.id,as.fun="none")
                
                dt.vars.id1[,included:=FALSE]
                dt.vars.id1[variable%in%setdiff(cols.to.use,col.id),included:=TRUE]

                dt.vars <- rbind(dt.vars,dt.vars.id1)

                ## have to avoid an additional col.id here. Or maybe not. We are merging, so col.id comes from both.
                ## tab.vars <- rbind(tab.vars,data.table(var=cols.to.use,source="output",level="idlevel"))
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
            data.recover <- NMtransInput(file,quiet=TRUE,use.rds=use.rds,applyFilters=cbind.by.filters,invert=T,as.fun="none")
        } else {
            data.recover <- data.input[!get(col.row)%in%tab.row[,get(col.row)]]
            ## data.input[get(col.row)%in%tab.row[,get(col.row)]]
        }
        data.recover[,nmout:=FALSE]
        tab.row <- rbind(tab.row,data.recover,fill=T)
    }

###  Section end: Recover rows
    if(!is.null(add.name)) {
        
        tab.row[,c(add.name):=runname]
        
        dt.vars <- rbind(dt.vars,
                         data.table(variable=add.name
                                   ,table=NA_character_
                                   ,included=TRUE 
                                   ,source="NMscanData"
                                   ,level="model"
                                    ))

        
        ## tab.vars <- rbind(tab.vars,
        ##                   data.table(var=add.name,source="NMscanData"),
        ##                   fill=T)
    }

    ## setorder(tab.vars,var)
    
    tab.row <- runAsFun(tab.row,as.fun)
    ##    tab.vars <- runAsFun(tab.vars,as.fun)

    
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
        variables=dt.vars,
        tables.output=tables$meta
    )

    
    if(use.input) meta$file.input <- attr(data.input,"file")
    ## if available: file info for input data
    if(use.input) meta$mtime.input <- attr(data.input,"mtime.file")

    setattr(tab.row,"meta",meta)
    
    setattr(tab.row,"class",c("NMdata",class(tab.row)))

    if(!quiet){
        print(summary(tab.row))
    }

    tab.row
}
