##' automatically find Nonmem tables and organize data
##'
##' @param file A nonmem control stream or output file from nonmem
##'     (.mod or .lst)
##' @param file.mod The input control stream. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. See dir.data too.
##' @param col.id The name of the subject ID variable, default is
##'     "ID".
##' @param col.row A column that is unique for each row. Such a column
##'     is needed for this function to work well.
##' @param use.input Merge with columns in input data? Using this, you
##'     don't have to worry about remembering including all relevant
##'     variables in the output tables.
##' @param recoverRows Include rows from input data files that do not
##'     exist in output tables? This will be added to the $row dataset
##'     only, and $run, $id, and $occ datasets are created before this
##'     is taken into account. A column called nmout will be TRUE when
##'     the row was found in output tables, and FALSE when not. This
##'     is still experimental. More testing is needed.
##' @param add.name If a character string, a column of this name will
##'     be included in all tables containing the model name. The
##'     default is to store this in a column called "model". See
##'     argument "name" as well. Set to NULL if not wanted.
##' @param name The model name to be stored if add.name is not
##'     NULL. If name is not supplied, the name will be taken from the
##'     control stream file name.
##' @param useRDS If an rds file is found with the exact same name
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
##' @param as.dt The default is to return data in data.tables. If
##'     data.frames are wanted, use as.dt=FALSE.
##' @param mergeByFilters This is experimental. If TRUE, the IGNORE
##'     filters in the nonmem control stream are attempted applied and
##'     then input and output data simply cbinded. This is not
##'     recommended (use a row identifier instead), but sometimes it
##'     is your only option.
##' @param NMtabCount Nonmem includes a counter of tables in the
##'     written data files. These are often not useful. Especially for
##'     NMscanData output it can be meaningless because multiple
##'     tables can be combined so this information is not unique
##'     across those source tables. However, if NMtabCount is TRUE
##'     (not default), this will be carried forward and added as a
##'     column called TABLENO.
##' @param debug start by running browser()?
##'
##' @details This function makes it very easy to collect the data from
##'     a Nonmem run. 
##' @family DataWrangling
##' @import data.table
##' @export



#### change log
## adding possibility to stack with discarded lines from input data.
#### end change log


### todo
## No longer sure this is an issue with the new data combination method: check if variables are consistent within ROW: ID (others?) This is fatal and will happen when using long ID's and non-matching format when writing tables from Nonmem.

## TODO: There are certain variables that can only be row specifc: WRES, CWRES, etc.

### end todo 

## when col.row and mergeByFilters are missing, do mergeByFilters but look for a row identifier. Explain and tell user to provide col.row or mergeByFilters to get less messages.

NMscanData <- function(file,col.row="ROW",mergeByFilters=FALSE,use.input=TRUE,recoverRows=FALSE,add.name="model",name,file.mod,dir.data,quiet=FALSE,useRDS=TRUE,as.dt=TRUE,col.id="ID",NMtabCount=FALSE,debug=FALSE) {

    if(debug) browser()

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
    tab.type <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


#### Section start: Process arguments  ####

    file <- filePathSimple(file)
    if(!file.exists(file)) stop(paste0("Model file ",file," does not exist."),call. = F)
    dir <- dirname(file)

    if(!is.null(add.name)) {
        if(!is.character(add.name) || length(add.name)!=1 ||  add.name=="" ) {
            stop("If not NULL, add.name must be a character name of the column to store the run name. The string cannot be empty.")
        }
        if (!missing(name)) {
            runname <- name
        } else {
            runname <- sub("\\.lst$","",basename(sub(" $","",file)))
        }
        include.model <- TRUE
    } else {
        include.model <- FALSE
    }

    ## for easier passing of the argument
    if(missing(dir.data)) dir.data <- NULL
    if(missing(file.mod)) file.mod <- NULL

    if(!is.null(dir.data)&&!is.null(file.mod)){
        stop("Only use one of dir.data and file.mod. The aim is to find the input data file, so either give the directory (dir.data) in which it is, and the filename will be taken from the lst file, or help finding the .mod file using the file.mod argument. Using both is redundant.")
    }
    
###  Section end: Process arguments 


#### Section start: read all output tables and merge to max one firstonly and max one row ####

    ## if(!quiet) message("Scanning for output tables.")
    tables <- NMscanTables(file,details=T,as.dt=T,NMtabCount=NMtabCount,quiet=quiet)

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
    overview.tables[,idlevel:=firstonly|lastonly]
    overview.tables[,full.length:=!idlevel&maxLength]
    NrowFull <- overview.tables[full.length==TRUE,unique(nrow)]

    ## browser()
    
### combine full tables into one
    tabs.full <- which(overview.tables$full.length)

    
    if(use.input && !mergeByFilters) {
        
        if(any(overview.tables[,full.length])&&!overview.tables[,sum(has.row)]) {
            warning("col.row not found in any full-length (not firstonly) output tables. Input data will not be used. See arguments col.row and mergeByFilters.")
            use.input <- FALSE
        }
    }
    
    tab.row <- NULL
    tab.vars <- NULL
    
    if(any(overview.tables[,full.length])) {
        
        ## there might be a little bit to save by reducing the columns before cbind. 
        tab.row <- Reduce(cbind,data[which(overview.tables$maxLength)])
        tab.row <- tab.row[,unique(colnames(tab.row)),with=FALSE]

        tab.row[,nmout:=TRUE]
        tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="output",tab.type="row"))
        tab.vars[var=="nmout",source:="NMscanData"]
        tab.vars[var=="nmout",tab.type:=NA_character_]
    } 
    

### combine idlevel tables into one
    tab.idlevel <- NULL
    if(any(overview.tables$idlevel)) {
        tab.idlevel <- Reduce(cbind,data[which(overview.tables$idlevel)])
        tab.idlevel <- tab.idlevel[,unique(colnames(tab.idlevel)),with=FALSE]
    }

###  Section end: read all output tables and merge to max one idlevel and max one row


#### Section start: handle input data ####
    ## use.input.row means if we will merge row-wise output data onto
    ## input data. Even if FALSE, we can still merge idlevel data
    ## onto input dataif no row-wise output exists.
    
    ## use.input.row <- use.input

    if(use.input&&is.null(dir.data)) {
        if(is.null(file.mod)){
            file.mod <- sub("\\.lst","\\.mod",file)
        }
        if(is.function(file.mod)) {
            path.file.mod <- file.mod(file)
        } else {
            path.file.mod <- file.mod
        }

        if(!file.exists(file.mod)) {
            warning("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.")
            use.input <- FALSE }
    }

    ## use.rows means if to use row-data from output tables
    use.rows <- TRUE
    if(!any(tables$meta$full.length)) {
        use.rows <- FALSE
    }
    
    if(use.input&&!any(tables$meta$full.length)) {
        tab.row <- NMtransInput(file,file.mod=file.mod,dir.data=dir.data,quiet=quiet,useRDS=useRDS,applyFilters=mergeByFilters,as.dt=TRUE,debug=F)
        tab.row[,nmout:=FALSE]
        tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="input",tab.type="row"))
    }
    
    if(use.input&&any(tables$meta$full.length)) {
        ## if(!quiet) message("Searching for input data.")
        data.input <- NMtransInput(file,file.mod=file.mod,dir.data=dir.data,quiet=quiet,useRDS=useRDS,applyFilters=mergeByFilters,as.dt=TRUE,debug=F)
        cnames.input <- colnames(data.input)

        if(mergeByFilters) {
            message("Input data is filtered by translation of the Nonmem controls stream. This works in most cases. However, it is recommended to always use a row identifier in both input and output data if possible. See col.row and mergeByFilters arguments.")

            if(!is.null(tab.row)&nrow(data.input)!=nrow(tab.row)) {
### we have a tab.row and the number of rows doesn't match what's found in input.                
                stop("After applying filters to input data, the resulting number of rows differ from the number of rows in output data. This is most likely because the filters implemented in the control stream are not correctly interpreted. For info on what limitations of this function, see ?NMtranFilters. At this point, all you can do to merge with input data is either adding a row identifier (always highly recommended) or manually merge output from NMscanTables() and NMtransInput().")
            }

            tab.vars <- rbind(tab.vars,data.table(var=setdiff(colnames(data.input),colnames(tab.row)),source="input",tab.type="row"))
            tab.row <- cbind(
                tab.row,
                data.input[,!colnames(data.input)%in%colnames(tab.row),with=F]
            )

            
        } else {
            ## !mergeByFilters
            if(is.null(col.row)||is.na(col.row)||(is.character(col.row)&&any(col.row==""))) {
                stop("when use.input=TRUE and mergeByFilters=FALSE, col.row cannot be NULL, NA, or empty.")
            }
### merging by col.row
            ## Has this check already been done?
            if(col.row%in%cnames.input) {
                if(data.input[,any(duplicated(get(col.row)))]) {
                    stop("use.input=TRUE and mergeByFilters=FALSE. Hence, input data and output data must be merged by a unique row identifier (col.row), but col.row has duplicate values in _input_ data. col.row must be a unique row identifier when use.input=TRUE and mergeByFilters=FALSE.")
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _input_ data. Only output data used.")
                use.input <- FALSE
            }
            
            if(col.row%in%colnames(tab.row)) {
                if( tab.row[,any(duplicated(get(col.row)))]) {
                    stop("use.input is TRUE, but col.row has duplicate values in _output_ data. col.row must be a unique row identifier. It is unique in input data, so how did rows get repeated in output data? Has input data been edited since the model was run?")
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _output_ data. Only output data used.")
                use.input <- FALSE
            }
            

            if(use.input) {
                

                tab.vars <- rbind(tab.vars,
                                  data.table(var=setdiff(colnames(data.input),colnames(tab.row)),source="input",tab.type="row"))
                tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T)
                
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
            
            tab.idlevel <- mergeCheck(tab.idlevel[,setdiff(colnames(tab.idlevel),col.id),with=F],tab.row[,c(col.row,col.id),with=F],by=col.row)
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

                tab.vars <- rbind(tab.vars,data.table(var=colnames(tab.row),source="output",tab.type="idlevel"))
                
            } else {
                
                ## use tab.vars for the subset
                cols.to.use <- unique(c(col.id,setdiff(colnames(tab.idlevel),tab.vars[source=="output",var])))
                tab.idlevel.merge <- tab.idlevel[,cols.to.use,with=F]
                tab.row <- mergeCheck(tab.row,tab.idlevel.merge,by=col.id)
                tab.vars <- rbind(tab.vars,data.table(var=cols.to.use,source="output",tab.type="idlevel"))
            }
        }
    }

### Section end: Add idlevel data
    
    if(!use.rows && skip.idlevel) {stop("No output data could be used. If enabled, try disabling use.input.")}

#### Section start: Recover rows ####

    if( use.input && recoverRows ) {
        
        skip.recover <- FALSE
        if(mergeByFilters) {
            data.recover <- NMtransInput(file,quiet=quiet,useRDS=useRDS,applyFilters=mergeByFilters,invert=T,as.dt=TRUE,debug=F)
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
        
        tab.vars <- rbind(tab.vars,
                          data.table(var=add.name,source="NMscanData"),
                          fill=T)
    }

    setorder(tab.vars,var)
    if(!as.dt) {
        tab.row <- as.data.frame(tab.row)
        tab.vars <- as.data.frame(tab.vars)
    }

    ## attr(tab.row,"vars") <- tab.vars
    setattr(tab.row,"vars",tab.vars)
    ## class(tab.row)  <- c("NMdata",class(tab.row))
    setattr(tab.row,"class",c("NMdata",class(tab.row)))
    
    tab.row
}
