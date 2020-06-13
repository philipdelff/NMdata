##' automatically find Nonmem tables and organize data
##'
##' @param file A nonmem control stream or output file from nonmem (.mod or
##'     .lst)
##' @param col.id The name of the subject ID variable, default is "ID".
##' @param col.row A column that is unique for each row. Such a column is needed
##'     for this function to work well.
##' @param col.grp If present, ID and OCC level info is grouped by
##'     col.grp. Should only be needed for cross-over - if not grp is a
##'     covariate. Please make sure that the (within-subject varying) grouping
##'     variable is not returned in an output table with firstonly option for
##'     now. For now, this may return unintended merges.
##' @param col.occ The name of a non-mandatory occasion variable (say "OCC").
##' @param structure Either "full" or something else. If full, all variables
##'     that can be represented will be included at all levels. If not, only
##'     row-level data will be included in $row, only occasion-level data in
##'     $occ, etc.
##' @param use.input Merge with columns in input data? Using this, you don't
##'     have to worry about remembering including all relevant variables in the
##'     output tables.
##' @param recoverRows Include rows from input data files that do not exist in
##'     output tables? This will be added to the $row dataset only, and $run,
##'     $id, and $occ datasets are created before this is taken into account. A
##'     column called nmout will be TRUE when the row was found in output
##'     tables, and FALSE when not. This is still experimental. More testing is
##'     needed.
##' @param add.name If a character string, a column of this name will be
##'     included in all tables containing the model name. The default is to
##'     store this in a column called "model". See argument "name" as well. Set
##'     to NULL if not wanted.
##' @param name The model name to be stored if add.name is not NULL. If name is
##'     not supplied, the name will be taken from the control stream file name.
##' @param useRDS If an rds file is found with the exact same name (except for
##'     .rds instead of say .csv) as the input data file mentioned in the Nonmem
##'     control stream, should this be used instead? The default is yes, and
##'     NMwriteData will create this by default too.
##' @param dir.data The data directory can only be read from the control stream
##'     (.mod) and not from the output file (.lst). So if you only have the
##'     output file, use dir.data to tell in which directory to find the data
##'     file. If dir.data is provided, the .mod file is not used at all.
##' @param quiet The default is to give some information along the way on what
##'     data is found. But consider setting this to TRUE for non-interactive
##'     use.
##' @param as.dt The default is to return data in data.tables. If data.frames
##'     are wanted, use as.dt=FALSE.
##' @param mergeByFilters This is experimental. If TRUE, the IGNORE filters in
##'     the nonmem control stream are attempted applied and then input and
##'     output data simply cbinded. This is not recommended (use a row
##'     identifier instead), but sometimes it is your only option. 
##' @param debug start by running browser()?
##'
##' @details This function makes it very easy to collect the data from
##'     a Nonmem run. Only, you have to make sure to include a row
##'     counter in your input data files and your output tables. It
##'     reorganizes the data into four different levels:
##' \itemize{
##'   \item pop
##'   \item id
##'   \item occ
##'   \item row
##' }
##' @family DataWrangling
##' @import data.table
##' @import stats
##' @export



#### change log
## adding possibility to stack with discarded lines from input data.
#### end change log


### todo
## No longer sure this is an issue with the new data combination method: check if variables are consistent within ROW: ID (others?) This is fatal and will happen when using long ID's and non-matching format when writing tables from Nonmem.

## bug: skip input data if not found.

## exit if no tables are found

## use default values for col.grp and col.occ. Use if present.

## TODO: check overview.tables. Either they must be firstonly, or they must be full.length.

## TODO: col.row can only be used if found in both input and at least one output table.

## TODO: There are certain variables that can only be row specifc: WRES, CWRES, etc.

### end todo 

NMscanData <- function(file,col.id="ID",col.row="ROW",col.grp=NULL,col.occ="OCC",structure="full",use.input=TRUE,recoverRows=FALSE,add.name="model",name,dir.data,quiet=FALSE,useRDS=TRUE,as.dt=TRUE,mergeByFilters=FALSE,debug=FALSE){

    if(debug) browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    firstonly <- NULL
    has.row <- NULL
    type <- NULL
    maxLength <- NULL
    full.length <- NULL
    all.firstonly <- NULL
    nmout <- NULL
    tab.out <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


#### Section start: Process arguments  ####

    file <- filePathSimple(file)
    if(!file.exists(file)) stop(paste0("Model file ",file," does not exist."),call. = F)
    dir <- dirname(file)

    if(!is.null(add.name)){
        if(!is.character(add.name) || length(add.name)!=1 ||  add.name=="" ){
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
    
###  Section end: Process arguments 


###{ read all output tables and merge to max one firstonly and max one row
    if(!quiet) message("Scanning for output tables.")
    tables <- NMscanTables(file,details=T,as.dt=T,quiet=quiet)
    data <- tables$data
    overview.tables <- tables$meta

#### TODO: check overview.tables. Either they must be firstonly, or they must be full.length.
    
    
#### add has.grp, has.occ, has.id?
    fun.has.row <- function(names) do.call(c,lapply(names,function(name)col.row%in%colnames(data[[name]])))
    overview.tables[,has.row:=fun.has.row(name)]
######## here
    overview.tables[,maxLength:=nrow==max(nrow)]
    overview.tables[,full.length:=!firstonly&maxLength]
    NrowFull <- overview.tables[full.length==TRUE,unique(nrow)]

    ## browser()
    
### combine full tables into one
    tabs.full <- which(overview.tables$full.length)
    if(overview.tables[,sum(full.length)]==0) {
        stop("No full-length tables found. This is currently not supported (but should be, sorry).")
    }
    if(!mergeByFilters){
        if(!overview.tables[,sum(has.row)]) {
            warning("col.row not found in any full-length (not firstonly) output tables. Input data cannot be used. See argument col.row.")
            use.input <- FALSE
        }
    }
    tab.row <- NULL
    ##    if(sum(overview.tables$full.length&overview.tables$has.row)){
    if(any(overview.tables[,full.length&has.row])){
        ## take row column from the first table in which it appears.
        first.table.with.row <- data[[overview.tables[has.row==TRUE&full.length==TRUE,name[1]]]]
        tab.row <- data.table(col.row=first.table.with.row[,get(col.row)])
    } else {
        tab.row <- data.table(col.row=1:NrowFull)
    }
    
    setnames(tab.row,old="col.row",new=col.row)
    
    for(I in which(overview.tables[,full.length])){
        dt.to.add <- data[[I]][,setdiff(names(data[[I]]),names(tab.row)),with=F]
        if(ncol(dt.to.add)>0){
            tab.row <- cbind(tab.row,dt.to.add)
        }
    }


### combine firstonly tables into one
    tabs.firstonly <- which(overview.tables$firstonly)
    tab.firstonly <- NULL
    if(length(tabs.firstonly)){
        tab.firstonly <- data.table(col.id=data[[tabs.firstonly[1]]][,get(col.id)])
        setnames(tab.firstonly,"col.id",col.id)
        ## setnames(all.row,old="col.id",new=col.id)
        for(I in tabs.firstonly){
            ## mergeCheck?
            tab.firstonly <- merge(tab.firstonly,data[[I]][,c(col.id,setdiff(names(data[[I]]),names(tab.firstonly))),with=F],by=col.id)
        }
    }

    ## data2 <- data[-c(tabs.full,tabs.firstonly)]
    ## data <- c(data2,list(all.row),list(all.firstonly))


###### all row tables combined into one
###}

###{ split tables into row, id, and occ level
### for each table
    ## scan for covariates
    ## scan for occasion variables
    ## check if col.row is present. If so, look for row-level info

    ## nmout is used to keep track of wether rows are from output data or only
    ## from input data.
    tab.row[,nmout:=TRUE]

    
###{ handle input data
    if(use.input) {
        file.mod <- sub("\\.lst","\\.mod",file)
        if(!file.exists(file.mod)&&is.null(dir.data)){
            warning("control stream (.mod) not found next to .lst file. If you don't have a .mod file, see the dir.data argument. Input data not used.")
            use.input <- FALSE
        }
    }
    if(use.input){
        if(!quiet) message("Searching for input data.")
        data.input <- NMtransInput(file,quiet=quiet,useRDS=useRDS,applyFilters=mergeByFilters,as.dt=TRUE,debug=F)
        cnames.input <- colnames(data.input)

        if(mergeByFilters){
            message("Merging input and output by translation of the filtering statemnts in the Nonmem controls stream is experimental, and only IGNORE statements are considered. It is highly recommended to use a row identifier in both input and output data if possible. See col.row and mergeByFilters arguments.")

            if(recoverRows) {
                stop("For now, you cannot combine mergeByFilters and recoverRows.")
            }
            
      
            if(nrow(data.input)!=nrow(tab.row)) {
                
                stop("After applying filters to input data, the resulting number of rows differ from the number of rows in output data. This is most likely because the filters implemented in the control stream are not correctly interpreted by this experimental implementation of the feature. At this point, all you can do to merge with input data is either adding a row identifier (always highly recommended) or merge manually.")
            }
            tab.row <- cbind(
                tab.row,
                data.input[,!colnames(data.input)%in%colnames(tab.row),with=F]
            )
            
        } else {
            
### merging by col.row
            ## Has this check already been done?
            if(col.row%in%cnames.input) {
                if(data.input[,any(duplicated(get(col.row)))]){
                    stop("use.input is TRUE, but col.row has duplicate values in _input_ data. col.row must be a unique row identifier when use.input is TRUE.")
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _input_ data. Only output data used.")
                use.input <- FALSE
            }
            
            if(col.row%in%colnames(tab.row)) {
                if( tab.row[,any(duplicated(get(col.row)))]){
                    stop("use.input is TRUE, but col.row has duplicate values in _output_ data. col.row must be a unique row identifier. It is unique in input data, so how did rows get repeated in output data? Has input data been edited since the model was run?")
                }
            } else {
                warning("use.input is TRUE, but col.row not found in _output_ data. Only output data used.")
                use.input <- FALSE
            }
            

            if(use.input){

                ## tab.row.1 <- copy(tab.row)
                ## tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T)
                tab.row <- mergeCheck(tab.row,data.input[,c(col.row,setdiff(colnames(data.input),colnames(tab.row))),with=FALSE],by=col.row,all.x=T)
                
            }
            
        }
    }

    
##### TODO: There are certain variables that can only be row specifc: WRES, CWRES, etc.
    if(structure=="full"){

        ## tab.row
        if(is.null(tab.row)){
            all.row <- NULL
            tab.occ <- NULL
        } else {
            all.row <- tab.row
            if(!is.null(tab.firstonly)){
                all.row <- merge(tab.row,
                                 tab.firstonly[,c(col.id,setdiff(names(tab.firstonly),names(all.row))),with=FALSE],
                                 by=col.id)
                
            }

            if(col.occ%in%colnames(all.row)){

                tab.occ <- findCovs(all.row,cols.id=c(col.id,col.occ,col.grp),debug=F)
                
            } else {
                tab.occ <- NULL
            }
        }

        ## tab.id
        tab.id <- NULL
        if(col.id%in%colnames(all.row)){
            tab.id <- findCovs(all.row,cols.id=c(col.id,col.grp))
        }
        tab.run <- findCovs(all.row)

    } else {
        stop("only structure=full is implemented.")
    }




    if(use.input&&recoverRows){
        setkeyv(data.input,col.row)
        message("Recovering input data that were not part of analysis. This is experimental. This only affects the \"row\" dataset.")
        data.recover <- data.input[!get(col.row)%in%tab.row[,get(col.row)]]
        ## data.input[get(col.row)%in%tab.row[,get(col.row)]]
        data.recover[,nmout:=FALSE]
        tab.row <- rbind(tab.row,data.recover,fill=T)
        setkeyv(tab.row,col.row)

        ## TODO: if not quite, tell user how much was added.
        
    }
    
    ## if(use.input&&reconstructRows){
    ##     stop("row reconstruction not implemented yet")
    
    ##     ## browser()
    ##     inp.touse <- data.input[setdiff(data.input[,col.row],tab.row[,col.row]),]
    ##     n.inp.touse <- names(inp.touse)
    ##     inp.touse$nmout <- FALSE
    ##     if(col.id%in%n.inp.touse) {
    ##         ## browser()
    ##         inp.touse <- merge(inp.touse,tab.id[,c(col.id,col.grp,setdiff(names(tab.id),n.inp.touse))],all.x=T)
    ##     }
    ##     if(col.occ%in%n.inp.touse) {
    ##         inp.touse <- merge(inp.touse,tab.occ[,c(col.id,col.occ,col.grp,setdiff(names(tab.occ),n.inp.touse))],all.x=T)
    ##     }
    ##     ##    browser()
    ##     tab.row <- rbindUnion(tab.row,inp.touse)
    ##     tab.row <- tab.row[order(tab.row[,col.row]),]
    ## }

    stopifnot(max(table(col.row))==1)

    

    list.str <- list(
        id=col.id,
        row=col.row,
        occ=col.occ,
        grp=col.grp)

    list.out <- list(pop=tab.run,
                     row=tab.row,
                     id=tab.id,
                     occ=tab.occ)
    attr(list.out,"columns") <- list.str
    class(list.out)  <- "NMdata"
    
    for(I in 1:length(list.out)){
        if(!is.null(list.out[[I]])){
            list.out[[I]][,c(add.name):=runname]
        }}
    if(!as.dt) list.out <- lapply(list.out,as.data.frame)
    
    list.out
}
