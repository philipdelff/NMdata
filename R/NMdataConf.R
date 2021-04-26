##' Configure default behavior of NMdata functions
##' 
##' Configure default behavior across the functions in NMdata rather
##' than typing the arguments in all function calls. Configure for
##' your file organization, data set column names, and other NMdata
##' behaviour. Also, you can control what data class NMdata functions
##' return (say data.tables or tibbles if you prefer one of those over
##' data.frames).
##'
##' @param ... NMdata options to modify. These are named arguments,
##'     like for base::options. Normally, multiple arguments can be
##'     used. The exception is if reset=TRUE is used which means all
##'     options are restored to default values. If NULL is passed to
##'     an argument, the argument is reset to default.  is See
##'     examples for how to use.
##'
##' Parameters that can be controlled are:
##'
##' \itemize{
##' 
##' \item{as.fun} A function that will be applied to data returned by various
##' data reading functions (NMscanData, NMreadTab, NMreadCsv, NMscanInput,
##' NMscanTables). Also, data processing functions like mergeCheck, findCovs,
##' findVars, flagsAssign, flagsCount take this into account, but slightly
##' differently. For these functions that take data as arguments, the as.fun
##' configuration is only taken into account if a the data passed to the
##' functions are not of class data.table. The argument as.fun to these
##' functions is always adhered to. Pass an actual function, say
##' as.fun=tibble::as_tibble. If you want data.table, use as.fun="data.table"
##' (not a function).
##'
##' \item{file.mod} A function that will derive the path to the input control
##' stream based on the path to the output control stream. Technically, it can
##' be a string too, but when using NMdataConf, this would make little sense
##' because it would direct all output control streams to the same input control
##' streams.
##' 
##' \item{modelname} A function that will translate the output control stream
##' path to a model name. Default is to strip .lst, so /path/to/run1.lst will
##' become run1. Technically, it can be a string too, but when using NMdataConf,
##' this would make little sense because it would translate all output control
##' streams model name.
##' 
##' \item{col.flagn} The name of the column containing numerical flag
##' values for data row omission. Default value is FLAG. Used by
##' flagsAssign, flagsCount.
##' 
##' \item{col.flagc} The name of the column containing the character
##' flag values for data row omission. Default value is FLAG. Used
##' by flagsAssign, flagsCount.
##'
##' \item{col.model} The name of the column that will hold the name of
##' the model. See modelname too (which defines the values that the
##' column will hold).
##'
##' \item{col.nomtime} The name of the column holding nominal
##' time. This is only used for sorting columns by NMorderColumns.
##' 
##' \item{col.row} The name of the column containing a unique row
##' identifier. This is used by NMscanData when merge.by.row=TRUE, and
##' by NMorderColumns (row counter will be first column in data).
##'
##' \item{merge.by.row} Adjust the default combine method in
##' NMscanData.
##'
##' \item{quiet} For non-interactive scripts, you can switch off the
##' chatty behavior once and for all using this setting.
##'
##' \item{use.input} In NMscanData, merge with columns in input data?
##' Using this, you don't have to worry about remembering including
##' all relevant variables in the output tables. Default is TRUE.
##'
##' \item{use.rds} Affects NMscanData and NMscanInput. 
##'
##' \item{recover.rows} In NMscanData, Include rows from input data
##'     files that do not exist in output tables? This will be added
##'     to the $row dataset only, and $run, $id, and $occ datasets are
##'     created before this is taken into account. A column called
##'     nmout will be TRUE when the row was found in output tables,
##'     and FALSE when not. Default is FALSE.
##' 
##' \item{check.time} Logical, applies to NMscanData only. NMscanData by
##' defaults checks if output control stream is newer than input control stream
##' and input data. Set this to FALSE if you are in an environment where time
##' stamps cannot be relied on.
##'
##' }
##' @details Recommendation: Use
##' this function transparently in the code and not in a configuration file
##' hidden from other users.
##' 
##' @examples
##' ## get current defaults
##' NMdataConf()
##' ## change a parameter
##' NMdataConf(check.time=FALSE)
##' ## reset one parameter to default value
##' NMdataConf(modelname=NULL)
##' ## reset all parameters to defaults
##' NMdataConf(reset=TRUE)

##' @export

NMdataConf <- function(...){
    
    dots <- list(...)
    if(length(dots)==0){
        ## dump existing options
        return(.NMdata$options)
    }

    names.args <- names(dots)
    if(is.null(names.args) || any(names.args=="")){
        stop("All arguments must be named.")
    }

    N.args <- length(dots)
    
### look for reset=TRUE. If so (and nothing else is given), set default values
    if("reset"%in%names.args) {
        reset <- dots$reset
        if(N.args>1) stop("reset cannot be combined with other arguments.")
        if(!is.logical(reset)) stop("reset must be logical")
        if(reset) {
            allopts <- NMdataConfOptions()
            names.args <- names(allopts)
            args <- lapply(allopts,function(x)x$default)
            N.args <- length(args)
        } else {
            return(.NMdata$options)
        }
    } else {

        args <- lapply(1:N.args,function(I){
            val <- dots[[I]]
            if(is.null(val)) val <- "default"
            NMdataDecideOption(names.args[I],val)
        })

    }
    
    ## if(!any(names(dots) %in% options.allowed)){
    ##     stop("not all option names recognized")
    ## }

    for(I in 1:N.args){
        .NMdata$options[[names.args[I]]] <- args[[I]]
    }
}

## A function that keeps track of the possible parameter options.

## do not export

NMdataConfOptions <- function(name){

    all.options <- list(
        
        as.fun=list(
            default=as.data.frame
           ,is.allowed=function(x){
               is.function(x) || (length(x)==1 && is.character(x) && x%in%c("none","data.table"))
           }
          ,msg.not.allowed="as.fun must be a function or the string \"data.table\""
          ,process=function(x){
              if(is.character(x)&&length(x)==1&&x%in%c("none")){
                  warning("as.fun=none is deprecated (still working but will be removed). Use as.fun=data.table.")
              }
              if(is.character(x)&&length(x)==1&&x%in%c("none","data.table")){
                  ## return(identity)
                  ## this is a trick to ensure data.tables are printed first time
                  return(function(DT)DT[])

              }
              x
          }
        )
       ,
        check.time=list(
            default=TRUE
            ## has to be length 1 character or function
           ,is.allowed=is.logical
           ,msg.not.allowed="check.time must be logical"
           ,process=identity
        )
       ,
        file.mod=list(
            default=function(file) fnExtension(file,ext=".mod")
            ## has to be length 1 character or function
           ,is.allowed=function(x) is.function(x) || (length(x)==1 && is.character(x))
           ,msg.not.allowed="file.mod must be a function or a character of length 1"
           ,process=function(x) {
               if(is.character(x)) return(function(file) x)
               x
           }
        )
       ,
        modelname=list(
            default=function(file) fnExtension(basename(file),"")
            ## has to be length 1 character or function
           ,is.allowed=function(x) is.function(x) || (length(x)==1 && is.character(x))
           ,msg.not.allowed="modelname must be either a function or a character."
           ,process=function(x) {
               if(is.character(x)) return(function(file) x)
               x
           }
        )
       ,
        col.model=list(
            default="model"
           ,is.allowed=function(x) (is.character(x) && length(x)==1)
           ,msg.not.allowed="col.model must be a character vector of length 1."
           ,process=identity
        )
       ,
        col.nomtime=list(
            default="NOMTIME"
           ,is.allowed=function(x) (is.character(x) && length(x)==1)
           ,msg.not.allowed="col.nomtime must be a character vector of length 1."
           ,process=identity
        )
       ,
        col.flagn=list(
            default="FLAG"
           ,is.allowed=function(x) (is.character(x) && length(x)==1)
           ,msg.not.allowed="col.flagn must be a character vector of length 1."
           ,process=identity
        )
       ,
        col.flagc=list(
            default="flag"
           ,is.allowed=function(x) (is.character(x) && length(x)==1)
           ,msg.not.allowed="col.flagc must be a character vector of length 1."
           ,process=identity
        )
       ,
        col.row=list(
            default="ROW"
           ,is.allowed=function(x) (is.character(x) && length(x)==1)
           ,msg.not.allowed="col.row must be a character vector of length 1."
           ,process=identity
        )
       ,
        merge.by.row=list(
            default=FALSE
            ## has to be length 1 character 
           ,is.allowed=function(x)is.logical(x) 
           ,msg.not.allowed="merge.by.row must be TRUE or FALSE."
           ,process=identity
        )
       ,
        quiet=list(
            default=FALSE
           ,is.allowed=is.logical
           ,msg.not.allowed="quiet must be logical"
           ,process=identity
        )
       ,
        use.input=list(
            default=TRUE
           ,is.allowed=is.logical
           ,msg.not.allowed="use.input must be logical"
           ,process=identity
        )
       ,
        use.rds=list(
            default=TRUE
           ,is.allowed=is.logical
           ,msg.not.allowed="use.rds must be logical"
           ,process=identity
        )
       ,
        recover.rows=list(
            default=FALSE
            ## has to be length 1 character or function
           ,is.allowed=is.logical
           ,msg.not.allowed="recover.rows must be logical"
           ,process=identity
        )

    )

    if(!missing(name)&&!is.null(name)){

        if(length(name)==1){
            if(name%in%names(all.options)){
                return(all.options[[name]])
            } else {
                stop("Option not found")
            }
        } else {
            stop("if name is given, it must be a character of length 1")
        }
        
    } else {
        return(all.options)
    }
}

## Could rename to NMdataDecideOption - > NMdataCoalConf for "coalesce". But the
## function is not exported anyway.

## Do not export.
NMdataDecideOption <- function(name,argument){
    
    values.option <- NMdataConfOptions(name)
    ## TODO check that we found that option at all

    if(!missing(argument) && is.character(argument) && length(argument)==1 && argument=="default") {
        return(values.option$default)
    }
    

    if(missing(argument)||is.null(argument)) return(NMdataGetOption(name))
    if(!values.option$is.allowed(argument)) stop(values.option$msg.not.allowed)

    argument <- values.option$process(argument)


    
    return(argument)

}

## This is only used by NMdataDecideOption
## do not export.
NMdataGetOption <- function(...){

    dots <- list(...)

    if(length(dots)!=1) stop("exactly one argument must be given.")

    .NMdata$options[[dots[[1]]]]
    
}


