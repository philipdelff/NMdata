## implement reset in NMdataConfig - OK

## initialization with a reset - OK

## individual errors for wrong option values - OK

## process option to support other than function input - OK

## replace getFileMod etc

NMdataConfig <- function(...){
    
    dots <- list(...)
    if(length(dots)==0){
        ## dump existing options
        return(.NMdata$options)
    }

    names.args <- names(dots)
    N.args <- length(dots)
    
### look for reset=TRUE. If so (and nothing else is given), set default values
    if("reset"%in%names.args) {
        reset <- dots$reset
        if(N.args>1) stop("reset cannot be combined with other arguments.")
        if(!is.logical(reset)) stop("reset must be logical")
        if(reset) {
            allopts <- NMdataOptionValues()
            names.args <- names(allopts)
            args <- lapply(allopts,function(x)x$default)
            N.args <- length(args)
        } else {
            return(.NMdata$options)
        }
    } else {


        ## this should be done calling NMdataDecideOption
        ## options.allowed <- c("as.fun","file.mod","modelname")
        ## Check names of options

        args <- lapply(1:N.args,function(I){
            NMdataDecideOption(names.args[I],dots[[I]])
        })

    }
    
    ## if(!any(names(dots) %in% options.allowed)){
    ##     stop("not all option names recognized")
    ## }

    for(I in 1:N.args){
        .NMdata$options[[names.args[I]]] <- args[[I]]
    }
}

NMdataOptionValues <- function(name){

    all.options <- list(
        as.fun=list(
            default=as.data.frame
           ,is.allowed=function(x){
               is.function(x) || (length(x)==1 && is.character(x) && x=="none")
           }
          ,msg.not.allowed="as.fun must be a function"
          ,process=function(x)if(is.character(x)&&length(x)==1&&x=="none"){
                                  identity
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
            default=function(file.lst) sub("\\.lst","\\.mod",file.lst)
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
            default=function(file) sub("\\.lst$","",basename(sub(" $","",file)))
            ## has to be length 1 character or function
           ,is.allowed=function(x) is.function(x) || (length(x)==1 && is.character(x))
           ,msg.not.allowed="modelname must be either a function or a character."
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

NMdataDecideOption <- function(name,argument){
    
    values.option <- NMdataOptionValues(name)
    ## TODO check that we found that option at all

    
    if(missing(argument)||is.null(argument)) return(NMdataGetOption(name))
    ## TODO better error message
    if(!values.option$is.allowed(argument)) stop(values.option$msg.not.allowed)

    argument <- values.option$process(argument)
    
    return(argument)

}

NMdataGetOption <- function(...){

    dots <- list(...)

    if(length(dots)!=1) stop("exactly one argument must be given.")

    .NMdata$options[[dots[[1]]]]
    
}


