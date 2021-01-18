NMdataConfig <- function(...){
    dots <- as.list(...)
    if(length(dots))==0{
        ## dump existing options
        return(.NMdata$options)
    }

    options.allowed <- c("as.fun","file.mod","modelname")
    ## Check names of options
    if(!any(names(dots) %in% options.allowed)){
        stop("not all option names recognized")
    }

    for(I in 1:length(dots)){
        .NMdata$options[[names(dots)[I]]] <- dots[[I]]
    }
}

NMdataGetOption <- function(...){

    dots <- as.list(...)

    if(length(dots)!=1) stop("exactly one argument must be given.")

    .NMdata$options[dots[[1]]]
    
}


