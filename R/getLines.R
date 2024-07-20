##' @keywords internal

getLines <- function(file,lines,linesep="\n",simplify=TRUE,col.model,modelname,as.one){

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL
    if(missing(as.one)) as.one <- NULL
    if(is.null(as.one)) as.one <- FALSE

    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)
    

    if(!xor(is.null(file),is.null(lines))) stop("Exactly one of file or lines must be supplied")
    
    if(!is.null(file)) {
        if(!all(file.exists(file))) stop("When using the file argument, file has to point to an existing file.")
        lines <- lapply(file,readLines,warn=FALSE)
    }

    if(!is.list(lines)) lines <- list(lines)
    
    if(!isFALSE(linesep)) {
        lines <- lapply(lines,function(x){strsplit(paste(x,collapse=linesep),split=linesep)[[1]]})
    }

    if(as.one && !is.null(file)){
        
        names.models <- modelname(file)
        lines <- lapply(1:length(file),function(N)data.table( text=lines[[N]])[,(col.model):=names.models[N]])
        ##lines[[N]][,(modelname):=names.models[N]])
        lines <- rbindlist(lines)
    }
    
    if(simplify && length(lines)==1){
        lines <- lines[[1]]
    } 
    
    lines

}
