##' Write dataset for use in Nonmem (and R)
##'
##' Instead of trying to remember the arguments to pass to write.csv, use this
##' wrapper. It does much more. It tells you what to write in $DATA and $INPUT
##' in nonmem, and it exports a rds file as well which is highly preferable for
##' use in R. 
##'
##' @param data The dataset to write to Nonmem.
##' @param file The file to write to.
##' @param write.csv Write to csv file?
##' @param write.RData In case you want to save to .RData object. Not recommended. Use write.rds instead.
##' @param write.rds write an rds file?
##' @param force.row Ensure that data contains a ROW column counting the rows in the dataset, and add one if none exists. Defaults to FALSE (default is not editing the dataset at all).
##' @param script If provided, the object will be stamped with this script name before saved to rds.
##' @param args.stamp A list of arguments to be passed to stamp.
##' @param debug Start by running browser()?
##'
##' @family Nonmem
##' @export

### Todo

## support for data.table?

## The printed message should not contain lowercase names

## print out all dropped variables. Not warning. Warning if standard variable?

## Check that file ends in either csv or txt

### end todo


NMwriteData <- function(data,file,write.csv=TRUE,write.RData=F,write.rds=write.csv,force.row=FALSE,script,args.stamp,debug=FALSE){
    if(debug) browser()
    stopifnot(is.data.frame(data))
    ## data.out <- as.data.frame(data)
    doStamp <- TRUE
    if(missing(args.stamp)) {
        args.stamp <- list()
        doStamp <- FALSE
    }
    if(missing(script)){
        doStamp <- FALSE
    } else {
        args.stamp$script <- script
        doStamp <- TRUE
    }

### these features have been dropped. Usually NMwriteData saves an rds as well
### which will contain all data to carry forward. Hence, dropping columns belong
### before calling NMwriteDate. NMwriteData writes data, that's all.
    
    ## if(!is.null(drop)) data <- data[,-which(names(data)%in%drop),drop=FALSE]
    ## if(drop.lowercase) data <- data[,which(toupper(names(data))==names(data))]

    ## we must not quote. ID is often a character. If quoted, nonmem will not be
    ## able to read. So avoid commas in strings. Maybe look for commas and
    ## report error if found?
    quote <- FALSE

    ## only report numerics to user. But this is not good enough. Only report
    ## until first character. Moreover, it's not this easy. Variables may be
    ## character but still be interpretable as numeric. Often ID is like this.
    
    ## col.is.num <- sapply(data,is.numeric)
    ## stopifnot(any(col.is.num))

### this function is used to replace .csv or whatever ending is used to .rds, .RData etc. file is path, ext is extension without ., e.g. "rds".
    transFileName <- function(file,ext){
        file.new <- sub("\\.[^\\.]+$",paste0(".",ext),file)
        file.new
    }

    
    cat("Nonmem data file:",file,"\n")
    cat("For NonMem:\n")
    cat("$INPUT",paste(colnames(data),collapse=" "),"\n")
    ##    cat("$DATA", sub(pattern="^E:/",replacement="/project/",file,ignore.case=TRUE))
    cat("$DATA", file)
    ## if(!quote) cat(" IGN=@")
    cat(" IGN=@")
    if("FLAG"%in%colnames(data)) cat(" IGNORE=(FLAG.NE.0)")
    cat("\n")

    written <- FALSE
    if(write.csv){
        opt.orig <- options(scipen=15)
        file.csv <- transFileName(file,"csv")
        write.csv(data,na=".",quote=quote,row.names=FALSE,file=file.csv)
        options(opt.orig)
        written <- TRUE
    }
    if(write.RData){
        name.data <- deparse(substitute(data))
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .RData. Choose a .csv file name.")
        file.RData <- transFileName(file,"RData")
        if(doStamp) data <- do.call(stampObj,append(list(data=data,writtenTo=file.RData),args.stamp))
        assign(name.data,data)
        save(list=name.data,file=file.RData)
        written <- TRUE
    }
    if(write.rds){
        ## A dot and then something is needed in the name for us to be able to
        ## translate
        if(!grepl("\\..+$",file)) stop("filename could not be translated to .rds. Choose a .csv file name.")
        file.rds <- transFileName(file,"rds")
        
        if(doStamp) data <- do.call(stampObj,append(list(data=data,writtenTo=file.rds),args.stamp))


        saveRDS(data,file=file.rds)
        written <- TRUE
    }
    if(written){
        cat("Data file(s) written.\n")
    } else {
        cat("Data returned but not written to file(s).\n")
    }
    invisible(data)
}

