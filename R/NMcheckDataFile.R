##' check input data based on control stream
##'
##' Finds input data and checks compatibility with Nonmem control
##' stream and runs NMcheckData. Don't call this function directly -
##' use the file argument in NMcheckData instead.
##'
##' @param file a model file (input or output control stream)
##' @param col.row row identifier
##' @param col.id subject identifier
##' @param formats.read Prioritized input data file formats to look
##'     for and use if found. Default is c("csv") which means
##'     \code{rds} and \code{fst} will _not_ be used. Typically only
##'     the csv file is wanted because we want to test the input data
##'     as read by Nonmem.
##' @param quiet Keep quiet? Default is FALSE.
##' @param file.mod How to find the input control stream if you are
##'     using the output control stream.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output control stream, use dir.data to
##'     tell in which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param as.fun The function to run results through before returning
##'     them.
##' @param use.rds Deprecated. Use formats.read instead.
##' @param ... passed to NMcheckData
##' @return A list of diagnostics
##' @keywords internal

### Don't export. NMcheckData will be the way in.

NMcheckDataFile <- function(file,col.row,col.id="ID",formats.read="csv",quiet=FALSE,file.mod,dir.data,as.fun,use.rds,...){
    
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdataDecideOption("file.mod",file.mod)

    if(missing(dir.data)) dir.data <- NULL
    
    if(missing(col.row)) {
        col.row <- NULL
    }
    col.row <- NMdataDecideOption("col.row",col.row)

    dots <- list(...)
    if("use.input"%in%names(dots) && !dots$use.input){
        messageWrap("use.input=FALSE not allowed",track.msg = TRUE,fun.msg=stop)
    }

    args <- getArgs(sys.call(),parent.frame())
    use.rds <- deprecatedArg(oldarg="use.rds",msg="Use `formats.read` instead. Overwriting `formats.read`.",args=args)
    if(!is.null(use.rds)&&use.rds){
        formats.read <- c("rds","csv")
    }
    if(!is.null(use.rds)&&!use.rds){
        formats.read <- setdiff(formats.read,c("rds"))
    }

    inp <- NMscanInput(file,formats.read=formats.read,col.id=col.id,col.row=col.row,
                       translate=TRUE,recover.cols=FALSE,apply.filters=TRUE,
                       file.mod=file.mod,dir.data=dir.data,
                       quiet=TRUE,as.fun=as.fun
                      )

    
    cols.num <- setdiff(colnames(inp),c("DV","AMT"))
    res.check <- NMcheckData(inp,return.summary=TRUE,col.id=col.id,
                             col.row=col.row,quiet=TRUE,as.fun=as.fun,
                             col.flagn=NULL,cols.num=cols.num,...)

    list.res <- NMinfo(inp,as.fun=as.fun)
    list.res <- append(list.res,list(NMcheckData=res.check))

    if(!quiet){
        message("Meta data on input data file:")
        ## message(print(list.res$tables))
        print(list.res$tables)
        message("Comparison of variable naming:")
        ##         message(print(list.res$colnames))
        ## cat("\n\n")
        print(list.res$input.colnames)
        message("NMcheckData on data as read by Nonmem:")
        print(list.res$NMcheckData$summary)
    }
    
    invisible(list.res)
}
