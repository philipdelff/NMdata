##' check input data based on control stream
##'
##' Finds input data and checks compatibility with nonmem control
##' stream and runs NMcheckData. Don't call this function directly -
##' use the file argument in NMcheckData instead.
##'
##' @param file a model file (input or output control stream)
##' @param col.row row identifier
##' @param col.id subject identifier
##' @param use.rds use rds file instead of csv. This is typically not wanted because we want to test the input data as read by NONMEM.
##' @param quiet Keep quiet? Default is FALSE.
##' @param file.mod How to find the input control stream if you are using the output control stream.
##' @param as.fun The function to run results through before returning them.
##' @param ... passed to NMcheckData

### Don't export. NMcheckData will be the way in.

NMcheckDataFile <- function(file,col.row,col.id="ID",use.rds=FALSE,quiet=FALSE,file.mod,as.fun,...){
    
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdataDecideOption("file.mod",file.mod)
        
    if(missing(col.row)) {
        col.row <- NULL
    }
    col.row <- NMdataDecideOption("col.row",col.row)

    inp <- NMscanInput(file,use.rds=use.rds,col.id=col.id,col.row=col.row,quiet=TRUE,applyFilters=TRUE,as.fun=as.fun)

    dots <- list(...)
    
    res.check <- NMcheckData(inp,return.summary=TRUE,col.id=col.id,col.row=col.row,quiet=TRUE,as.fun=as.fun,col.flagn=NULL,...)

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
        message("NMcheckData on data as read by NONMEM:")
        print(list.res$NMcheckData$summary)
    }
    
    invisible(list.res)
}