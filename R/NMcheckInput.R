## checks: existence of input file


##' @param ... passed to NMcheckData
NMcheckInput <- function(file,col.row,col.id="ID",use.rds=FALSE,quiet=FALSE,file.mod,as.fun,...){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdataDecideOption("file.mod",file.mod)
    
    
    if(missing(col.row)) {
        col.row <- NULL
    }
    col.row <- NMdataDecideOption("col.row",col.row)

    
    ## res.cnames is part of inp metadata. But what if input data is missing?
    ## res.cnames <- NMcheckColnames(file,use.rds=use.rds,quiet=TRUE,file.mod=file.mod)
    inp <- NMscanInput(file,use.rds=use.rds,col.id=col.id,col.row=col.row,quiet=T,as.fun=as.fun)


    res.check <- NMcheckData(inp,return.summary=TRUE,col.id=col.id,col.row=col.row,quiet=TRUE,as.fun=as.fun,...)

    
    
    list.res <- NMinfo(inp,as.fun=as.fun)
    ##    list.res <- append(list.res,list(colnames=res.cnames))
    list.res <- append(list.res,list(NMcheckData=res.check))

    
    ## list.res <- append(list.res,list(colnames=res.cnames,res.check.findings=res.check$findings,res.check.summary=res.check$summary))
    if(!quiet){
        
        message("Meta data on input data file:")
        message(print(list.res$tables))
        message("Comparison of variable naming:")
        ##         message(print(list.res$colnames))
        ## cat("\n\n")
        message(print(list.res$input.colnames))
        message("NMcheckData as NONMEM reads:")
        message(print(list.res$NMcheckData$summary))

    }
    
    invisible(list.res)
}

