## checks: existence of input file


##' @param ... passed to NMcheckData
NMcheckInput <- function(file,col.row,col.id="ID",use.rds=FALSE,quiet=FALSE,...){

    if(missing(col.row)) {
        col.row <- NULL
    }
    col.row <- NMdataDecideOption("col.row",col.row)
    
    res.cnames <- NMcheckColnames(file,use.rds=use.rds)
    inp <- NMscanInput(file,use.rds=use.rds,col.id=col.id,col.row=col.row,quiet=T)

### t
    ## tables$has.col.row <- NULL
    ## tables$has.col.id <- NULL

    res.check <- NMcheckData(inp,return.summary=TRUE,col.id=col.id,col.row=col.row,...)
    
    tables <- NMinfo(inp,"tables")

    list.res <- list(meta.data.input=tables,colnames=res.cnames,res.check$findings,res.check.summary=res.check$summary)
    if(!quiet){
        
        message("Meta data on input data file:")
        message(list.res$meta.data.input)
        message("Comparison of variable naming:")
        print(list.res$colnames)
        message("NMcheckData as NONMEM reads:")
        print(list.res$res.check.summary)

        }
    
    invisible(list.res)
}

