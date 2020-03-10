##' Merge, order, and check resulting rows and columns.
##'
##' This function is a useful wrapper for merges where df1 will be extended with
##' columns from df2, i.e. all rows in df1 are retained, and no new rows can be
##' created. For this very common type of (simple) merges, mergeCheck does the
##' merge and ensures that exactly this happened. 
##'
##' @param df1 A data.fram with the number of rows must should be obtained from
##'     the merge. The resulting data.frame will be ordered like df1.
##' @param df2 A data.frame that will be merged onto df1.
##' @param by The column(s) to merge by. Character string (vector). Must be
##'     supplied.
##' @param debug Start by calling browser()?
##' @param ... additional arguments passed to merge. If all is among them, an
##'     error will be returned.
##' @details Besides merging and checking rows, mergeCheck makes sure the order
##'     in df1 is retained in the resulting data. Also, a warning is given if
##'     column names are overlapping, making merge create new column names like
##'     col.x and col.y. Merges and other operations are done using
##'     data.table. If df1 is a data.frame (and not a data.table), it will
##'     internally be converted to a data.table, and the resulting data.table
##'     will be converted back to a data.frame before returning.
##' @family DataWrangling
##' @import data.table
##' @return a data.frame resulting from merging df1 and df2
##' @export

mergeCheck <- function(df1,df2,by,debug=F,...){
    if(debug) browser()
    
    name.df1 <- deparse(substitute(df1))
    name.df2 <- deparse(substitute(df2))
    name.df3 <- "merged.df"
    if("all"%in%names(list(...))) stop("option all not supported. mergeCheck is for merges that are intended to result in column additions to df1, that's all.")

    ## if data is not data.tables, convert to data.tables
    stopifnot(is.data.frame(df1))
    stopifnot(is.data.frame(df2))
    was.df.df1 <- FALSE
    if(is.data.table(df1)){
        df1 <- copy(df1)
    } else {
        df1 <- as.data.table(df1)
        was.df.df1 <- TRUE
    }
    was.df.df2 <- FALSE
    if(is.data.table(df2)){
        df2 <- copy(df2)
    } else {
        df2 <- as.data.table(df2)
        was.df.df2 <- TRUE
    }

    rowcol <- tmpcol(names=c(colnames(df1),colnames(df2)))

    if(nrow(df1)) {
        ## df1 is not NULL
        reorder <- T
        df1[,(rowcol):=1:nrow(df1)]
    } else {
        reorder <- F
    }
    
    df3 <- merge(df1,df2,by=by,...)
    if(reorder){
        rows.disappeared <- !(all(df1[,get(rowcol)]%in%df3[,get(rowcol)]))
        if(rows.disappeared) cat("Rows disappeared during merge.\n")
        rows.created <- !(all(df3[,get(rowcol)]%in%df1[,get(rowcol)]))
        if(rows.created) cat("New rows appeared during merge.\n")
        rows.number.changed <- nrow(df1)!=nrow(df3)
        if(rows.number.changed) cat("Number of rows changed during merge.\n")
        
        if(any(c(rows.disappeared,rows.created,rows.number.changed))){

            cat(paste0("nrow(",name.df1,"):"),nrow(df1),"\n")
            cat(paste0("nrow(",name.df2,"):"),nrow(df2),"\n")
            cat(paste0("nrow(",name.df3,"):"),nrow(df3),"\n")

            stop("Merge added and/or removed rows.")
        }
        ## if(nrow(df3)!=nrow(df1)){
        ##  cat(paste0("nrow(",name.df1,"):"),nrow(df1),"\n")
        ##  cat(paste0("nrow(",name.df2,"):"),nrow(df2),"\n")
        ##  cat(paste0("nrow(",name.df3,"):"),nrow(df3),"\n")
        ##  stop("merge changed dimensions")        
        ## }
        df3 <- setorderv(df3,rowcol)
        df3[,(rowcol):=NULL]
    }
    

###### check if new column names have been created
    if(!all(colnames(df3) %in% c(colnames(df1),colnames(df2)))){
        warning("Merge created new column names. Not merging by all common columns?")
    }

    if(was.df.df1){
        df3 <- as.data.frame(df3)
    }

    df3

}
