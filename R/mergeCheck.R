##' Merge, order, and check resulting rows and columns.
##'
##' This function is a useful wrapper for merges where df1 will be extended with
##' columns from df2, i.e. all rows in df1 are retained, and no new rows can be
##' created. For this very common type of (simple) merges, mergeCheck does the
##' merge and ensures that exactly this and nothing else happened. 
##'
##' @param df1 A data.fram with the number of rows must should be
##'     obtained from the merge. The resulting data.frame will be
##'     ordered like df1.
##' @param df2 A data.frame that will be merged onto df1.
##' @param by The column(s) to merge by. Character string
##'     (vector). Must be supplied.
##' @param as.fun The default is to return a data.table if df1 is a
##'     data.table and return a data.frame in all other cases. Pass a
##'     function in as.fun to convert to something else.
##' @param fun.commoncols If common columns are found in df1 and df2,
##'     and they are not used in by, this will create columns named
##'     like col.x and col.y in result (see ?merge). Often, this is a
##'     mistake, and the default is to throw a warning if this
##'     happens. If using mergeCheck in a function, you may want to
##'     make sure this is not happening and use
##'     fun.commoncols=stop. If you want nothing to happen, you can do
##'     fun.commoncols=NULL.
##' @param ... additional arguments passed to merge. If all is among
##'     them, an error will be returned.
##' @details Besides merging and checking rows, mergeCheck makes sure
##'     the order in df1 is retained in the resulting data. Also, a
##'     warning is given if column names are overlapping, making merge
##'     create new column names like col.x and col.y. Merges and other
##'     operations are done using data.table. If df1 is a data.frame
##'     (and not a data.table), it will internally be converted to a
##'     data.table, and the resulting data.table will be converted
##'     back to a data.frame before returning.
##' @family DataCreate
##' @import data.table
##' @return a data.frame resulting from merging df1 and df2
##' @export

mergeCheck <- function(df1,df2,by,as.fun=NULL,fun.commoncols=base::warning,...){
    
    name.df1 <- deparse(substitute(df1))
    name.df2 <- deparse(substitute(df2))
    name.df3 <- "merged.df"
    if("all"%in%names(list(...))) {
        messageWrap("option all not supported. mergeCheck is for merges that are intended to result in column additions to df1, that's all.",
                    fun.msg=stop)
    }

    ## if data is not data.tables, convert to data.tables
    stopifnot(is.data.frame(df1))
    stopifnot(is.data.frame(df2))
    df1.was.dt <- TRUE
    if(is.data.table(df1)){
        df1 <- copy(df1)
    } else {
        df1 <- as.data.table(df1)
        df1.was.dt <- FALSE
    }

    ## df2 is not edited. So if already a data.table, we don't edit.
    if(!is.data.table(df2)){
        df2 <- as.data.table(df2)
    }

    if(!missing(by)){
        cols.common <- intersect(colnames(df1),colnames(df2))
        cols.com.notby <- setdiff(cols.common,by)
        commoncols.found <- FALSE
        if(length(cols.com.notby)) {
            messageWrap("option all not supported. mergeCheck is for merges that are intended to result in column additions to df1, that's all.",
                        fun.msg=fun.commoncols)
            commoncols.found <- TRUE
        }
    }
    rowcol <- tmpcol(names=c(colnames(df1),colnames(df2)))

    if(nrow(df1)) {
        ## df1 is not NULL
        reorder <- T
        df1[,(rowcol):=1:nrow(df1)]
    } else {
        reorder <- F
    }
    
    df3 <- merge(df1,df2,by=by,sort=FALSE,...)
    if(reorder){
        rows.disappeared <- !(all(df1[,get(rowcol)]%in%df3[,get(rowcol)]))
        if(rows.disappeared) warning("Rows disappeared during merge.\n")
        rows.created <- !(all(df3[,get(rowcol)]%in%df1[,get(rowcol)]))
        if(rows.created) warning("New rows appeared during merge.\n")
        rows.number.changed <- nrow(df1)!=nrow(df3)
        if(!rows.disappeared&&!rows.created){
            if(rows.number.changed) warning("Number of rows changed during merge.\n")
        }
        
        if(any(c(rows.disappeared,rows.created,rows.number.changed))){

            warning(paste0("\nnrow(",name.df1,"): "),nrow(df1),"\n",
                    paste0("nrow(",name.df2,"): "),nrow(df2),"\n",
                    paste0("nrow(",name.df3,"): "),nrow(df3),"\n")

            stop("Merge added and/or removed rows.")
        }
        df3 <- setorderv(df3,rowcol)
        df3[,(rowcol):=NULL]
    }
    

###### check if new column names have been created
    if(!all(colnames(df3) %in% c(colnames(df1),colnames(df2)))){
        messageWrap("Merge created new column names. Apart from values of by, common columns were not found in df1 and df2, so this should not happen. Please inspect df1, df2 and result carefully.",
                    fun.msg=warning)
    }

    if(!df1.was.dt || !is.null(as.fun)){
        ##        df3 <- as.data.frame(df3)
        df3 <- runAsFun(df3,as.fun)
    }
    
    df3
}
