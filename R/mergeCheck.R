##' Merge, order, and check resulting rows and columns.
##'
##' This function is a useful wrapper for merges where df1 will be
##' extended with columns from df2, i.e. all rows in df1 are retained,
##' and no new rows can be created. For this very common type of
##' (simple) merges, mergeCheck does the merge and ensures that
##' exactly this and nothing else happened. Notice, mergeCheck passes
##' the hard work to merge.data.table, the contributions lies in
##' checking that the results are consistent with the simple merge
##' described above.
##'
##' @param df1 A data.frame with the number of rows must should be
##'     obtained from the merge. The resulting data.frame will be
##'     ordered like df1.
##' @param df2 A data.frame that will be merged onto df1.
##' @param by The column(s) to merge by. Character string
##'     (vector). by or by.x and by.y must be supplied.
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
##' @param ncols.expect If you want to include a check of the number
##'     of columns being added to the dimensions of df1. So if
##'     ncols.expect=1, the resulting data must have exactly one
##'     column more than df1 - if not, an error will be returned.
##' @param track.msg If using mergeCheck inside other functions, it
##'     can be useful to use track.msg=TRUE. This will add information
##'     to messages/warnings/errors that they came from mergCheck.
##' @param quiet If FALSE, the names of the added columns are
##'     reported. Default value controlled by NMdataConf.
##' @param ... additional arguments passed to data.table::merge. If
##'     all is among them, an error will be returned.
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
##' @importFrom utils capture.output
##' @importFrom stats setNames
##' @return a data.frame resulting from merging df1 and df2. Class as
##'     defined by as.fun.
##' 
##'
##' @export

mergeCheck <- function(df1,df2,by,fun.commoncols=base::warning,ncols.expect,track.msg=FALSE,quiet,as.fun,...){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    N.result <- NULL
    N.df1 <- NULL
    N <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    name.df1 <- deparse(substitute(df1))
    name.df2 <- deparse(substitute(df2))
    ## in some cases where data.frames are built inside the mergeCheck
    ## call, names can turn out with multiple elements. We must avoid
    ## that.
    if(length(name.df1)!=1) name.df1 <- "df1"
    if(length(name.df2)!=1) name.df2 <- "df2"

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    
    name.df3 <- "merged.df"
    if("all"%in%names(list(...))) {
        messageWrap("option all not supported. mergeCheck is intended for merges that result in column additions to df1, that's all.",
                    fun.msg=stop,track.msg=track.msg)
    }

### check if sort is in ... That cannot be passed.
    if("sort"%in%names(list(...))) messageWrap("sort is not meaningful in combination with mergeCheck because mergeCheck will preserve order of df1. Please drop sort argument.",fun.msg=stop,track.msg=track.msg)

### df1 cannot be of nrow==0. There would be no way to add columns to a zero row data set
    if(nrow(df1)==0) messageWrap(paste(name.df1," has no rows. df1 must have existing rows so that values of the columns in by can be used for the merge."),fun.msg=stop,track.msg=track.msg)
    
    ## if data is not data.tables, convert to data.tables
    stopifnot(is.data.frame(df1))
    stopifnot(is.data.frame(df2))
    df1.was.dt <- TRUE
    if(is.data.table(df1)){
        ## will add a row counter to df1, so better avoid editing by ref
        df1 <- copy(df1)
    } else {
        df1 <- as.data.table(df1)
        df1.was.dt <- FALSE
    }

    ## df2 is not edited. So if already a data.table, we don't edit.
    if(!is.data.table(df2)){
        df2 <- as.data.table(df2)
    }

    if(missing(as.fun)) as.fun <- NULL
    if(df1.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    ### this has to be refined. by.x+by.y will do too.
    ## if(missing(by) || is.null(by)) stop("The \"by\" argument must be supplied.") 

    if(!missing(by)){
        cols.common <- intersect(colnames(df1),colnames(df2))
        cols.com.notby <- setdiff(cols.common,by)
        commoncols.found <- FALSE
        if(length(cols.com.notby)) {
            messageWrap(paste0("df1 and df2 have common column names not being merged by. This will create new column names in output. Common but not merged by: ",paste(cols.com.notby,collapse=", "),"."),
                        fun.msg=fun.commoncols,track.msg=track.msg)
            commoncols.found <- TRUE
        }
    }
    rowcol <- tmpcol(names=c(colnames(df1),colnames(df2)))

    if(nrow(df1)) {
        ## df1 is not NULL
        reorder <- TRUE
        df1[,(rowcol):=1:nrow(df1)]
    } else {
        reorder <- FALSE
    }
    
    df3 <- merge(df1,df2,by=by,sort=FALSE,...)

    rows.disappeared <- !(all(df1[,get(rowcol)]%in%df3[,get(rowcol)]))
    rows.created <- !(all(df3[,get(rowcol)]%in%df1[,get(rowcol)]))
### this has to be tested before activated 
    rows.dup  <- df3[,.N,by=rowcol][,any(N>1)]
    rows.number.changed <- nrow(df1)!=nrow(df3)

    if(any(c(rows.disappeared,rows.created,rows.number.changed))){

        if(rows.number.changed && !rows.disappeared && !rows.created){
            messageWrap("Number of rows changed during merge.\n",fun.msg=message,track.msg=track.msg)
        }
        if(rows.disappeared) messageWrap("Rows disappeared during merge.",fun.msg=message,track.msg=track.msg)
        if(rows.dup) messageWrap("Rows duplicated during merge.",fun.msg=message,track.msg=track.msg)
        if(rows.created) messageWrap("New rows appeared during merge.",fun.msg=message,track.msg=track.msg)
        
        dims.rep <- dims(list.data=
                             setNames(list(df1,df2,df3),c(name.df1,name.df2,name.df3))
                         )
        
        message("Overview of dimensions of input and output data:\n",
                paste0(capture.output(dims.rep), collapse = "\n"),"\n"
                )
        
        dtcheck <- merge(df1[,.N,by=by],
                         df3[,.N,by=by]
                        ,by=by,all=TRUE,suffixes=c(".df1",".result")
                         )
        dtcheck[is.na(N.df1),N.df1:=0]
        dtcheck[is.na(N.result),N.result:=0]
        dtcheck <- dtcheck[N.df1!=N.result]

        message("Overview of values of by where number of rows in df1 changes:\n",
                paste0(capture.output(dtcheck), collapse = "\n"))
        
        stop("Merge added and/or removed rows.")
    }

    if(reorder){
        df3 <- setorderv(df3,rowcol)
        df3[,(rowcol):=NULL]
    }

    newcols <- setdiff(colnames(df3),colnames(df1))
### checking number of new columns
    if(!missing(ncols.expect)) {
        df1[,(rowcol):=NULL]
        n.newcols <- ncol(df3)-ncol(df1)
        if(n.newcols!=ncols.expect) {
            messageWrap(sprintf("Number of new columns (%d) does not mactch the expected (%d). New columns are: %s.",n.newcols,ncols.expect,paste(newcols,", ")),fun.msg=stop,track.msg=track.msg)
        }
    }
    
    df3 <- as.fun(df3)

    if(!quiet){
        msg <- paste0("The following columns were added: ",paste(newcols,collapse=", "))
        message(msg)
    }
    
    df3
}
