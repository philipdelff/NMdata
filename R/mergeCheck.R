##' Merge, order, and check resulting rows and columns.
##'
##' Stop checking that the number of rows is unchanged after a merge -
##' mergeCheck checks what you really want - i.e. x is extended with
##' columns from y while all rows in x are retained, and no new rows
##' are created (plus some more checks). mergeCheck is not a merge implementation - it is a
##' useful merge wrapper. The advantage over using much more flexible
##' merge or join function lies in the fully automated checking that
##' the results are consistent with the simple merge described above.
##'
##' @param x A data.frame with the number of rows must should be
##'     obtained from the merge. The resulting data.frame will be
##'     ordered like x.
##' @param y A data.frame that will be merged onto x.
##' @param by The column(s) to merge by. Character string (vector). by
##'     or by.x and by.y must be supplied.
##' @param by.x If the columns to merge by in x and y are named
##'     differently. by or by.x and by.y must be supplied.
##' @param by.y If the columns to merge by in x and y are named
##'     differently. by or by.x and by.y must be supplied.
##' @param common.cols If common columns are found in x and y, and
##'     they are not used in `by`, this will by default create columns
##'     named like col.x and col.y in result (see ?merge). Often, this
##'     is a mistake, and the default is to throw a warning if this
##'     happens. If using mergeCheck in programming, you may want to
##'     make sure this is not happening and use
##'     common.cols=stop. If you want nothing to happen, you can do
##'     common.cols=NULL. You can also use `common.cols="drop.x"`
##'     to drop "non-by" columns in `x` with identical column names in
##'     `y`. Use "drop.y" to drop them in `y` and avoid the
##'     conflicts. The last option is to use `common.cols="merge.by"`
##'     which means `by` will automatically be extended to include all
##'     common column names.
##' @param ncols.expect If you want to include a check of the number
##'     of columns being added to the dimensions of x. So if
##'     ncols.expect=1, the resulting data must have exactly one
##'     column more than x - if not, an error will be returned.
##' @param subset.x Not implemented.
##' @param track.msg If using mergeCheck inside other functions, it
##'     can be useful to use track.msg=TRUE. This will add information
##'     to messages/warnings/errors that they came from mergCheck.
##' @param quiet If FALSE, the names of the added columns are
##'     reported. Default value controlled by NMdataConf.
##' @param fun.na.by If NA's are found in (matched) by columns in both
##'     x and why, what should we do? This could be OK, but in many
##'     cases, it's because something unexpected is happening. Use
##'     fun.na.by=NULL if you don't want to be notified and want to go
##'     ahead regardless.
##' @param as.fun The default is to return a data.table if x is a
##'     data.table and return a data.frame in all other cases. Pass a
##'     function in as.fun to convert to something else.
##' @param df1 Deprecated. Use x.
##' @param df2 Deprecated. Use y.
##' @param fun.commoncols Deprecated. Please use `common.cols`.
##' @param ... additional arguments passed to data.table::merge. If
##'     all is among them, an error will be returned.
##' @details Besides merging and checking rows, mergeCheck makes sure
##'     the order in x is retained in the resulting data (both rows
##'     and column order). Also, a warning is given if column names
##'     are overlapping, making merge create new column names like
##'     col.x and col.y. Merges and other operations are done using
##'     data.table. If x is a data.frame (and not a data.table), it
##'     will internally be converted to a data.table, and the
##'     resulting data.table will be converted back to a data.frame
##'     before returning.
##' @family DataCreate
##' @import data.table
##' @importFrom utils capture.output
##' @importFrom stats setNames
##' @return a data.frame resulting from merging x and y. Class as
##'     defined by as.fun.
##' @details mergeCheck is for the kind of merges where we think of x
##'     as the data to be enriched with columns from y - rows
##'     unchanged. This is even further limited than a left join where
##'     you can match rows multiple times. A common example of the use
##'     of mergeCheck is for adding covariates to a pk/pd data set. We
##'     do not want that to remove or duplicate doses, observations,
##'     or simulation records. In those cases, mergeCheck does all
##'     needed checks, and you can run full speed without checking
##'     dimensions (which is anyway not exactly the right thing to do
##'     in the general case) or worry that something might go wrong.
##'
##' Checks performed:
##' 
##' \itemize{
##' \item x has >0 rows
##'
##' \item by columns are present in x an y
##'
##' \item Merge is not performed on NA values. If by=ID and both x$ID and
##' y$ID contain NA's, an error is thrown (see argument fun.na.by).
##'
##' \item Merge is done by all common column names in x and y. A
##' warning is thrown if there are column names that are not being
##' used to merge by. This will result in two columns named like BW.x
##' and BW.y and is often unintended.
##'
##' \item Before merging a row counter is added to x. After the merge, the
##' result is assured to have exactly one occurrence of each of the
##' values of the row counter in x.
##'
##' }
##'
##' Moreover, row and column order from x is retained in the result.
##' 
##' @examples
##'  df1 <- data.frame(x = 1:10,
##'                    y=letters[1:10],
##'                    stringsAsFactors=FALSE)
##'  df2 <- data.frame(y=letters[1:11],
##'                    x2 = 1:11,
##'                    stringsAsFactors=FALSE)
##'
##'  mc1 <- mergeCheck(x=df1,y=df2,by="y")
##' 
##' ## Notice as opposed to most merge/join algorithms, mergeCheck by
##' #default retains both row and column order from x
##' library(data.table)
##' merge(as.data.table(df1),as.data.table(df2))
##' ## Here we get a duplicate of a df1 row in the result. If we only
##' ## check dimensions, we make a mistake. mergeCheck captures the
##' ## error - and tell us where to find the problem (ID 31 and 180):
##' \dontrun{
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' dt.cov <- pk[,.(ID=unique(ID))]
##' dt.cov[,COV:=sample(1:5,size=.N,replace=TRUE)]
##' dt.cov <- dt.cov[c(1,1:(.N-1))]
##' dim(pk)
##' res.merge <- merge(pk,dt.cov,by="ID")
##' dim(res.merge)
##' mergeCheck(pk,dt.cov,by="ID")
##' }
##' @export

mergeCheck <- function(x,y,by,by.x,by.y,common.cols=base::warning,ncols.expect,track.msg=FALSE,quiet,df1,df2,subset.x,fun.na.by=base::stop,as.fun,fun.commoncols,...){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    N.result <- NULL
    N.x <- NULL
    N <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    ## deprecate df1 and df2. This was done way before 2023-06-13.
    ## hard to use deprecatedArg because name.x and name.y depend on it.
    ## args <- getArgs()    
    ## x <- deprecatedArg("df1","x",args=args)
    ## y <- deprecatedArg("df2","y",args=args)
    ## name.x <- deparse(substitute(x))
    ## name.y <- deparse(substitute(y))

    if(!xor(missing(x),missing(df1))){stop("You must supply x. Don't use the deprecated df1.")}
    if(!xor(missing(y),missing(df2))){stop("You must supply y. Don't use the deprecated df2.")}

    if(!missing(fun.commoncols)) {
        message("\"fun.commoncols\" argument deprecated. Use \"common.cols\" instead.")
        common.cols <- fun.commoncols
    }

    if(!missing(df1)) {
        message("\"df1\" argument deprecated. Use \"x\" instead.")
        x <- df1
        name.x <- deparse(substitute(df1))
    } else {
        name.x <- deparse(substitute(x))
    }
    if(!missing(df2)) {
        message("\"df2\" argument deprecated. Use \"y\" instead.")
        y <- df2
        name.y <- deparse(substitute(df2))
    } else{
        name.y <- deparse(substitute(y))
    }
    
    ## in some cases where data.frames are built inside the mergeCheck
    ## call, names can turn out with multiple elements. We must avoid
    ## that.
    if(length(name.x)!=1) name.x <- "x"
    if(length(name.y)!=1) name.y <- "y"

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)

    dots <- list(...)
    names.dots <- names(dots)

    if(!xor(missing(by),missing(by.x)&&missing(by.y))){
        messageWrap("Either by or both by.x and by.y must be provided",fun.msg=stop)
    }
    if(!missing(by)) {
        by.x <- by
        by.y <- by
    }

    name.df3 <- "result"
    if("all"%in%names.dots) {
        messageWrap("option all not supported. mergeCheck is intended for merges that result in column additions to x, that's all.",
                    fun.msg=stop,track.msg=track.msg)
    }

### check if sort is in ... That cannot be passed.
    if("sort"%in%names.dots) messageWrap("sort is not meaningful in combination with mergeCheck because mergeCheck will preserve order of x. Please drop sort argument.",fun.msg=stop,track.msg=track.msg)

### x cannot be of nrow==0. There would be no way to add columns to a zero row data set
    if(nrow(x)==0) messageWrap(paste(name.x," has no rows. x must have existing rows so that values of the columns in by can be used for the merge."),fun.msg=stop,track.msg=track.msg)
    
    ## if data is not data.tables, convert to data.tables
    if(!is.data.frame(x)) stop("x must be a data.frame")
    if(!is.data.frame(y)) stop("y must be a data.frame")

    x.was.dt <- TRUE
    if(is.data.table(x)){
        ## will add a row counter to x, so better avoid editing by ref
        x <- copy(x)
    } else {
        x <- as.data.table(x)
        x.was.dt <- FALSE
    }
    
    ## y is not edited. So if already a data.table, we don't edit.
    if(!is.data.table(y)){
        y <- as.data.table(y)
    }

    ## check that by columns are 
    by.not.in.x <- setdiff(by.x,colnames(x))
    if(length(by.not.in.x)>0) stop(paste("This by.x column is not available in x:",paste(by.not.in.x,collapse=",")))
    by.not.in.y <- setdiff(by.y,colnames(y))
    if(length(by.not.in.y)>0) stop(paste("This by.y column is not available in y:",paste(by.not.in.y,collapse=",")))

    ## check for NA in by columns    
    nas.in.by.x <- x[,sapply(.SD,function(x)any(is.na(x))),.SDcols=by.x]
    nas.in.by.y <- y[,sapply(.SD,function(x)any(is.na(x))),.SDcols=by.y]
    
    if(any(nas.in.by.x & nas.in.by.y)){
        messageWrap("NA\'s found in matched by.x and in by.y column(s). This loosely speaking means, you are trying to merge on NA values. Double-check the columns you are merging by. If this is expected, you can use \'fun.na.by=NULL\' to allow it.",fun.msg=fun.na.by)
    }

    if(missing(subset.x)) subset.x <- NULL
    x.subsetx <- NULL
    if(!is.null(subset.x)){
        stop("subset.x argument not supported.")
        x.subsetx <- x[eval(parse(text=sprintf("!(%s)",subset.x)))]
        x <- x[eval(parse(text=subset.x))]
    }
    
    if(missing(as.fun)) as.fun <- NULL
    if(x.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    
#### Section start: Handle common columns not merged by ####

    cols.common.notby <- intersect(setdiff(colnames(x),by.x),setdiff(colnames(y),by.y))

    if(is.character(common.cols)){
        common.cols <- cleanSpaces(common.cols)
    }
    if(length(cols.common.notby) && is.character(common.cols) && common.cols=="drop.x") {

        x <- x[,setdiff(colnames(x),cols.common.notby),with=FALSE]
        cols.common.notby <- intersect(setdiff(colnames(x),by.x),setdiff(colnames(y),by.y))
    }
    if(length(cols.common.notby)&& is.character(common.cols) && common.cols=="drop.y") {
        y <- y[,setdiff(colnames(y),cols.common.notby),with=FALSE]
        cols.common.notby <- intersect(setdiff(colnames(x),by.x),setdiff(colnames(y),by.y))
    }
    if(length(cols.common.notby)&& is.character(common.cols) && common.cols=="merge.by") {
        by.x <- c(by.x,cols.common.notby)
        by.y <- c(by.y,cols.common.notby)
        cols.common.notby <- c()
    }
    
    commoncols.found <- FALSE
    fun.commoncols <- common.cols
    ## if common.cols was one of drop.x etc, that handling must be sufficient to avoid findings
if(is.character(fun.commoncols)) fun.commoncols <- stop
    
    if(length(cols.common.notby)) {
        messageWrap(paste0("x and y have common column names not being merged by. This will create new column names in output. Common but not merged by: ",paste(cols.common.notby,collapse=", "),"."),
                    fun.msg=fun.commoncols,track.msg=track.msg)
        commoncols.found <- TRUE
    }

###  Section end: Handle common columns not merged by
    
    rowcol <- tmpcol(names=c(colnames(x),colnames(y)))

    if(nrow(x)) {
        ## x is not NULL
        reorder <- TRUE
        x[,(rowcol):=.I]
    } else {
        reorder <- FALSE
    }
    
    df3 <- tryCatch(merge(x,y,by.x=by.x,by.y=by.y,sort=FALSE,...),error=identity)
    if("error"%in%class(df3)){        
        stop(paste0("Merge failed. This error was returned by merge.data.table:\n",df3$message))
    }

    rows.disappeared <- !(all(x[,get(rowcol)]%in%df3[,get(rowcol)]))
    rows.created <- !(all(df3[,get(rowcol)]%in%x[,get(rowcol)]))
### this has to be tested before activated 
    rows.dup  <- df3[,.N,by=rowcol][,any(N>1)]
    rows.number.changed <- nrow(x)!=nrow(df3)

    if(any(c(rows.disappeared,rows.created,rows.number.changed))){

        if(rows.number.changed && !rows.disappeared && !rows.created){
            messageWrap("Number of rows changed during merge.\n",fun.msg=message,track.msg=track.msg)
        }
        if(rows.disappeared) messageWrap("Rows disappeared during merge.",fun.msg=message,track.msg=track.msg)
        if(rows.dup) messageWrap("Rows duplicated during merge.",fun.msg=message,track.msg=track.msg)
        if(rows.created) messageWrap("New rows appeared during merge.",fun.msg=message,track.msg=track.msg)

        
        list.data.rep <- list(x[,setdiff(colnames(x),c(rowcol)),with=FALSE],y,df3[,setdiff(colnames(df3),c(rowcol)),with=FALSE])
        setNames(list.data.rep,c(setdiff(name.x,rowcol),name.y,name.df3))
        dims.rep <- dims(list.data=list.data.rep)
        
        message("Overview of dimensions of input and output data:\n",
                paste0(capture.output(dims.rep), collapse = "\n"),"\n"
                )
        
        dtcheck <- merge(x[,.N,by=by],
                         df3[,.N,by=by]
                        ,by=by.x,all=TRUE,suffixes=c(".x",".result")
                         )
        dtcheck[is.na(N.x),N.x:=0]
        dtcheck[is.na(N.result),N.result:=0]
        dtcheck <- dtcheck[N.x!=N.result]

        message("Overview of values of by where number of rows in x changes:\n",
                paste0(capture.output(dtcheck), collapse = "\n"))
        
        stop("Merge added and/or removed rows.")
    }

    
    if(reorder){
        df3 <- setorderv(df3,rowcol)
        df3[,(rowcol):=NULL]
    }
    colorder <- setdiff(colnames(x),rowcol)
    colorder <- intersect(colorder,colnames(df3))
    setcolorder(df3,colorder)
    
    newcols <- setdiff(colnames(df3),colnames(x))
### checking number of new columns
    if(!missing(ncols.expect)) {
        x[,(rowcol):=NULL]
        n.newcols <- ncol(df3)-ncol(x)
        if(n.newcols!=ncols.expect) {
            messageWrap(sprintf("Number of new columns (%d) does not mactch the expected (%d). New columns are: %s.",n.newcols,ncols.expect,paste(newcols,", ")),fun.msg=stop,track.msg=track.msg)
        }
    }

    #### todo: Move reorder after this. Make sure row identifier is added before splitting.
    if(!is.null(x.subsetx)){
        df3 <- rbind(x.subsetx,df3)
    }
    df3 <- as.fun(df3)

    if(!quiet){
        msg <- paste0("Column(s) added: ",paste(newcols,collapse=", "))
        message(msg)
    }
    
    df3
}
