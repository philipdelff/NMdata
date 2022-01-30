##' Expand grid of data.tables
##'
##' @param dt1 a data.table.
##' @param dt2 another data.table.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @details Merging works mostly similarly for data.table and
##'     data.table. However, for data.table the merge must be done by
##'     one or more columns. This means that the convenient way to
##'     expand all combinations of all rows in two data.frames is not
##'     available for data.tables. This functions provides that
##'     functionality. It always returns data.tables.
##' @return a data.table that expands combinations of rows in dt1 and
##'     dt2.
##' @examples
##' df1 <- data.frame(a=1:2,b=3:4)
##' df2 <- data.frame(c=5:6,d=7:8)
##' merge(df1,df2)
##' library(data.table)
##' ## This is not possible
##' \dontrun{
##' merge(as.data.table(df1),as.data.table(df2),allow.cartesian=TRUE)
##' }
##' ## Use egdt instead
##' egdt(as.data.table(df1),as.data.table(df2))
##' @import data.table

##' @export

egdt <- function(dt1,dt2,quiet){

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    
    dt1 <- copy(as.data.table(dt1))
    dt2 <- copy(as.data.table(dt2))
    ## check for common columns
    cols.common <- intersect(colnames(dt1),colnames(dt2))
    if(length(cols.common)>0) messageWrap("common columns in dt1 and dt2. If this is intended, it is beyond the scope of egdt. If you still want to use egdt, you can rename the common column names first.",fun.msg=stop)
    tc <- tmpcol(names=c(colnames(dt1),colnames(dt2)))

    dt1[,(tc):=1]
    dt2[,(tc):=1]

    result <- merge(dt1,dt2,by=tc,allow.cartesian = TRUE)
    result[,(tc):=NULL]

    if(!quiet){
        dt1[,(tc):=NULL]
        dt2[,(tc):=NULL]    
        print(dims(dt1,dt2,result))
    }
    result[]
}
