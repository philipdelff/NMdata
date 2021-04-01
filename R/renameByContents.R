
### don't export yet.

## renames column names based on column contents.
## should check whether new column names are in use already
renameByContents <- function(data,fun.test,fun.rename){

    dt <- as.data.table(data)
    dt.res <- dt[,lapply(.SD,fun.test)]
    dt.res.l <- melt(dt.res,measure.vars=colnames(dt.res))
    cols.to.rename <- dt.res.l[value==FALSE,as.character(variable)]
    setnames(dt,cols.to.rename,fun.rename)
    return(dt)
}
