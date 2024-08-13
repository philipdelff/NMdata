
##' Read Nonmem table files without assumptions about what tables they
##' contain
##'
##' @param file A Nonmem table file. Can be output tables, or one of
##'     the several different results files from Nonmem.
##' @import data.table
##' @details `NMreadTabSlow` reads parameter tables from Nonmem very
##'     slowly, and most often `NMreadTab` is a better function to
##'     use. However, `NMreadTabslow` also works for table files that
##'     contain incompatible tables.
##' @keywords internal
##' 
### Polishing needed. Don't export.

NMreadTabSlow <- function(file){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    tableno <- NULL
    end.idx <- NULL
    start.idx <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    lines <- readLines(file)
    idx.tabstart <- grep("^TABLE NO",lines)
    dt.ts2 <- data.table(idx=c(idx.tabstart,length(lines)+1))
    dt.ts3 <- data.table(start=dt.ts2[-.N],end=dt.ts2[-1]-1)
    dt.ts3[,tableno:=sub(" *TABLE NO\\. +([1-9][0-9]*).*","\\1",lines[start.idx])]
    dt.ts3
    ## dt.ts3[,fread(text=lines[(start.idx+1):end.idx]),by=tableno]
    list.res <- lapply(
        split(dt.ts3,by="tableno")
       ,function(x)fread(text=x[,lines[(start.idx+1):end.idx]]))
    lapply(names(list.res),function(name)list.res[[name]][,tableno:=name])

    list.res
}
