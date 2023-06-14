

##' Replace strings in character character columns of a data set
##'
##' @param data The data set to edit.
##' @param pattern Pattern to search for in character columns. Passed
##'     to `gsub()`. By default, `gsub()` works with regular
##'     expressions. See ... for how to disable this if you want to
##'     replace a specific string.
##' @param replacement pattern or string to replace with. Passed to
##'     `gsub()`.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param ... Additional arguments passed to `gsub()`. Especially,
##'     notice fixed=TRUE will disable interpretation of `pattern` and
##'     `replace` as regular expressions.
##' @examples
##' ### remove commas from character columns 
##' dat <- data.frame(A=1:3,text=cc(a,"a,d","g"))
##' editCharCols(dat,pattern=",","")
##' ### factors are not edited but result in an error
##'\dontrun{
##' dat <- data.frame(A=1:3,text=cc(a,"a,d",g),fac=cl("a","a,d","g"))
##' editCharCols(dat,pattern=",","")
##' }
##' 
##' @export



editCharCols <- function(data,pattern,replacement,as.fun,...){
    
#### Section start: Pre-process arguments ####

    ## make sure to work on a copy of data and not edit by ref
    data.was.dt <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)
        data.was.dt <- FALSE
    }

    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    if(data.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

###  Section end: Pre-process arguments
    
    ## identify columns we want to search for string
    cnames.char <- colnames(data)[ !sapply(data,function(x)NMisNumeric(x)||"POSIXct"%in%class(x)||is.factor(x))]

    ## character columns are edited removing 'string' from elements (contents of columns, not names)
    data[,(cnames.char):=lapply(.SD,function(x)gsub(pattern,replacement,x,...)),.SDcols=cnames.char]

    ## factors are checked for commas in strings but not edited
    cnames.fac <- colnames(data)[ !sapply(data,function(x)NMisNumeric(x)||"POSIXct"%in%class(x))&sapply(data,function(x)is.factor(x))]
    if(length(cnames.fac)){
        
        factors.found <- cnames.fac[
            data[,sapply(.SD,function(x)any(grepl(pattern,x))),.SDcols=cnames.fac]
        ]
        if(length(factors.found)){
            stop(paste("Pattern found in factors. These have NOT been removed. Concerned columns:",factors.found))
        }
    }

    as.fun(data)
}
