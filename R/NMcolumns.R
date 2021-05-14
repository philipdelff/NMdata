##' @export

NMcolumns <- function(data,as.fun){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    as.fun(attributes(data)$meta$variables)

}

