##' Read in data file
##'
##' @param file The .cov covariance Nonmem matrix file to read
##' @param auto.ext If `TRUE` (default) the extension will automatically
##'     be modified using `NMdataConf()$file.cov`. This means `file`
##'     can be the path to an input or output control stream, and
##'     `NMreadCov()` will still read the `.cov` file.
##' @param ... Passed to fread
##'
##' @details This function is taken from nonmem2rx::nmcov which was
##'     based on NMdata::NMreadTab.
##' 
##' @return A matrix with covariance step from NONMEM
##' 
##' @author Philip Delff and Matthew L. Fidler
##' 
##' @export



NMreadCov <- function (file,auto.ext, ...) {


    if(missing(auto.ext) || is.null(auto.ext)) auto.ext <- TRUE
    fun.file.cov <- NMdataDecideOption("file.cov")
    if(auto.ext){
        file <- fun.file.cov(file)
    }


    if(!file.exists(file)){stop("file does not exist.")}
    TABLE <- NULL
    NMREP <- NULL
    NAME <- NULL
    
    colnames <- readLines(file, n=2)[2]
    if (grepl(", *OMEGA\\( *1 *, *1\\)", colnames)) {
        ## in this case the NAME also has commas
        lines <- readLines(file)
        lines <- gsub("(OMEGA|SIGMA)[(]([0-9]+),([0-9]+)[)]", "\\1\\2AAAAAAAAA\\3", lines)
        dt1 <- fread(text=lines, fill = TRUE, header = TRUE, skip = 1, sep=",", ...)
        dt1[,NAME:=gsub("[A]([0-9]+)AAAAAAAAA([0-9]+)", "A(\\1,\\2)",NAME)]
        setnames(dt1, function(x)gsub("[A]([0-9]+)AAAAAAAAA([0-9]+)", "A(\\1,\\2)", x))
    } else {
        dt1 <- fread(file, fill = TRUE, header = TRUE, skip = 1, 
                     ...)
    }
    cnames <- colnames(dt1)
    dt1[grep("^TABLE", as.character(get(cnames[1])), invert = FALSE, 
             perl = TRUE), `:=`(TABLE, get(cnames[1]))]
    dt1[, `:=`(NMREP, cumsum(!is.na(TABLE)) + 1)]
    dt1[, `:=`(TABLE, NULL)]
    dt1 <- dt1[NMREP==1,]
    name <- dt1$NAME
    dt1[,`:=`(NAME, NULL)]
    dt1[, `:=`(NMREP, NULL),]
    dt1 <- dt1[,name, with=FALSE]
    cnames <- colnames(dt1)
    dt1[, `:=`((cnames), lapply(.SD, as.numeric))]
    dt1 <- as.matrix(dt1)
    dn <- dimnames(dt1)
    dn[[1]] <- dn[[2]]
    dimnames(dt1) <- dn
    return(dt1)
}
