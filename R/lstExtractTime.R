##' Extract run time from output control stream
##'
##' @param file path to output control stream
##'
##' @return A POSIXct date-time object
##' @examples
##' file <- system.file("examples/nonmem/xgxr003.lst",package="NMdata")
##' NMdata:::lstExtractTime(file)
##' file <- system.file("examples/nonmem/xgxr003.mod",package="NMdata")
##' NMdata:::lstExtractTime(file)
##' 
##' all.lsts <- list.files(
##'   system.file("examples/nonmem",package="NMdata"),
##'   pattern="\\.lst",full.names=TRUE)
##' lapply(all.lsts,NMdata:::lstExtractTime)
##' @keyword internal

lstExtractTime <- function(file){

    ### the time stamp must be before any $ blocks
    lines <- readLines(file)
    last.pre <- min(grep(" *\\$",lines))-1
    if(last.pre<1) return(NA)
    lines <- lines[1:last.pre]

    ## must be first line
    time.char <- lines[1]

    ## extract timestamp and time zone
    time.char <- sub("^... ","",time.char)
    tz <- sub(".+ ([^ ]+) [0-9]{4}$","\\1",time.char)
    timedate <- sub("(.+) [^ ]+ ([0-9]{4})$","\\1 \\2",time.char)
    time.file <- as.POSIXct(timedate,format="%b %d %H:%M:%S %Y",tz=tz)
    time.file

}
