
file <- "tests/testthat/testData/nonmem/xgxr021.lst"
lstReadTime(file)
file <- "tests/testthat/testData/nonmem/xgxr021.mod"
lstReadTime(file)


lstReadTime <- function(file){
    
    lines <- readLines(file)
    last.pre <- min(grep(" *\\$",lines))-1
    if(last.pre<1) return(NA)
    lines <- lines[1:last.pre]
    
    time.char <- lines[1]

    time.char <- sub("^... ","",time.char)
    tz <- sub(".+ ([^ ]+) [0-9]{4}$","\\1",time.char)
    timedate <- sub("(.+) [^ ]+ ([0-9]{4})$","\\1 \\2",time.char)
    time.file <- as.POSIXct(timedate,format="%b %d %H:%M:%S %Y",tz=tz)
    time.file

}


