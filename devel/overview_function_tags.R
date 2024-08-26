library(stringi)

dir.r <- "~/wdirs/NMdata/tests/testthat"
files.r <- list.files(dir.r,pattern=".*\\.r",full.names=T,ignore.case=TRUE)
fdt <- data.table(file=files.r)

fun.grab.model <- function(file){ 
    
    lines <- try(readLines(file))
    if("try-error" %in%class(lines)){
        return(NULL)
    }                
    
    dt.lines <- data.table(file=file,code=lines)
    
    dt.lines[,ROW:=.I]
    dt.lines <- dt.lines[grepl(".*xgxr...\\..*",code)]
    if(nrow(dt.lines)==0) return(NULL)
    
    ## dt.lines[,file=basename(file),line.model=lines[grepl(".*xgxr???\\..*",lines)])
    dt.lines[,file.model:=stri_extract_all_regex(code,pattern="xgxr...\\....")]
    dt.lines[,fnExt:=fnExtension(file.model)]
    dt.lines[,run:=fnExtension(file.model,"")]

    dt.lines[]
}

file <- "/data/home/philipde/wdirs/NMdata/tests/testthat/test_addTAPD.R"
res <- fun.grab.model(file="/data/home/philipde/wdirs/NMdata/tests/testthat/test_NMscanTables.R")


## dir.r <- "~/wdirs/NMdata/tests/testthat"
dir.r <- "~/wdirs/NMdata/R"
files.r <- list.files(dir.r,pattern=".*\\.r",full.names=T,ignore.case=TRUE)
fdt <- data.table(file=files.r)


odt.fams <- lapply(files.r,fun.grab.model)
dt.calls <- rbindlist(odt.fams)
dt.calls
dt.calls[,.N,by=.(run)][order(N)]
dt.calls[,.N,by=.(run,fnExt)][order(N)]

## dir.inst <- "../tests/testthat/testData/nonmem"
dir.inst <- "../inst/examples/nonmem"
dt.found <- data.table(file.lst=list.files(dir.inst,pattern=".*\\.lst"))[
   ,run:=fnExtension(file.lst,"")][
   ,found:=TRUE]

dt.cf <- merge(dt.found,dt.calls,by="run",all=T)

dt.cf[,.N,by=.(run,found)][order(N)]
## dt.cf[,.N,by=.(run,fnExt,found)][order(N)]


dt.cf[run=="xgxr002"]
## trimmed to only mod and ext
dt.cf[run=="xgxr031"]
## left out by rbuildignore
dt.cf[run=="xgxr005"]

## try omit 002 014 
