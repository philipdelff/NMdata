## library(devtools)
load_all()

setwd("../../tests/testthat/")
## lsts1 <- list.files(system.file("examples/nonmem",package="NMdata"),pattern="xgxr.*\\.lst$",full.names=TRUE)
## lsts2 <- list.files(system.file("examples/nonmem/xgxr001dir",package="NMdata"),pattern="output*\\.txt$",full.names=TRUE)
## lsts1 <- list.files("../../inst/examples/nonmem",pattern="xgxr.*\\.lst$",full.names=TRUE)
## lsts2 <- list.files("../../inst/examples/nonmem/xgxr001dir",pattern="output*\\.txt$",full.names=TRUE)
lsts3 <- list.files("testData/nonmem",pattern=".*\\.lst$",full.names=TRUE)
lsts4 <- list.files("testData/nonmem/xgxr001dir",pattern="output*\\.txt$",full.names=TRUE)

## lsts1
## lsts2
lsts3
lsts4

## backup
## for(lst in lsts){
##     file.copy(lst,file.path("~/tmp",basename(lst)))
## }

lsts  <- c(## lsts1,lsts2,
    lsts3,lsts4)

### complete stripping of estimation information
for (lst in lsts){

    lines <- readLines(lst)
    ## discard lines until "$PROBLEM" - time stamp
    ## lines <- lines[-max(1,grep(" *\\$PROBLEM.*",x=lines)[1]-1)]

    ## keep only control stream part
    block.start <- grep("^ *NM-TRAN MESSAGES",lines)
    if(length(block.start) ){
        lines <- lines[1:(block.start-1)]
        writeLines(lines,con=lst)
        cat(lst,"updated\n" )
    } else {
        cat("Nothing to be done\n")
    }

}

## copy test cases to examples
runs <- cc(xgxr001, xgxr002,xgxr003,  xgxr014,xgxr018)
lsts.ex <- paste0(runs,".lst")

dir.mods <- "../../tests/testthat/testData/nonmem"
exts <- cc(.mod,.lst,.phi)
files1 <- c(outer(runs,exts,fnExtension))

NMdataConf(as.fun="data.table")
tabs <- lapply(file.path(dir.mods,lsts.ex),NMscanTables,details=T)
tabs <- unlist(lapply(tabs,function(x)x$meta[,name]))

all.files <- c(files1,tabs)
lapply(file.path(dir.mods,all.files),file.copy,to="../../inst/examples/nonmem/",overwrite=T)



## xgxr001dir
dir.mod <- file.path(dir.mods,"xgxr001dir")
files.dir <- cc(input.txt,
   input.ext
   ,input.phi
   ,output.txt
   ,xgxr001_res.txt
)
lapply(file.path(dir.mod,files.dir),file.copy,to="../../inst/examples/nonmem/",overwrite=T)

### only stripping of info that can make trouble
if(F){

    for (lst in lsts){

        lines <- readLines(lst)
        ## until "$PROBLEM" - timestamp
        ## lines <- lines[-max(1,grep(" *\\$PROBLEM.*",x=lines)[1]-1)]

        ## remove block
        block.start <- grep("^License Registered to.*",lines)
        block.end <- grep("^1NONLINEAR MIXED EFFECTS.*",lines)-1
        if(length(block.start) && length(block.end)){
            lines <- lines[-(block.start:block.end)]

            ## disregard from "Stop Time:"
            lines <- lines[1:(min(grep("^ *Stop Time.*",lines))-1)]

            writeLines(lines,con=lst)
        }
    }

}
