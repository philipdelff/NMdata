
load_all()
lsts <- list.files(system.file("examples/nonmem",package="NMdata"),pattern="xgxr.*\\.lst$",full.names=TRUE)

## backup
## for(lst in lsts){
##     file.copy(lst,file.path("~/tmp",basename(lst)))
## }



### complete
for (lst in lsts){

    lines <- readLines(lst)
    ## until "$PROBLEM"
    lines <- lines[-max(1,grep(" *\\$PROBLEM.*",x=lines)[1]-1)]

    ## keep only control stream part
    block.start <- grep("^ *NM-TRAN MESSAGES",lines)
    if(length(block.start) ){
        lines <- lines[1:(block.start-1)]
    }
    writeLines(lines,con=lst)
}



### only partial stripping
if(F){

    for (lst in lsts){

        lines <- readLines(lst)
        ## until "$PROBLEM"
        lines <- lines[-max(1,grep(" *\\$PROBLEM.*",x=lines)[1]-1)]

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
