execsafe <- function(file.mod,sge=TRUE,nc=64){
    
    library(NMdata)
    source("~/wdirs/tracee/R/fnAppend.R")

    ## path.input <- NMextractDataFile(file.mod)
    file.data.archive <- function(file){
        fn.input <- fnAppend(file,"input")
        fn.input <- fnExtension(fn.input,".rds")
        fn.input
    } 
    fn.input <- file.data.archive(file.mod)
    ## replace extension of fn.input based on path.input - prefer rds
    rundir <- dirname(file.mod)
    ## copy input data
    dat.inp <- NMscanInput(file.mod,translate=FALSE,applyFilters = FALSE,file.data="extract")
    saveRDS(dat.inp,file=file.path(rundir,basename(fn.input)))

    system(
        paste0("cd ",rundir,"; execute ",basename(file.mod)," &")
        )
    }
}


### testing models
dir.models <- "tests/testthat/testData/nonmem"
mods <- list.files(dir.models,pattern="\\.mod")

lapply (mods,function(mod){
system(
        paste0("cd ",dir.models,"; execute ",mod," &")
)
})

