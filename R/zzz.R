.NMdata <- new.env(parent = emptyenv())
.NMdata$options <- list()
NMdataConf(reset=TRUE)

.onAttach <- function(libname,pkgname){
    packageStartupMessage("Welcome to NMdata. Best place to browse NMdata documentation is\nhttps://philipdelff.github.io/NMdata/")
}
