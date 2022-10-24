### this is run every time library(NMdata) or require(NMdata) is
### called.

.NMdata <- new.env(parent = emptyenv())
.NMdata$options <- list()
NMdataConf(reset=TRUE)

##' @importFrom utils packageVersion
.onAttach <- function(libname,pkgname){
    packageStartupMessage(paste0("Welcome to NMdata ",packageVersion("NMdata"),". Best place to browse NMdata documentation is\nhttps://philipdelff.github.io/NMdata/"))
}
