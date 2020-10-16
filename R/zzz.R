## .onLoad <- function(){

##     message("Welcome to NMdata. Best place to browse documentation is https://philipdelff.github.io/NMdata")

## }

.onAttach <- function(libname,pkgname){
    packageStartupMessage("Best place to browse NMdata documentation is https://philipdelff.github.io/NMdata")
}
