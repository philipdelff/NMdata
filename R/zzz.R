
.onAttach <- function(libname,pkgname){
    packageStartupMessage("Welcome to NMdata. Best place to browse NMdata documentation is\nhttps://philipdelff.github.io/NMdata
NMdata now generally returns data.frames by default. Version 0.0.6 was the last time NMdata returned data.table objects by default. In order to change this behaviour, see ?runAsFun and/or see the FAQ.")
}
