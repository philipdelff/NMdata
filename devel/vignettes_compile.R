### vignettes are too large to be included on CRAN. I think because they are in .Rbuildignore devtools::document does not build them. I don't quite understand that because Roxygen is configured to build them in DESCRIPTION.  Anyway, I had to run this to update them. 

dir.vignettes <- "~/wdirs/NMdata/vignettes"
setwd(dir.vignettes)

library(NMdata)
library(rmarkdown)

names.vignettes <- cc(DataPrepare,"NMdata-cheat","NMdata-FAQ",NMscanData)

for(fn in names.vignettes){
    render(fnExtension(fn,"Rmd"))
}

