

library(devtools)

load_all()

res.test <- test()
res.chek <- check()


devtools::build_readme()
setwd("~/wdirs/NMdata")
library(rmarkdown)
render("NEWS.md")

library(pkgdown)
pkgdown::build_news()
