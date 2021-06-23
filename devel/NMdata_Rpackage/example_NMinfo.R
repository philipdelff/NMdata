
## this is a nice NMinfo example
NMdataConf(check.time=T)
file.lst <- file.nm("xgxr021.lst")
res1  <- NMscanData(file.lst,merge.by.row=F)
NMinfo(res1)
## add NOMTIME=DROP ## notice gives an extra input column in summary
## add DV=CONC
inp1=NMscanInput(file.lst)
dat1=readRDS("C:/Users/delff/working_copies/NMdata/inst/examples/data/xgxr2.rds")
## this one is interesting too
res2 <- NMscanData(file.lst,merge.by.row=F,translate.input = F)
NMinfo(res2,"input.colnames")

install.packages("ggPMX")
library(ggPMX)
ctr <- pmx_nm(
    directory = file.nm(),
    file     = "xgxr021.lst" 
)
