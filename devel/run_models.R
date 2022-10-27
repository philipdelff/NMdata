library(devtools)
load_all("~/wdirs/NMexec",export_all = FALSE)

NMexec("../tests/testthat/testData/nonmem/xgxr028.mod",sge=FALSE)
