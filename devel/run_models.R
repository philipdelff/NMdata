library(devtools)
load_all()
load_all("~/wdirs/NMsim",export_all = FALSE)

## library(NMsim)

NMdataConf(path.nonmem="/opt/nonmem/nm751/run/nmfe75")
NMdataConf(dir.psn="/opt/psn")

NMexec("../tests/testthat/testData/nonmem/xgxr028.mod",sge=FALSE)

NMexec("../inst/examples/nonmem/xgxr031.mod",sge=FALSE)
NMexec("../inst/examples/nonmem/xgxr032.mod",sge=FALSE)
