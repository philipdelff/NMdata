library(NMdata)
library(devtools)
load_all()
load_all("~/wdirs/NMsim",export_all = FALSE)

## library(NMsim)

NMdataConf(path.nonmem="/opt/nonmem/nm751/run/nmfe75")
NMdataConf(dir.psn="/opt/psn")

NMexec("../tests/testthat/testData/nonmem/xgxr004.mod",sge=FALSE)
NMexec("../tests/testthat/testData/nonmem/xgxr014.mod",sge=FALSE)
NMexec("../tests/testthat/testData/nonmem/xgxr028.mod",sge=FALSE)
NMexec("../tests/testthat/testData/nonmem/xgxr033.mod",sge=FALSE)

NMexec("../inst/examples/nonmem/xgxr031.mod",sge=FALSE)
NMexec("../inst/examples/nonmem/xgxr032.mod",sge=FALSE)



### generating output tables with subproblems
library(NMsim)
dt.dos <- NMcreateDoses(TIME=0,AMT=100,CMT=1)
dt.sim <- addEVID2(dt.dos,time.sim=1:3,CMT=2,as.fun="data.table")
dt.sim[,ROW:=.I]
dt.sim[,BBW:=60]

getwd()
simres <- NMsim(file.mod="../tests/testthat/testData/nonmem/xgxr014.mod",
      data=dt.sim,
      dir.sims="../tests/testthat/testData/simulations",
      subproblems=4,
      name.sim="subprobs",
      path.nonmem="/opt/nonmem/nm751/run/nmfe75")

