library(devtools)
load_all()
NMdataConf(as.fun="data.table")

file.ext <- "../tests/testthat/testData/nonmem/xgxr032.ext"

### if just NMREP was called TABLENO this would make good sense.
## would be good if we could get some table names. Recognice FOCE, SAEM, IMP and make a tablename column
res.dt <- NMreadExt(file=file.ext,as.fun="data.table",return="all")

library(ggplot2)
ggplot(res.dt$iterations,aes(ITERATION,value))+
    geom_point()+
    facet_wrap(~variable+NMREP)


ggplot(res.dt$iterations[NMREP==2],aes(ITERATION,value))+
    geom_point()+
    facet_wrap(~variable+NMREP,scales="free")



### OK - Phi should have a par.type like exts get
load_all()
NMdataConf(as.fun="data.table")

## Where to take phis from?
res.phi <- NMreadPhi(fnExtension(file.ext,"phi"))
head(res.phi)
res.phi[,.N,by=.(parameter,NMREP)]
res.phi[SUBJECT_NO==1]

## it doesn't really matter where we take PHI's from
dcast(res.phi[par.type=="PHI"],model+ID+parameter~NMREP,value.var="value") |>
    ggplot(aes(`1`,`2`))+geom_point()+
    facet_wrap(~parameter)
