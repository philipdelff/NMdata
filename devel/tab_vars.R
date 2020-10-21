file.lst <- NMdata_filepath("examples/nonmem/xgxr004.lst")
file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")

res <- NMscanData(file=file.lst,cbind.by.filters = T)

vars=attr(res,"vars")[order(var)]
vars[,dt:="vars"]
dt.vars=attr(res,"dt.vars")[included==TRUE][order(variable)]
dt.vars[,dt:="dt.vars"]
setnames(vars,"var","variable")

dim(vars)
dim(dt.vars)

merge(vars[,.(variable,dt)],dt.vars[,.(variable,dt)],by="variable",all=T)


####
attr(res,"dt.vars")


tabs <- NMscanTables(file=file.lst)






load_all("../..")
file.lst <- NMdata_filepath("examples/nonmem/xgxr003.lst")
res <- NMscanData(file=file.lst,cbind.by.filters = T)
attributes(res)
summary(res)

inp <- NMtransInput(file.lst)
colnames(inp)
