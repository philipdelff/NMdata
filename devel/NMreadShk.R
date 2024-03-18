
## Type 1=etabar
## Type 2=Etabar SE
## Type 3=P val
## Type 4=%Eta shrinkage SD version
## Type 5=%EPS shrinkage SD version
## Type 6=%Eta shrinkage based on empirical Bayes Variance (SD version)
## Type 7=number of subjects used.
## Type 8=%Eta shrinkage variance version
## Type 9=%Eta shrinkage based on empirical Bayes Variance (variance version)
## Type 10=%EPS shrinkage variance version
## Type 11=%Relative information



file.shk <- fnExtension(file.lst,"shk")
lines.shk <- readLines(file.shk)
idx.tabstart <- grep("^TABLE NO",lines.shk)
idx.tabstart
dt.ts2 <- data.table(idx=c(idx.tabstart,length(lines.shk)+1))
dt.ts3 <- data.table(start=dt.ts2[-.N],end=dt.ts2[-1]-1)
dt.ts3[,tableno:=sub(" *TABLE NO\\. +([1-9][0-9]*).*","\\1",lines.shk[start.idx])]
dt.ts3
## dt.ts3[,fread(text=lines.shk[(start.idx+1):end.idx]),by=tableno]
list.shk <- lapply(
    split(dt.ts3,by="tableno")
      ,function(x)fread(text=x[,lines.shk[(start.idx+1):end.idx]]))
lapply(names(list.shk),function(name)list.shk[[name]][,tableno:=name])

shk <- list.shk[[length(list.shk)]]
shk

