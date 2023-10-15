dir.r <- "~/wdirs/NMdata/R"
files.r <- list.files(dir.r,pattern=".*\\.r",full.names=T,ignore.case=TRUE)

dt.fams <- lapply(files.r,function(file){
    lines <- readLines(file)
    dt <- data.table(file=basename(file),line.fam=lines[grepl(".*family.*",lines)])
    dt[,family:=sub(".*family *","",line.fam)]
}
) |> rbindlist()

dt.fams[,.N,by=.(family)]
