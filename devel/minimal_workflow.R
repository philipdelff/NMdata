### Load NMdata and make it return tibbles
library(NMdata)
NMdataConf(as.fun=tibble::as_tibble)
## By default NMdata looks for a row identifier (most often but not necessarily a row counter) called ROW. Say you like calling it RNUM:
NMdataConf(col.row="RNUM")

### say you have prepared a dataset called pkpd

## write to csv makings sure it is compatible with nonmem:
nmtext <- NMwriteData(pkpd,file="file.csv")

#### update $INPUT in your control streams to match the new data columns:
## NMwriteData suggested some text for $INPUT and $DATA. You probably don't want to update $DATA. The following updates $INPUT only:
NMwriteSection(dir="nonmem",
               file.pattern="^run.*\\.mod",
               list.sections=text.nm["INPUT"])
}

## You have to tell NMwriteData if you want to use =DROP, use different column names or want to use aliases (like DV=CONC). You can either tailor this using the NMgenText function (especially if you use the data differently in different models, like using different columns as DV), or have NMwriteData pass the options to NMgenText. Example:
nmtext <- NMwriteData(pkpd,file="file.csv",args.NMgenText=list(copy=c(DV="CONC"),drop="RACECHAR"))


### Now go run Nonmem

### Welcome back to NMdata. Get the data back (combine all $TABLE and input data):

res1 <- NMscanData("nonmem/run.lst")

### NMscanData by default looks for the row identifier specified in NMdataConf(col.row="RNUM") and if it finds it uses it to _merge_ input and output. You can explicitly tell it to interpret IGNORE and ACCEPT statements in your control stream and then cbind instead (I wouldn't):

res1 <- NMscanData("nonmem/run.lst",merge.by.row=FALSE)

### If you want to "recover" rows that were discarded (by ACCEPT/IGNORE) and hence are not found in $TABLE:
res1 <- NMscanData("nonmem/run.lst",recover.rows=TRUE)

## you are ready to plot against character vars etc

###### additional tricks
## Most often, I want to look at parameters that don't vary within the dataset (say typical values):
fixedpars <- findCovs(res1)

## they may vary between studies though:
fixedpars <- findCovs(res1,by="STUDY")

## then values that don't vary within ID:
fixedpars <- findCovs(res1,by="ID")

### Advanced (but very useful for automated generation of plots and tables): sometimes I want to find variables that actually varies (say discard ETA's that are zero anyway):
varpars <- findVars(res1)
## Again, those that vary wind subjects 
ivarpars <- findVars(res1, by="ID")


## Befaor saving the dataset for nonmem, you may want to Order columns in a predefined way (some customization available in arguments - ideas to how this fits in with your preferences appreciated). I always do this (and NMscanData does this by default before returning the results if you don't tell it not to):

pkpd <- NMorderColumns(pkpd)
