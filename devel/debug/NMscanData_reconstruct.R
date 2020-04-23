### 2020-04-22 issue resolved. 

library(devtools)
load_all("c:/Users/kctw748/working_copies/NMdata")

dres1 <- NMscanData("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/Models/BaseModel/run7211.lst")

dres2 <- NMscanData("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/Models/BaseModel/run7211.lst",recoverRows = T)

dim(dres1$row)
dim(dres2$row)

## this is obviously wrong
head(dres2$row$ROW)

## seems like all rows are added. So the recovered are there, but the ones that
## didnt need to be recovered are duplicated.
dres2$row[1:5]

dres2 <- NMscanData("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/Models/BaseModel/run7211.lst",recoverRows = T,debug=F)
dim(dres2$row)

indat <- NMreadCsv("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/DerivedData/NM_PK_recoded_v2.csv")
dim(indat)
