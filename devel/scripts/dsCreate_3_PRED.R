library(data.table)
file.data.test <- function(...) file.path("../../tests/testthat/testData/data",...)

script.3 <- "dsCreate_3_PRED.R"

dat <- data.table(ID=rep(1:10,each=3))
dat[,X:=rnorm(n=.N)][
   ,Y:=.4-.28*X+rlnorm(n=.N,meanlog=.7)
]


dat


## save dat with modified name for ID
setnames(dat,"ID","SUBJ")

NMwriteData(dat,args.NMgenText = list(copy=c(ID="SUBJ"))
           ,file=file.data.test("pred_data1.csv")
           ,script=script.3
           ,args.rds=list(version=2))

