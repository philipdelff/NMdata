library(data.table)

dat <- data.table(ID=rep(1:10,each=3))
dat[,x:=rnorm(n=.N)][
   ,y:=.4-.28*x+rlnorm(n=.N,meanlog=.7)
]


dat

## save dat with modified name for ID
