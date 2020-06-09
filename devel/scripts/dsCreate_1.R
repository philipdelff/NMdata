## install.packages("xgxr")
library(xgxr)
library(data.table)
library(ggplot2)

### what is pmxtricks used for?
## we need to ensure a specific version of pmxtricks is used. 0.0.7 is a candidate

library(pmxtricks)

## library(remotes)
## install_github("philipdelff/NMdata")
library(NMdata)

pkpd <- as.data.table(case1_pkpd)

pk <- pkpd[CMT %in% 1:2] 

pk <- pk[CYCLE==1]

pk <- pk[,!c("IPRED")]
pk[,trtact := factor(TRTACT, levels = unique(TRTACT))]


ggplot(data = pk, aes(x     = NOMTIME,
                      y     = LIDV,
                      group = DOSE
                     ,color = trtact)
       ) +
                             xgx_geom_ci(conf_level = 0.95) +
                             xgx_scale_y_log10() 


ggplot(data = pk, aes(x     = TIME,
                      y     = LIDV,
                      group = ID
                     ,color = trtact)
       ) +
    geom_line()+geom_point()+
    xgx_scale_y_log10() +
    facet_wrap(~trtact)
    

## rename
setnames(pk,
         old=c("LIDV","CENS"),
         new=c("DV","BLQ")
         )


pk
pk[,table(CMT,EVID)]

pk[ID==1]
indprofs <- ggIndProfs(pk,amt="AMT")
ggwrite(indprofs,file="indprofs.pdf",onefile=T)

pk <- pk[DOSE>0]
pk <- NMorderColumns(pk)

dim(pk)

NMwriteData(pk,file="../../inst/examples/data/xgxr1.csv")
