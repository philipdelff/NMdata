

res1.m <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),
                     quiet=TRUE,recover.rows=T,modelname="Two compartments")
## using a custom modelname for this model
res2.m <- NMscanData(system.file("examples/nonmem/xgxr014.lst", package="NMdata"),
                     modelname="One compartment",
                     quiet=TRUE)
res2.m[,DV:=NA]
## notice fill is an option to rbind with data.table (like bind_rows in dplyr)
res.mult <- rbind(res1.m,res2.m,fill=T)
## Notice, the NMdata class disappeared
class(res.mult)
res.mult[EVID==0&nmout==TRUE,
         gmPRED:=exp(mean(log(PRED))),
         by=.(model,trtact,NOMTIME)]


## ggplot(res.mult[EVID==0],aes(NOMTIME,gmPRED))+
##     geom_point(aes(TIME,DV,shape=flag),colour="grey")+
##     geom_line(data=function(x)x[EVID==0&nmout==TRUE],aes(colour=model),size=1.2)+
##     scale_y_log10()+
##     facet_wrap(~trtact,scales="free_y")+
##     labs(x="Hours since administration",
##          y="Concentration (ng/mL)",
##          subtitle="data: res.mult. Lines are gmPRED at output observation times.")+
##     scale_shape_manual(values=c(1,3,4))



p1 <- ggplot(res.mult[EVID==0],aes(NOMTIME,gmPRED))+
    geom_point(aes(TIME,DV,colour=flag),shape=1)+
    geom_line(data=function(x)x[EVID==0&nmout==TRUE],aes(colour=model),size=1.2)+
    scale_y_log10()+
    facet_wrap(~trtact,scales="free_y")+
    labs(x="Hours since administration"
        ,y="Concentration (ng/mL)"
         ## ,subtitle="data: res.mult. Lines are gmPRED at output observation times."
        ,colour="")+
    scale_colour_manual(breaks=c("Analysis set","Below LLOQ","Pre-dose sample","One compartment","Two compartments"),values = c(1:5),
                        guide = guide_legend(override.aes = list(
                                                 linetype = c(rep("blank", 3),"solid","solid"),
                                                 shape=c(1,1,1,NA,NA)
                                             )))

## install.packages("devtools")
library(devtools)
## install_github("philipdelff/pmxtricks")
library(pmxtricks)

ggwrite(p1,file="geomean_plot1.png")
getwd()
