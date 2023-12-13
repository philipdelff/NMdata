finds <- NMcheckData(dat,col.time="TSFD",col.dv="DV1",col.mdv="MDV1")

getFindings <- function(finds,data,check1){
### only one check allowed at a time for now
    
    finds.rel <- finds[check==check1]
    if(nrow(finds.rel)==0) {
        message("No findings for that check")
        return(NULL)
    }
    level <- finds.rel[,unique(level)]

    if(level=="row"){
        cbind(
            ## finds[check=="Duplicated event"]
            finds.rel
           ,
            data[finds.rel[,row]]
        )
    }
}

## this is weird. Could these observations be duplicated from obs records?
getFindings(finds,data=dat,check1="DV not NA in dosing recs")
