## 

specsDefault <- fread(text="variable,desc,unit,var.char,col.unit,values
AMT,Dose amount,,amt.unit
CMT,Compartment identifier
DV,Observed value,,dv.unit
EVID,Event identifier
ID,Nonmem subject ID
MDV,Missing dependent variable specifier,0/1
TIME,Actual time
RATE,Infusion rate
II,Dose repetition interval
ADDL,Number of dose repetitions
AGE,Age,years
BMI,Body mass index,kg/m2
WT,Body weight,kg
BLQ,Below LLOQ indicator,0/1
"
## add in unit.amt
## unit.dv should go on DV and LLOQ
## col.usubjid can be var.char for ID
## time unit on TIME, II

)


specsInit <- function(data,stamp, analysis=NULL,header=NULL,use.standard.cols=T,time.unit="h",dv.unit=NULL,amt.unit=NULL,ndos.unit=NULL,debug=F){

    if(debug){browser()}

#### check arguments
    if(!is.null(analysis)) stopifnot(is.character(analysis))


    ## type variable descrip unit/possible vals
    meta <- list(DataCreateScript=stamp,
                 Analysis=analysis)
    meta.data <- list(meta=meta)

    if(!is.null(header)) meta.data$meta <- metaAdd(data=data,meta.data=meta.data,header=header)$meta
    
    if(!use.standard.cols) return(list(meta=meta))
    
    datacols <- colnames(data)

    specs <- specsDefault
    
}

    
