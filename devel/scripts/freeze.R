
## Still some ways to go, but this script performs all needed steps in a freeze.

##### Files starting in the same name as the .mod are copied restab001.csv will not be copied if control stream is run001.mod. 

## decide what models use the specified data file
### this is no longer a needed argument
## file.data <- "../derived/cpt14.csv"
dir.freeze <- "../../models/cpt/freeze_210421"

dt.models <- data.table(model=list.files("../../models/cpt",pattern="^cpt.+\\.mod$",full.names=T))
dt.models[,ROW:=1:.N]
dt.models <- dt.models[,NMextractDataFile(model),by=.(ROW,model)]

## add the path exactly as the string in the .mod file

unique(dt.models[,.(path,path.rds)])

dt.models[,unique(path,path.rds)]

## this is the old version of the data without baseline observations. We will skip the models based on this data.
dt.models[path=="../../models/cpt/../../scripts_and_output/derived/cold_pressor_pred.csv"]
dt.models <- dt.models[path=="../../models/cpt/../../scripts_and_output/derived/cpt14.csv"]

unique(dt.models[,.(path,path.rds)])

## for now, only one data file is supported
stopifnot(nrow(
    unique(dt.models[,.(path,path.rds)])
    )==1)

#### todo check exist columns before copying - especially rds

## create freeze dir. Stop if exists. This is supposed to be a clean
## and controlled set of operations.
if(dir.exists(dir.freeze)) stop("dir.freeze already exists. This is not allowed. Choose another name or delete dir completely.")

## add the new path exactly as the string wanted in the new .mod file
dt.models[,string.new:=basename(string)]

dir.create(dir.freeze)

### copy data - csv and rds
path <- unique(dt.models[,path])
file.copy(path,to=dir.freeze)
path.rds <- unique(dt.models[,path.rds])
file.copy(path.rds,to=dir.freeze)

## dt.models[,model.new:=filePathSimple(dir.freeze,basename(model))]

## copy .mod files
dt.models[,file.copy(model,dir.freeze),by=.(model)]
dt.models[,model.freeze:=filePathSimple(dir.freeze,basename(model)),by=.(ROW,model)]

## modify .mod
dt.models[,{lines <- NMreadSection(model.freeze,section="DATA")
    lines.new <- sub(string,string.new,lines)
    lines.new <- c(";; Modified for freeze",lines.new)
    NMwriteSection(file=model.freeze,section="DATA",newlines=lines.new,backup=FALSE)
},by=.(ROW)]


#### copy all others than .mod and data
## For now, assume that all files start by the model name. We could look for tables without too much work. But if they are written to other directories, it will be more complicated.
dt.models[,{all.files <- list.files(dirname(model),pattern=paste0("^",fnExtension(basename(model),""),".+$"),full.names=TRUE)
    ## not entirely sure that this filter will always work. Could
    ## filePathSimple help with the comparisson?

    all.files <- setdiff(all.files,model)
    lapply(all.files,file.copy,to=dir.freeze)
    TRUE
},by=.(model)]
