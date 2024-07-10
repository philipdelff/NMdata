##' Read comments to parameter definitions in Nonmem control streams
##'
##' When interpreting parameter estimates, it is often needed to
##' recover information about the meaning of the different parameters
##' from control stream. `NMreadParsText` provides a flexible way to
##' organize the comments in the parameter sections into a
##' `data.frame`. This can subsequently easily be merged with parameter
##' values as obtained with `NMreadExt`.
##' 
##' @param file Path to the control stream to read.
##' @param fields Defines naming and splitting of contents of lines in
##'     parameter sections. Default is
##'     \code{"\%init;\%symbol;\%num;\%label;\%unit"}. Be careful to
##'     remember percentage symbols in front of any variable names.
##' @param fields.omega Like `fields`, applied to `$OMEGA`
##'     section. Default is to reuse `fields`.
##' @param fields.sigma Like `fields`, applied to `$SIGMA`
##'     section. Default is to reuse `fields.omega`.
##' @param use.theta.nums If the num field should be used to number
##'     thetas. I do not see where this is advantageous to do.
##' @param spaces.split Is a blank in `fields` to be treated as a
##'     field seperator? Default is not to (i.e. neglect spaces in
##'     `fields`).
##' @param modelname See ?NMscanData
##' @param col.model See ?NMscanData
##' @param as.fun See ?NMscanData
##' @details Off-diagonal omega and sigma elements will only be
##'     correctly treated if their num field specifies say 1-2 to
##'     specify it is covariance between 1 and 2.
##'
##' \code{SAME} elements in \code{$OMEGA} will be skipped altogether.
##' @export


NMreadParsText <- function(file,lines,format,format.omega=format,format.sigma=format.omega,
                           fields,fields.omega=fields,
                           fields.sigma=fields.omega,
                           use.theta.nums=FALSE,spaces.split=FALSE,
                           modelname,col.model,as.fun){

    idx <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    num <- NULL
    parameter <- NULL
    THETA <- NULL
    OMEGA <- NULL
    SIGMA <- NULL

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL
    
    if(!xor(is.null(file),is.null(lines))){
        stop("Exactly one of file and lines must be provided.")
    }
    if(!is.null(file)){
        if(length(file)>1) {
            return(rbindlist(lapply(file,NMreadParsText,
                                    fields=fields,
                                    fields.omega=fields.omega,
                                    fields.sigma=fields.sigma,
                                    use.theta.nums=use.theta.nums,
                                    spaces.split=spaces.split)))
        } else {
            lines <- readLines(file)
        }
        
    }
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)

    args <- getArgs()

    ## deprecated since 2024-07-09 - v0.1.7
    ## format <- deprecatedArg(newarg="format","fields",args=args)
    ## format.omega <- deprecatedArg(newarg="format.omega","fields.omega",args=args)
    ## format.sigma <- deprecatedArg(newarg="format.sigma","fields.sigma",args=args)
    if(!missing(fields)) {
        .Deprecated(new="format",old="fields")
    }
    if(!missing(fields.omega)) {
        .Deprecated(new="format.omega",old="fields.omega")
    }
    if(!missing(fields.sigma)) {
        .Deprecated(new="format.sigma",old="fields.sigma")
    }

    
    if(missing(format)){
        format <- NULL
    }
    if(is.null(format)){
        format <- "%init;%symbol;%num;%label;%unit"
    }
    if(is.function(format)) format <- format(lines)
    if(is.function(format.omega)) format.omega <- format.omega(lines)
    if(is.function(format.sigma)) format.sigma <- format.sigma(lines)

    cleanSpaces <- function(x,double=TRUE,lead=TRUE,trail=TRUE){
        if(double) x <- gsub(paste0(" +")," ",x)
        if(lead) x <- sub(paste0("^ +"),"",x)
        if(trail) x <- sub(paste0(" +$"),"",x)
        x
    }

    
    ## function to extact however many format are available in a commented
    ## parameter row
    fun.get.fields <- function(x,format){
        
        ## pat <- gsub("%[a-zA-Z0-9]*","(.*)",format)

        splitters <- format$splitters
        ## splitters.un <- unique(splitters)
        
        nfields.x <- 0
        xleft <- x

        if(F){
            for(spl in splitters){
                
                ## if(!spaces.split) spl <- paste(paste0(strsplit(spl, "")[[1]]," *"),collapse="")
                ## spl <- escape.charclass(spl)
                found <- grepl(spl,xleft)
                ## if(!found) break
                nfields.x <- nfields.x+as.numeric(found)
### not working
                ##xleft <- sub(paste0("^[^",spl,"]*",spl),"",xleft,fixed=F)
                xleft <- sub(sprintf(".*%s(.*)",spl),"\\1",xleft)
            }
            nfields.x <- nfields.x+1
        }
        ## nsplitters.x <- length(fields$splitters)
        nsplitters.x <- length(format$splitters)
        
        ## need a regex that matches the number of items found
        xleft <- x
        items <- c()
        
        for(I in 1:nsplitters.x){
            spl <- format$splitters[I]
            spl.raw <- format$splitters.raw[I]
            ## this.item.all <- sub(sprintf("([^%s]*)%s.*",
            ##                              splitters[I],
            ##                              splitters[I]),
            ##                      "\\1",xleft)

            this.item.all <- sub(sprintf("%s.*",splitters[I]), "", xleft)
            
            ## this.item <- sub("([^ ]*) $","\\1",this.item.all)
            this.item <- cleanSpaces(this.item.all)
            
            
            if(I<nsplitters.x){
                
                spl.raw.next <- format$splitters.raw[I]
                delim.partial <- paste(paste(strsplit(gsub("\\*","",spl.raw.next),"")[[1]],"*",sep=""),collapse="")
                delim.partial <- escape.charclass(delim.partial)
                this.item <- sub(sprintf("%s$",delim.partial),"",this.item)
            }
            items <- c(items,this.item)

            if(nchar(this.item.all)>0){
                xleft <- sub(paste0(this.item.all),"",xleft,fixed=TRUE)
            }
            xleft <- sub(paste0("^",splitters[I]),"",xleft)
            ##if(!grepl("[[:alnum:]]",xleft)) xleft <- ""
            if(xleft=="")  break
        }
        do.call(data.table,as.list(items))
    }
    
    



    get.theta.comments <- function(lines,section,format,use.theta.nums=FALSE){

        res.fields.theta <- splitFields(format,spaces.split=spaces.split)
        get.comments(lines,section,res.fields=res.fields.theta,use.theta.nums=FALSE)
    }

### applies processed fields on control stream sections. It does so by calling fun.get.fields
    get.comments <- function(lines,section,res.fields,use.theta.nums=FALSE){
        
        ## get theta comments
### due to a bug in NMreadSection in NMdata 0.1.3 we need to run this in two steps with keep.comments=TRUE and then remove comments lines
        lines.thetas <- NMreadSection(lines=lines,section=section,keep.name=FALSE,keep.empty=FALSE,keep.comments=TRUE)
        if(length(lines.thetas)==0) return(NULL)
        ## this should be the same as switching keep.comments to FALSE in NMreadSection()
        lines.thetas <- sub(pattern="^ *;.*$",replacement="",x=lines.thetas)
        ## these will confuse in omega/sigma sections with the current method. For those, numbering has to be done if off-diag elements are defined.
        lines.thetas <- gsub("BLOCK(.+)","",lines.thetas)
        lines.thetas <- lines.thetas[!grepl("^ *$",lines.thetas)]

        thetas.list <- lapply(lines.thetas,fun.get.fields,res.fields)
        thetas <- rbindlist(thetas.list,fill=TRUE)
        colnames(thetas) <- res.fields$fields[1:ncol(thetas)]

        thetas[,par.type:=toupper(section)]
        if(use.theta.nums){
            thetas[,i:=num]
        } else {
            thetas[,i:=.I]
        }
    }

    get.omega.comments <- function(lines,section,format){
        
        res.fields <- splitFields(format,spaces.split=spaces.split)
        omegas <- get.comments(lines=lines,section=section,res.fields=res.fields)
        if(is.null(omegas)) return(NULL)
        if(!"num"%in%colnames(omegas)) omegas[,num:=.I]
        omegas[,i:=NA_integer_]
        omegas[,j:=NA_integer_]
        
        omegas[grepl("^ *[0-9]+ *$",num),i:=as.integer(num)]
        omegas[grepl("^ *[0-9]+ *$",num),j:=as.integer(num)]
        omegas[grepl("-",num),i:=as.integer(sub(" *([0-9])+ *-.*","\\1",num))]
        omegas[grepl("-",num),j:=as.integer(sub(".*- *([0-9])","\\1",num))]
        omegas[,par.type:=toupper(section)]
        omegas
    }

    
    thetas <- get.theta.comments(lines=lines,section="THETA",format=format,
                                 use.theta.nums=use.theta.nums)
    omegas <- get.omega.comments(lines=lines,section="omega",format=format.omega)
    sigmas <- get.omega.comments(lines=lines,section="sigma",format=format.sigma)

    ## collect thetas and omegas
    pt1 <- rbind(thetas,omegas,sigmas,fill=T)
    ## we are aligning with Nonmem's behavior in ext liness. The
    ## off-diags are lower triangle. In other words, OMEGA and SIGMA
    ## are specified by column, not by row.
    pt1[par.type%in%cc(OMEGA,SIGMA),`:=`(i=j,j=i)]

### it is inconsistent, but this is how it is reported in NMreadExt
    pt1[par.type%in%cc(THETA),parameter:=sprintf("THETA%d",i)]
    pt1[par.type%in%cc(OMEGA,SIGMA),parameter:=sprintf("%s(%d,%d)",par.type,i,j)]
    
    
    if(!is.null(file)){
        this.model <- modelname(file)
        pt1[,(col.model):=this.model]
    }
    
    as.fun(pt1)
}


##' @keywords internal
escape.charclass <- function(x) {
    ##gsub("([][\\\\^-])", "\\\\\\1", x)
    gsub("([][\\\\^(\\\\^-])", "\\\\\\1", x)
}


##' splitFields splits the fields format string into the splitters
##' and the variable names. NB, this is interpreting the user-provided
##' fields, it is not looking at control stream text,
##' @keywords internal
splitFields <- function(format,spaces.split=FALSE){


    
    ## number of fields defined in format
    
    ## paste0(".*",paste(chars.to.extract,collapse=").*("),".*")
    ## regex <- paste0(".* [^", escape.charclass(chars.to.extract), "]")
    ## nfields.string <- nchars( gsub(regex, "", strings.to.search, perl=TRUE))
    
    nfields.string <- nchar(gsub("[^%]","",format))
    ## number of fields provided in string
    if(!spaces.split){
        format <- gsub(" +","",format)
    }
    
    ## a string where variables are not %var but just % so we can find splitters.
    ## splitters <- gsub("%[[:alnum:]]+","",fields)
    string.splitters <- gsub("%[[:alnum:]]+","%",format)
    splitters <- strsplit(string.splitters,"%")[[1]][-1]
    
    if(!spaces.split) splitters <- unlist(lapply(strsplit(splitters, ""),function(x)paste(paste0(x," *"),collapse="")))
    ## adding an extra splitter for the last regex to find match
    splitters <- c(splitters,";")
    splitters.raw <- splitters
    splitters <- escape.charclass(splitters)

    
    string.vars <- gsub("[^[:alnum:]]+",";",format)
    vars <- strsplit(string.vars,";")[[1]][-1]

    ## string.left <- fields
    ## fields.res <- c()
    ## ## adding an extra splitter for the last regex to find match
    ##
    ## splitters.tmp <- escape.charclass(splitters.tmp)
    

    ## for(I in 1:nfields.string){
    ##     ## cat(I,"\n")
    ##     fields.res <- c(fields.res,
    ##                     sub(
    ##                         paste0(" *%*([^",splitters.tmp[I],"]*)",splitters.tmp[I],"*.*")
    ##                        ,"\\1",string.left)
    ##                     )
    ##     string.left <- sub(sprintf("[^%s]*%s",splitters.tmp[I],splitters.tmp[I]),"",string.left)
    ## }
    
    list(fields=vars,splitters=splitters,splitters.raw=splitters.raw)
}
