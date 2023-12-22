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
##'     \code{"\%init;\%symbol;\%num;\%label;\%unit"}.
##' @param fields.omega Like `fields`, applied to `$OMEGA`
##'     section. Default is to reuse `fields`.
##' @param fields.sigma Like `fields`, applied to `$SIGMA`
##'     section. Default is to reuse `fields.omega`.
##' @param use.theta.nums If the num field should be used to number
##'     thetas. I do not see where this is advantageous to do.
##' @param spaces.split Is a blank in `fields` to be treated as a
##'     field seperator? Default is not to (i.e. neglect spaces in
##'     `fields`).
##' @details Off-diagonal omega and sigma elements will only be
##'     correctly treated if their num field specifies say 1-2 to
##'     specify it is covariance between 1 and 2.
##' @export


NMreadParsText <- function(file,fields,fields.omega=fields,
                           fields.sigma=fields.omega,
                           use.theta.nums=FALSE,spaces.split=FALSE){

    idx <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    num <- NULL
    OMEGA <- NULL
    SIGMA <- NULL
    
    if(missing(fields)){
        fields <- "%init;%symbol;%num;%label;%unit"
    }

    cleanSpaces <- function(x,double=TRUE,lead=TRUE,trail=TRUE){
        if(double) x <- gsub(paste0(" +")," ",x)
        if(lead) x <- sub(paste0("^ +"),"",x)
        if(trail) x <- sub(paste0(" +$"),"",x)
        x
    }
    escape.charclass <- function(x) {
        ##gsub("([][\\\\^-])", "\\\\\\1", x)
        gsub("([][\\\\^(\\\\^-])", "\\\\\\1", x)
    }

    
    ## function to extact however many fields are available in a commented
    ## parameter row
    fun.get.fields <- function(x,fields){
        
        ## pat <- gsub("%[a-zA-Z0-9]*","(.*)",fields)

        splitters <- fields$splitters
        splitters.un <- unique(splitters)
        


        
        nfields.x <- 0
        xleft <- x
        for(spl in fields$splitters){
            spl <- escape.charclass(spl)
            nfields.x <- nfields.x+as.numeric(grepl(spl,x))
            xleft <- sub(paste0("^[^",spl,"]*",spl),"",xleft,fixed=F)
        }
        nfields.x <- nfields.x+1

        

        if(F){
            ## regex <- paste0("[^", paste(escape.charclass(splitters.un),collapse=""), "]")
            ## gsub(regex, "", x, perl=TRUE)
            ## gsub("(?m)(^[^(\n]+)[)]|-", "\\1;", x,perl = TRUE)
            ## gsub("(?m)(^[^(\n]+)[;]|-", "\\1;", x,perl = TRUE)
### this doesn't work for characters that need escaping like -
            nfields.x <- nchar(
                ## splitters.un2 <- gsub("\\-","\\\\\\\\-",splitters.un)
                ## splitters.un2 <- gsub("\\)","\\\\\\\\)",splitters.un2)
                gsub(paste0('[^',paste(splitters.un,collapse=""),"]"),"",x)
                ## gsub(paste0('[^', splitters.un,"]",collapse=""), "", x)

            )+1
        }
        
        ## need a regex that matches the number of items found
        x.left <- x
        items <- c()
        for(I in 1:nfields.x){
            
            this.item.all <- sub(sprintf("([^%s]*)%s.*",escape.charclass(splitters[I]),
                                         escape.charclass(splitters[I])),"\\1",x.left)
            ## this.item <- sub("([^ ]*) $","\\1",this.item.all)
            this.item <- cleanSpaces(this.item.all)
            items <- c(items,this.item)
            x.left <- sub(paste0(this.item.all),"",x.left,fixed=TRUE)
            x.left <- sub(paste0("^",escape.charclass(splitters[I])),"",x.left)
            if(x.left=="") break
        }
        do.call(data.table,as.list(items))
    }
    
    
    split.fields <- function(fields){
        ## number of fields defined in fields
        
        ## paste0(".*",paste(chars.to.extract,collapse=").*("),".*")
        ## regex <- paste0(".* [^", escape.charclass(chars.to.extract), "]")
        ## nfields.string <- nchars( gsub(regex, "", strings.to.search, perl=TRUE))
        
        nfields.string <- nchar(gsub("[^%]","",fields))
        ## number of fields provided in string
        if(!spaces.split){
            fields <- gsub(" +","",fields)
        }
        splitters <- gsub("%[[:alnum:]]+","",fields)
        splitters <- strsplit(splitters, "")[[1]]
        ## splitters.un <- unique(splitters)
        
        string.left <- fields
        fields.res <- c()
        ## adding an extra splitter for the last regex to find match
        splitters.tmp <- c(splitters,";")
        splitters.tmp <- escape.charclass(splitters.tmp)
        

        for(I in 1:nfields.string){
            ## cat(I,"\n")
            fields.res <- c(fields.res,
                            sub(
                                paste0(" *%*([^",splitters.tmp[I],"]*)",splitters.tmp[I],"*.*")
                               ,"\\1",string.left)
                            )
            string.left <- sub(sprintf("[^%s]*%s",splitters.tmp[I],splitters.tmp[I]),"",string.left)
        }
        
        list(fields=fields.res,splitters=splitters)
    }


    get.theta.comments <- function(file,section,fields,use.theta.nums=FALSE){

        res.fields.theta <- split.fields(fields)        
        get.comments(file,section,res.fields=res.fields.theta,use.theta.nums=FALSE)
    }
    
    get.comments <- function(file,section,res.fields,use.theta.nums=FALSE){
        
        ## get theta comments
### due to a bug in NMreadSection in NMdata 0.1.3 we need to run this in two steps with keep.comments=TRUE and then remove comments lines
        lines.thetas <- NMreadSection(file=file,section=section,keep.name=FALSE,keep.empty=FALSE,keep.comments=TRUE)
        
        lines.thetas <- sub(pattern="^ *;.*$",replacement="",x=lines.thetas)
        ## these will confuse in omega/sigma sections with the current method. For those, numbering has to be done if off-diag elements are defined.
        lines.thetas <- gsub("BLOCK(.+)","",lines.thetas)
        lines.thetas <- lines.thetas[!grepl("^ *$",lines.thetas)]

        thetas <- lapply(lines.thetas,fun.get.fields,res.fields) |> rbindlist(fill=TRUE)
        colnames(thetas) <- res.fields$fields[1:ncol(thetas)]

        thetas[,par.type:=toupper(section)]
        if(use.theta.nums){
            thetas[,i:=num]
        } else {
            thetas[,i:=.I]
        }
    }

    get.omega.comments <- function(file,section,fields){
        
        res.fields <- split.fields(fields)
        omegas <- get.comments(file=file,section=section,res.fields=res.fields)

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
    
    
    thetas <- get.theta.comments(file=file,section="THETA",fields=fields,use.theta.nums=use.theta.nums)
    omegas <- get.omega.comments(file=file,section="omega",fields=fields.omega)
    sigmas <- get.omega.comments(file=file,section="sigma",fields=fields.sigma)

    ## collect thetas and omegas
    pt1 <- rbind(thetas,omegas,sigmas,fill=T)
    ## we are aligning with Nonmem's behavior in ext files. The
    ## off-diags are lower triangle. In other words, OMEGA and SIGMA
    ## are specified by column, not by row.
    pt1[par.type%in%cc(OMEGA,SIGMA),`:=`(i=j,j=i)]

    pt1[]
}

