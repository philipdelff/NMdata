##' @keywords internal

NMwriteSectionOne <- function(file0,lines,section,location="replace",
                              newlines,list.sections,newfile,
                              backup=TRUE,blank.append=TRUE,write,
                              quiet=FALSE){

    after <- NULL 
    before <- NULL
    mad.dl <- NULL
    
    if(!missing(file0)){
        file0 <- filePathSimple(file0)
        stopifnot(file.exists(file0))
        ## see below why we need to read the lines for now
        lines <- readLines(file0,warn=FALSE)
        if(missing(newfile)) newfile <- file0
    }
    if(missing(write)) write <- !missing(file0)

    if(missing(newfile)) newfile <- NULL
    if(!is.null(newfile)){
        newfile <- filePathSimple(newfile)
    }


    if(missing(list.sections)||is.null(list.sections)){
        ## this must be list, not as.list. as.list would translate multiple lines into multiple sections.
        list.sections=list(newlines)
        names(list.sections) <- section
    } else {
        
        if(length(list.sections)>1 && location!="replace"){
            messageWrap("Only location=replace is supported in combination with list.sections.",fun.msg=stop)
        }
    }
    
    ## put this part in a function to be sequentially applied for all elements in list.
    replaceOnePart <- function(lines,section,newlines,quiet=FALSE){
        if(!quiet) message(paste("Writing",newfile))
        
        ## make sure section is capital and does not start with $.
        section <- gsub(" ","",section)
        section <- sub("^\\$","",section)
        section <- toupper(section)
        
        if(is.function(newlines)){
            ## this check should be outside replaceOnePart
            if(location!="replace") stop("When newlines is a function, location must be replace.")
            newlines.fun <- newlines
            
            newlines <- NMreadSection(lines=lines,section=section,return="text",keep.empty=TRUE,
                                      keep.name=TRUE,keep.comments=TRUE,as.one=TRUE,
                                      clean.spaces=FALSE)
            newlines <- newlines.fun(newlines)
        }
        
        ## make sure newlines start with $SECTION
        newlines <- sub("^ +","",newlines)            
        if(blank.append) newlines <- c(newlines,"")
        
        idx.dlines <- NMreadSection(lines=lines,section=section,return="idx",keep.empty=TRUE,
                                    keep.name=TRUE,keep.comments=TRUE,as.one=TRUE,
                                    clean.spaces=FALSE)

        if(length(idx.dlines)==0&location%in%cc(replace,before,after)) {
            if(!quiet) message("Section not found. Nothing to be done.")
            return(lines)
        }
        
        if(length(idx.dlines)>1) {
            ## if th
            stopifnot(max(diff(idx.dlines))==1)
        }
        
        if(location%in%cc(replace,before,after)){
            min.dl <- min(idx.dlines)
            max.dl <- max(idx.dlines)

### these two cases need to be handled slightly differently so not supported for now
            
            stopifnot(min.dl>1)
        }
        nlines <- length(lines)
        
        if(location=="replace"){
            
            if(min.dl==1&&max.dl==nlines){
                all.lines <- newlines
            } else if(min.dl==1){
                all.lines <- c(newlines,lines[(mad.dl+1),nlines])
            } else if(max.dl==nlines){
                all.lines <- c(lines[1:(min.dl-1)],newlines)
            } else {
                all.lines <- c(lines[1:(min.dl-1)],
                               newlines,
                               lines[(max.dl+1):nlines])
            }
        }
        if(location=="before"){
            if(min.dl==1){
                all.lines <- c(newlines,lines)
            } else {
                all.lines <- c(lines[1:(min.dl-1)],
                               newlines,
                               lines[(min.dl:length(lines))]
                               )
            } 
        }
        if(location=="after"){
            all.lines <- c(lines,newlines)
            if(min.dl>1){
                all.lines <- c(lines[1:(max.dl)],
                               newlines,
                               lines[(max.dl+1):length(lines)]
                               )
            } else {
                all.lines <- lines
            }
        }
        if(location=="last"){
            all.lines <- c(lines,newlines)
        }
        all.lines
    }
    
    newlines <- lines
    for (I in 1:length(list.sections)) {
        newlines <- replaceOnePart(lines=newlines,section=names(list.sections)[I],
                                   newlines=list.sections[[I]]
                                   ,quiet=quiet)
    }
    
    if(is.null(newfile)) return(newlines)
    
    if(file0==newfile && backup ) {
        dir.backup <- file.path(dirname(file0),"NMdata_backup")
        ## make sure backup dir exists
        if(file.exists(dir.backup)&&!dir.exists(dir.backup)) messageWrap("Something called NMdata_backup is found and it is not a directory. Please remove or use backup=FALSE.",fun.msg=stop)
        if(!dir.exists(dir.backup)) dir.create(dir.backup)
        ## file.copy (file0,
        ##            sub("(.+/)([^/].+$)","\\1backup_\\2",x=file0)
        ##            )
        file.copy(file0,dir.backup,overwrite=T)
    }

    if(!write||is.null(newfile)){
        return(newlines)
    }
    
    con.newfile <- file(newfile,"wb")
    writeLines(newlines,con=con.newfile)
    close(con.newfile)
    return(invisible(newlines))
}
