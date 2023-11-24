##' Check data for Nonmem compatibility or check control stream for
##' data compatibility
##'
##' Check data in various ways for compatibility with Nonmem. Some
##' findings will be reported even if they will not make Nonmem fail
##' but because they are typical dataset issues.
##' 
##' @param data The data to check. data.frame, data.table, tibble,
##'     anything that can be converted to data.table.
##' @param file Alternatively to checking a data object, you can use
##'     file to specify a control stream to check. This can either be
##'     a (working or non-working) input control stream or an output
##'     control stream. In this case, \code{NMdataCheck} checks column
##'     names in data against control stream (see
##'     \code{NMcheckColnames}), reads the data as Nonmem would do,
##'     and do the same checks on the data as NMdataCheck would do
##'     using the data argument. \code{col.flagn} is ignored in this
##'     case - instead, ACCEPT/IGNORE statements in control stream are
##'     applied. The file argument is useful for debugging a Nonmem
##'     model.
##' @param covs columns that contain subject-level covariates. They
##'     are expected to be non-missing, numeric and not varying within
##'     subjects.
##' @param covs.occ A list specifying columns that contain
##'     subject:occasion-level covariates. They are expected to be
##'     non-missing, numeric and not varying within combinations of
##'     subject and occasion. \code{covs.occ=list(PERIOD=c("FED"))}
##'     means that \code{FED} is the covariate, while \code{PERIOD}
##'     indicates the occasion.
##' @param cols.num Columns that are expected to be present, numeric
##'     and non-NA. If a character vector is given, the columns are
##'     expected to be used in all rows. If a column is only used for
##'     a subset of rows, use a list and name the elements by
##'     subsetting strings. See examples.
##' @param col.cmt The name(s) of the compartment column(s). These
##'     will be checked to be positive integers for all rows. They are
##'     also used in checks for row duplicates.
##' @param col.amt The name of the dose amount column.
##' @param col.id The name of the column that holds the subject
##'     identifier. Default is "ID".
##' @param col.time The name of the column holding actual time.
##' @param col.dv The name of the column holding the dependent
##'     variable. For now, only one column can be specified, and
##'     \code{MDV} is assumed to match this column. Default is
##'     \code{DV}.
##' @param col.mdv The name of the column holding the binary indicator
##'     of the dependent variable missing. Default is \code{MDV}.
##' @param col.flagn Optionally, the name of the column holding
##'     numeric exclusion flags. Default value is \code{FLAG} and can
##'     be configured using \code{NMdataConf}. Even though \code{FLAG}
##'     is the default value, no finding will be returned if the
##'     column is missing unless explicitly defined as
##'     \code{col.flagn="FLAG"}. This is because this way of using
##'     exclusion flags is only one of many ways you could choose to
##'     handle exclusions. Disable completely by using
##'     \code{col.flagn=FALSE}.
##' @param col.row A column with a unique value for each row. Such a
##'     column is recommended to use if possible. Default
##'     (\code{"ROW"}) can be modified using \code{NMdataConf}.
##' @param col.usubjid Optional unique subject identifier. It is
##'     recommended to keep a unique subject identifier (typically a
##'     character string including an abbreviated study name and the
##'     subject id) from the clinical datasets in the analysis set. If
##'     you supply the name of the column holding this identifier,
##'     NMcheckData will check that it is non-missing, that it is
##'     unique within values of col.id (i.e. that the analysis subject
##'     ID's are unique across actual subjects), and that col.id is
##'     unique within the unique subject ID (a violation of the latter
##'     is less likely).
##' @param cols.dup Additional column names to consider in search of
##'     duplicate events. col.id, col.cmt, col.evid, and col.time are
##'     always considered if found in data, and cols.dup is added to
##'     this list if provided.
##' @param type.data "est" for estimation data (default), and "sim"
##'     for simulation data. Differences are that \code{col.row} is
##'     not expected for simulation data, and subjects will be checked
##'     to have \code{EVID==0} rows for estimation data and
##'     \code{EVID==2} rows for simulation data.
##' @param na.strings Strings to be accepted when trying to convert
##'     characters to numerics. This will typically be a string that
##'     represents missing values. Default is ".". Notice, actual NA,
##'     i.e. not a string, is allowed independently of na.strings. See
##'     \code{?NMisNumeric}.
##' @param return.summary If TRUE (not default), the table summary
##'     that is printed if \code{quiet=FALSE} is returned as well. In
##'     that case, a list is returned, and the findings are in an
##'     element called findings.
##' @param quiet Keep quiet? Default is not to.
##' @param as.fun The default is to return data as a
##'     \code{data.frame}. Pass a function (say
##'     \code{tibble::as_tibble}) in as.fun to convert to something
##'     else. If \code{data.tables} are wanted, use
##'     \code{as.fun="data.table"}. The default can be configured
##'     using \code{NMdataConf}.
##' @details The following checks are performed. The term "numeric"
##'     does not refer to a numeric representation in R, but
##'     compatibility with Nonmem. The character string "2" is in this
##'     sense a valid numeric, "id2" is not.  \itemize{
##'
##' \item Column
##'     names must be unique and not contain special characters
##' 
##' \item If an exclusion flag is used (for ACCEPT/IGNORE in Nonmem),
##'     elements must be non-missing and integers. Notice, if an exclusion
##'     flag is found, the rest of the checks are performed on rows
##'     where that flag equals 0 (zero) only.
##'
##' \item If a unique row identifier is found, it has to be
##' non-missing, increasing integers. 
##'
##' \item col.time (TIME),
##'     EVID, col.id (ID), col.cmt (CMT), and col.mdv (MDV): If present, elements must be non-missing
##'     and numeric.
##'
##' \item col.time (TIME) must be non-negative
##'
##' \item EVID must be in \{0,1,2,3,4\}.
##'
##' \item CMT must be positive integers. However, can be missing or zero for EVID==3.
##'
##' \item MDV must be the binary (1/0) representation of is.na(DV) for
##' dosing records (EVID==0).
##'
##' \item AMT must be 0 or NA for EVID 0 and 2
##'
##' \item AMT must be positive for EVID 1 and 4
##'
##' \item DV must be numeric
##'
##' \item DV must be missing for EVID in \{1,4\}.
##'
##' \item If found, RATE must be a numeric, equaling -2 or non-negative for dosing events.
##'
##' \item If found, SS must be a numeric, equaling 0 or 1 for dosing records.
##'
##' \item If found, \code{ADDL} must be a non-negative integer for dosing
##' records. II must be present.
##'
##' \item If found, II must be a non-negative integer for dosing
##' records. \code{ADDL} must be present.
##'
##' \item ID must be positive and values cannot be disjoint (all
##'     records for each ID must be following each other. This is
##'     technically not a requirement in Nonmem but most often an
##'     error. Use a second ID column if you deliberately want to
##'     soften this check)
##'
##' \item TIME cannot be decreasing within ID, unless EVID in \{3,4\}.
##'
##' \item all ID's must have doses (EVID in \{1,4\})
##'
##' \item all ID's must have observations (EVID==0)
##'
##' \item ID's should not have leading zeros since these will be lost
##' when Nonmem read, then write the data.
##'
##' \item If a unique row identifier is used, this must be
##'     non-missing, increasing, integer
##'
##' \item Character values must not contain commas (they will mess up
##'     writing/reading csv)
##'
##' \item Columns specified in covs argument must be non-missing,
##'     numeric and not varying within subjects.
##' 
##' \item Columns specified in covs.occ must be
##'     non-missing, numeric and not varying within combinations of
##'     subject and occasion.
##'
##' \item Columns specified in cols.num must be present, numeric
##'     and non-NA.
##'
##' \item If a unique subject identifier column (col.usubjid) is
##' provided, col.id must be unique within values of col.usubjid and
##' vice versa.
##'
##' \item Events should not be duplicated. For all rows, the
##' combination of col.id, col.cmt ,col.evid, col.time plus the
##' optional columns specified in cols.dup must be unique. In other
##' words, if a subject (col.id) that has say observations (col.evid)
##' at the same time (col.time), this is considered a duplicate. The
##' exception is if there is a reset event (col.evid is 3 or 4) in
##' between the two rows. cols.dup can be used to add columns to this
##' analysis. This is useful for different assays run on the same
##' compartment (say a DVID column) or maybe stacked datasets. If
##' col.cmt is of length>1, this search is repeated for each cmt
##' column.
##'
##' }
##' @return A table with findings
##' @examples
##' \dontrun{
##' dat <- readRDS(system.file("examples/data/xgxr2.rds", package="NMdata"))
##' NMcheckData(dat)
##' dat[EVID==0,LLOQ:=3.5]
##' ## expecting LLOQ only for samples
##' NMcheckData(dat,cols.num=list(c("STUDY"),"EVID==0"=c("LLOQ")))
##' }
##' @import data.table
##' @export


NMcheckData <- function(data,file,covs,covs.occ,cols.num,col.id="ID",
                        col.time="TIME",col.dv="DV",col.mdv="MDV",
                        col.cmt="CMT",col.amt="AMT",col.flagn,col.row,
                        col.usubjid,cols.dup,type.data="est",
                        na.strings,return.summary=FALSE, quiet=FALSE,
                        as.fun){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    ADDL <- NULL
    AMT <- NULL
    CMT <- NULL
    DV <- NULL
    EVID <- NULL
    ID.jump <- NULL
    ID <- NULL
    II <- NULL
    MDVDV <- NULL
    MDV <- NULL
    N <- NULL
    Nrep <- NULL
    RATE <- NULL
    ROW <- NULL
    SS <- NULL
    column <- NULL
    check <- NULL
    checkTimeInc <- NULL
    cov <- NULL
    newID <- NULL
    isnewID <- NULL
    level <- NULL
    occ <- NULL
    reset <- NULL
    variable <- NULL
    NUID <- NULL
    NidByUID <- NULL
    usubjNotUnique <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
#### Section start: Checks of arguments ####
    
    col.evid <- "EVID"
    if(missing(covs)) covs <- NULL
    if(missing(covs.occ)) covs.occ <- NULL
    if(missing(cols.num)) cols.num <- NULL
    if(missing(na.strings)) na.strings <- "."

    if(!is.character(type.data)||!type.data%in% c("est","sim")){
        stop("type.data myst be either \"est\" or \"sim\".")
    }

    col.row.was.mising <- FALSE
    if(missing(col.row)) {
        col.row <- NULL
        col.row.was.mising <- TRUE
    }

    if(! (type.data=="sim" && is.null(col.row))){
        col.row <- NMdataDecideOption("col.row",col.row)
    }
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(col.flagn)) col.flagn <- NULL
    col.flagn.orig <- col.flagn
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    if(missing(file)) file <- NULL

    if(!is.null(covs) && !is.character(covs)) {
        messageWrap("covs must be a character vector or a string.",fun.msg=stop)
    }
    if(!is.null(cols.num) && !is.list(cols.num) ){
        cols.num <- list("TRUE"=cols.num)
    }
    if(missing(cols.dup)) cols.dup <- NULL

    names.cols.num <- names(cols.num)
    
    if(length(cols.num)>0){
        if(length(names.cols.num)==0) names.cols.num <- rep("",length(cols.num))
        names.cols.num[gsub(" +","",names.cols.num)==""] <- "TRUE"
        names(cols.num) <- names.cols.num
    }
    if(!is.null(cols.num)) {
        lapply(cols.num, function(x){if(!is.character(x)) {
                                         messageWrap("cols.num must be a character vector or a string.",fun.msg=stop)
                                     }})}
    if(!is.null(covs.occ)){
        if(!is.null(covs.occ)) {
            if( !is.list(covs.occ) || is.data.frame(covs.occ)) {
                messageWrap("covs.occ must be a list and not a data.frame.",fun.msg=stop)
            }
            if(is.null(names(covs.occ)) || ""%in%gsub(" ","",names(covs.occ))){
                messageWrap("covs.occ must be a named list.",fun.msg=stop)
            }
            if(!(all(sapply(covs.occ,is.character)))){
                messageWrap("covs.occ must be a named list of character vectors or strings.",fun.msg=stop)
            }
        }
    }
    
    
    if(!(is.character(col.dv)&&length(col.dv)==1)){
        stop("col.dv must be a character and vector of length 1.")
    }
    if(!(is.character(col.mdv)&&length(col.mdv)==1)){
        stop("col.mdv must be a character and vector of length 1.")
    }
    
    if(is.null(file)){
        if(missing(data)||is.null(data)) {
            message("No data supplied")
            return(invisible(NULL))
        }
        if(!is.data.frame(data)) stop("Data must be inheriting from data.frame (meaning that more advanced classes like data.table and tibble are OK.")
        data <- copy(as.data.table(data))
        ## for reference as data is edited
        cnames.data.0 <- colnames(data)
        
        if(!col.id %in%cnames.data.0) {stop("col.id must point to an existing column in data. If you don't have one, you can create a dummy column with a constant value.")}

    }

    
### file mode
    if(!is.null(file)){
        if(!is.null(col.flagn.orig)){warning("col.flagn is not used when file is specified.")}
        col.id <- "ID"
        ## use.rds <- FALSE
        formats.read="csv"
        file.mod <- NULL
        res <- NMcheckDataFile(file=file,col.time=col.time,
                               col.row=col.row,col.id=col.id,
                               na.strings=na.strings
                              ,formats.read=formats.read
                              ,quiet=quiet,file.mod=file.mod,
                               as.fun=as.fun)
        return(invisible(res))
    }
    

### Section end: Checks of arguments


### row counter
    c.row <- tmpcol(data,base="row",prefer.plain=TRUE)
    data[,(c.row):=.I]

### save original col.id and col.row
    if(!is.null(col.row)&&col.row%in%colnames(data)){
        col.row.orig <- tmpcol(data,base="row.orig",prefer.plain=TRUE)
        data[,(col.row.orig):=get(col.row)]
    }
    if(col.id%in%colnames(data)){
        col.id.orig <- tmpcol(data,base="id.orig",prefer.plain=TRUE)
        data[,(col.id.orig):=get(col.id)]
    }

    if(any(!cols.dup%in%colnames(data))){
        warning("Not all columns specified in cols.dup found in data. Skipping those not found.")
        cols.dup <- intersect(cols.dup,colnames(data))
    }


    NMasNumeric <- function(x,warn=F) {
        if(warn){
            as.numeric(as.character(x))
        } else {
            suppressWarnings(as.numeric(as.character(x)))
        }
    }
    NMisMissing <- function(x) is.na(x) | (is.character(x) & x %in% na.strings)

    ## listEvents is for row-level findings
    ## @param col is the actual column to be used for the condition
    ## @param name The name of the check. Will end up in the check column in the resulting table.
    ## @param fun if fun does not return TRUE, we have a finding.
    ## @param colname is the column name reported to user.

    ## @param new.rows.only. For nested criteria. Say that CMT is not
    ## @param col.required Include a finding if column not found?
    ## numeric for row 10. Then don't report that it is not an integer
    ## too.
    listEvents <- function(col,name,fun,colname=col,dat=data,events=NULL,invert=FALSE,new.rows.only=T,quiet=FALSE,col.required=TRUE,debug=F,args.fun=NULL){
        if(debug) browser()

        if(!col%in%colnames(dat)){
            if(col.required){
                if(is.null(events)||events[check=="Column not found"&column==colname&level=="column",.N]==0){
                    events <- rbind(events,
                                    data.table(check="Column not found",column=colname,level="column")
                                   ,fill=TRUE) }
            }
            return(events)
        }
        
        
        if(invert){
            rows <- dat[do.call(fun,append(list(get(col)),args.fun))==TRUE,get(c.row)]
        } else {
            rows <- dat[do.call(fun,append(list(get(col)),args.fun))==FALSE,get(c.row)]
        }

        if(length(rows)==0) {
            res <- data.table(check=name,column=colname,row=NA,level="row")[0]
        } else {
            
            if(new.rows.only&&!is.null(events)){
                rows <- setdiff(rows,events[column==colname,row])
            }
            if(length(rows)>0){
                res <- data.table(check=name,column=colname,row=rows,level="row")
            } else {
                res <- NULL
            }
        }
        rbind(events,res,fill=TRUE)
    }

    reportFindings <- function(findings,data,col.id,col.row,c.row,col.row.orig,col.id.orig,quiet,as.fun,return.summary){
        
### Add ID's to row-level findings
        findings.row <- findings[level=="row"]
        if(!is.null(col.id)&&nrow(findings.row)>0){
            if(col.id%in%colnames(findings.row)) findings.row[,(col.id):=NULL]
            
            findings.row <- mergeCheck(findings.row,data[,c(c.row,col.id.orig),with=F],by.x="row",by.y=c.row,all.x=T,fun.commoncols=stop,quiet=TRUE)
            setnames(findings.row,col.id.orig,col.id)        
            findings <- rbind(findings[level!="row"],findings.row,fill=T)
            
        }

        
        if(nrow(findings)==0) {
            if(!quiet) message("No findings. Great!")
            summary.findings <- NULL
        } else {
### use the row identifier for reporting
            if(!is.null(col.row)&&col.row%in%colnames(data)){
                
                findings <- mergeCheck(findings,
                                       data[,c(c.row,col.row.orig),with=F],
                                       by.x="row",by.y=c.row,all.x=T,
                                       fun.commoncols=stop,quiet=TRUE)
                setnames(findings,col.row.orig,col.row)
            }
            if(!col.id%in%colnames(findings)) findings[,(col.id):=NA_real_]
            setcolorder(findings,c(c.row,col.id,"column","check"))
            setorderv(findings,c(c.row,"column","check"))

            
            summary.findings <- findings[,.(.N,Nid=uniqueN(get(col.id)[!is.na(get(col.id))])),by=.(column,check)]
            
            if(!quiet) print(summary.findings,row.names=FALSE)

        }
        findings <- as.fun(findings)

        res <- findings
        if(return.summary){
            res <- list(findings=findings,summary=summary.findings)
        }
        return(invisible(res))

    }


    
    findings <- data.table(check="is NA",column="TIME",row=0,level="row")[0]

####### Checking column names
    cnames <- colnames(data)
### Duplicate column names
    Ntab.cnames <- data.table(col=cnames)[,.N,by=.(col)]
    dt.dups <- Ntab.cnames[N>1]
    if(nrow(dt.dups)){
        warning("Duplicate column names found. Please fix. Other checks may be affected.")
        findings <- rbind(findings,
                          data.table(check="Non-unique column names",column=dt.dups[,col],row=NA_integer_,level="column"))
    }


### special chars in col names
    are.cols.num <- sapply(data,NMisNumeric,na.strings=na.strings)
    cnames.num <- colnames(data)[are.cols.num]
    cnames.spchars <- cnames.num[grep("[[:punct:]]",cnames.num)]
    cnames.spchars <- setdiff(cnames.spchars,c("id.orig","row.orig"))
    if(length(cnames.spchars)>0){
        findings <- rbind(findings,
                          data.table(check="Special character in column name",column=cnames.spchars,row=NA_integer_,level="column"))
    }
### check for na.strings in numeric columns encoded as character. 

####### End checking column names


    if(nrow(data)==0){
        findings <- rbind(findings,
                          data.table(check="No rows in data",level="data"),
                          fill=T)
        return(reportFindings(findings=findings,data=data,col.id=col.id,col.row=col.row,c.row=c.row,col.row.orig=col.row.orig,col.id.orig=col.id.orig,as.fun=as.fun,return.summary=return.summary,quiet=quiet))
    }

    
    
### check unique row identifier
    if(!is.null(col.row)){

        findings <- listEvents(col.row,"Missing value",
                               fun=is.na,invert=TRUE,events=findings,debug=FALSE)
        findings <- listEvents(col.row,"Not numeric",
                               fun=function(x)NMisNumeric(x,na.strings=na.strings),
                               events=findings,
                               new.rows.only=T)
        
        ## leading zeros if character?
        if(col.row%in%colnames(data)&&data[,is.character(get(col.row))]){
            findings <- listEvents(col.row,"Leading zero may corrupt merging",
                                   fun=function(x)grepl("^0.+",x),invert=TRUE,
                                   events=findings,debug=FALSE)
        }
        if(col.row%in%colnames(data)){
            data[,(col.row):=NMasNumeric(get(col.row))]
        }
        
        ## translate to numeric for rest of checks
        
        findings <- listEvents(col.row,"Row identifier decreasing",
                               fun=function(x)!is.na(c(1,diff(x))) & c(1,diff(x))>0,
                               events=findings,debug=FALSE)
        
        findings <- listEvents(col.row,"Duplicated",
                               fun=function(x)!is.na(x)&!duplicated(x),events=findings,
                               new.rows.only=T,debug=FALSE)
        findings <- listEvents(col.row,
                               "Row identifier not an integer",
                               fun=function(x)x%%1==0,events=findings,
                               new.rows.only=T,debug=F)
    }
    
### check flags for NA's before subsetting on FLAG
    if(!is.null(col.flagn)){
        
        findings <- listEvents(col.flagn,"Missing value",
                               fun=is.na,invert=TRUE,events=findings,
                               col.required=!is.null(col.flagn.orig),debug=FALSE)
        ## not sure if this should be NMisNumeric(...,each=T). I guess
        ## something is completely wrong with the column if elements
        ## are not numeric. But other columns are check with each=T.
        findings <- listEvents(col.flagn,"Not numeric",
                               fun=function(x)NMisNumeric(x,na.strings=na.strings),
                               events=findings,
                               new.rows.only=T,
                               col.required=!is.null(col.flagn.orig))
        findings <- listEvents(col.flagn,"col.flagn not an integer",
                               fun=function(x)as.numeric(x)%%1==0,events=findings,
                               new.rows.only=T,
                               col.required=!is.null(col.flagn.orig))
        if(col.flagn%in%colnames(data)){

### Done checking flagn. For rest of checks, only consider data where col.flagn==0
            data[,(col.flagn):=NMasNumeric(get(col.flagn))]
            if(is.numeric(data[,get(col.flagn)])){
                data <- data[get(col.flagn)==0]
            }
        }
    }

    if(nrow(data)==0){
        findings <- rbind(findings,
                          data.table(check="Data empty after applying exclusion flags",level="data"),
                          fill=T)
        return(reportFindings(findings=findings,data=data,col.id=col.id,col.row=col.row,c.row=c.row,col.row.orig=col.row.orig,col.id.orig=col.id.orig,as.fun=as.fun,return.summary=return.summary,quiet=quiet))
    }

    
#### Section start: commas in character columns ####
    
    cols.char <- colnames(data)[!are.cols.num]
    newfinds <- rbindlist(
        lapply(cols.char,listEvents,name="Comma in character string",fun=function(x)grepl(",",x),invert=TRUE,debug=FALSE)
    )
    findings <- rbind(findings,
                      newfinds
                     ,fill=TRUE)

### Section end: commas in character columns
    

    ## a new row counter for internal use only. It matches the data we
    ## are checking but not the data supplied by user.
    rowint <- tmpcol(data,base="ROWINT",prefer.plain=TRUE)
    data[,(rowint):=.I]


    
######## Default numeric columns. Will be checked for presence, NA, non-numeric (col-level)
### Others that: If column present, must be numeric, and values must be non-NA. Remember eg DV, CMT and AMT can be NA.
    
    cols.num.all <- c( col.time,"EVID",col.id,col.mdv,
                      covs,names(covs.occ),as.character(unlist(covs.occ))
                      )
    if(!is.null(col.flagn.orig)) cols.num.all <- c(cols.num.all,col.flagn)
    

    cols.num.all <- c(list("TRUE"=cols.num.all),
                      cols.num)

    

##### I believe this is covered altogether as part of cols.num.all.
    ##     ## cols.num is a named list. Names are subsets.
    if(!is.null(cols.num.all
                )){
        
        subsets.cols.num <- names(cols.num.all)
        
        for(n in 1:length(cols.num.all)){
            ##             
            expr.sub <- subsets.cols.num[n]
            rows.sub <- data[eval(parse(text=expr.sub)),get(rowint)]
            for(col in cols.num.all[[n]]){
                ## will this ever be needed? cols.num is only what is
                ## supplied as arg, and those are already checked as
                ## part of cols.num.all.
                findings <- listEvents(col,name="Not numeric",
                                       fun=function(x)NMisNumeric(x,na.strings=na.strings,each=TRUE),
                                       new.rows.only=TRUE,events=findings,dat=data[eval(parse(text=expr.sub))])
                findings <- listEvents(col,name="is NA",
                                       fun=is.na,invert=TRUE,new.rows.only=T,debug=FALSE,events=findings,dat=data[get(rowint)%in%rows.sub])
                ## not expecting values if outside subset
### can a col.num ever be outside subset?
                findings <- listEvents(col,name="NA expected",
                                       fun=NMisMissing,
                                       new.rows.only=FALSE,events=findings,dat=data[!get(rowint)%in%rows.sub])
            }
        }
    }

    
###### checks on cols.num.all before converting to numeric
### if col.row or ID are characters, they must not have leading zeros.
    ## Check ID for leading zeros before converting to numeric
    
    if(col.id%in%colnames(data)&&data[,is.character(get(col.id))]){
        
        findings <- listEvents(col.id,"Leading zero may corrupt merging",
                               fun=function(x)grepl("^0.+",x),invert=TRUE,
                               events=findings,debug=FALSE)
    }

    
    cols.num.all <- intersect(
        do.call(c,cols.num.all)
       ,cnames.data.0)
##### Done checking required columns for NMisNumeric. overwrite cols.num.all with NMasNumeric of cols.num.all.
    
    data[,(cols.num.all):=lapply(.SD,NMasNumeric),.SDcols=cols.num.all]

    
### col.time must be positive
    findings <- listEvents(col.time,"Negative time",fun=function(x)x>=0,events=findings,debug=F)

### EVID must be in c(0,1,2,3,4)
    findings <- listEvents("EVID","EVID not in 0:4",function(x) x%in%c(0:4),events=findings)
    
### ID must be a positive integer
    findings <- listEvents(col.id,paste(col.id,"not a positive integer"),
                           fun=function(x)x>0&x%%1==0,events=findings,
                           new.rows.only=T)

    
### MDV should perfectly reflect is.na(DV)
    if(col.mdv%in%colnames(data)){
        
        data[,MDVDV:=!is.na(get(col.mdv))&get(col.mdv)==as.numeric(is.na(get(col.dv)))]
        findings <- listEvents("MDVDV","MDV does not match DV",colname=col.mdv,fun=function(x)x==TRUE,dat=data[get(col.evid)==0],events=findings)
    }

###  columns that are required for all rows done

#### all other required columns (NA elements OK). Run NMisNumeric for each element, then translate using NMasNumeric
    
    cols.req <- c(col.cmt,col.dv,col.amt)
    for(col in cols.req){
        
        findings <- listEvents(col,name="Not numeric",fun=function(x)NMisNumeric(x,na.strings=na.strings,each=TRUE),
                               new.rows.only=T,events=findings)
    }
    
    cols.req.found <- intersect(cols.req,colnames(data))
    if(length(cols.req.found)){
        data[,(cols.req.found):=lapply(.SD,NMasNumeric),.SDcols=cols.req.found]
    }
    
    for(cmt in col.cmt){
### CMT must be a positive integer. It can be missing for CMT=3
        findings <- listEvents(cmt,"Missing for EVID!=3",
                               fun=is.na,invert=TRUE,events=findings,
                               dat=data[EVID%in%c(1,2,4)])
        ## For EVID!=3, must be a positive integer 
        findings <- listEvents(cmt,"CMT not a positive integer",
                               fun=function(x)x>0&x%%1==0,events=findings,
                               dat=data[EVID%in%c(1,2,4)],
                               new.rows.only=T)
        ## For EVID==3, if CMT not missing, must be 0 or a positive integer
        findings <- listEvents(cmt,"CMT not a positive integer",
                               fun=function(x)x>=0&x%%1==0,events=findings,
                               dat=data[EVID%in%c(3)&!is.na(CMT)]
                               )
    }



###### DV
### DV must be present
### DV must be numeric for EVID==0
    if(col.mdv%in%colnames(data)){
        findings <- listEvents(col.dv,"DV not numeric",fun=is.na,events=findings,invert=TRUE,dat=data[EVID%in%c(0)&get(col.mdv)==0])
    } else {
        findings <- listEvents(col.dv,"DV not numeric",fun=is.na,events=findings,invert=TRUE,dat=data[EVID%in%c(0)])
    }

### DV should be NA or 0 for dosing records
    findings <- listEvents(col.dv,"DV not NA or 0 in dosing recs",fun=function(x)is.na(x)|as.numeric(x)==0,events=findings,dat=data[EVID%in%c(1,4)])


#### AMT
    ## must be numeric
    findings <- listEvents(col.amt,name="Not numeric",
                           fun=function(x)NMisNumeric(x,na.strings=na.strings,each=TRUE),
                           events=findings,                           
                           dat=data) 
    ## positive for EVID 1 and 4
    findings <- listEvents(col.amt,"Non-positive dose amounts",
                           fun=function(x)x>0,events=findings,
                           dat=data[EVID%in%c(1,4)])
    ## must be 0 or NA for EVID 0 and 2
    findings <- listEvents(col.amt,"Non-zero dose for obs or sim record",
                           fun=function(x)is.na(x)|x==0,
                           events=findings,
                           dat=data[EVID%in%c(0,2)])
    

### optional default columns - RATE, SS, ADDL, II
    cols.opt <- cc(RATE,SS,II,ADDL)
    for(col in cols.opt){
        findings <- listEvents(col,name="Not numeric",
                               fun=function(x)NMisNumeric(x,na.strings=na.strings,each=TRUE),
                               events=findings,
                               col.required=FALSE,
                               dat=data)
    }
    cols.opt.found <- intersect(cols.opt,colnames(data))
    if(length(cols.opt.found)>0){
        data[,(cols.opt.found):=lapply(.SD,NMasNumeric),.SDcols=cols.opt.found]
    }
    
    ## RATE -2 or positive for EVID%in%c(1,4)
    findings <- listEvents("RATE","Must be -2 or non-negative",
                           fun=function(x)x==-2|x>=0,events=findings,
                           col.required=FALSE,
                           dat=data[EVID%in%c(1,4)])
    
    ## RATE 0 or missing for !EVID%in%c(1,4)
    findings <- listEvents("RATE","Expecting missing or zero for non-dose events",
                           fun=function(x)is.na(x)|x==0,events=findings,
                           col.required=FALSE,
                           dat=data[!EVID%in%c(1,4)])

    ## SS 0 or 1
    findings <- listEvents("SS","must be 0 or 1",
                           col.required=FALSE,
                           fun=function(x)x%in%c(0,1),events=findings
                           )
    
    
    if("ADDL"%in%colnames(data)){
        ## Because ADDL is found, we require II. The label will be "II
        ## not found" or something loke that. The label below is not
        ## reported. We give a message because it is not clear why II
        ## is needed.
        if(!quiet && !"II"%in%colnames(data)){
            messageWrap("Column ADDL is found but not II.")
        }
        findings <- listEvents("II",name="(This label will not be used)",
                               fun=function(x)TRUE,
                               events=findings,
                               col.required=TRUE,
                               dat=data[EVID%in%c(1,4)]) 

        findings <- listEvents("ADDL","Must be 0 or missing for non-dosing events",
                               fun=function(x)is.na(x)|x==0,
                               events=findings,
                               new.rows.only=T,
                               dat=data[!EVID%in%c(1,4)])

        findings <- listEvents("ADDL","Must be a non-negative integer",
                               fun=function(x)x>=0&x%%1==0,events=findings,
                               dat=data[EVID%in%c(1,4)],
                               new.rows.only=T)
    }


    if("II"%in%colnames(data)){
        ## II only makes sense together with II
        if(!quiet && !"ADDL"%in%colnames(data)){
            messageWrap("Column II is found but not ADDL.")
        }
        findings <- listEvents("ADDL",name="(This label will not be used)",
                               fun=function(x)TRUE,
                               events=findings,
                               col.required=TRUE,
                               dat=data[EVID%in%c(1,4)])         
        ## must be 0 or na for non-dosing events
        findings <- listEvents("II","Must be 0 or missing for non-dosing events",
                               fun=function(x)is.na(x)|x==0,
                               events=findings,
                               new.rows.only=T,
                               dat=data[!EVID%in%c(1,4)])
        
        findings <- listEvents("II","Must be a positive integer when ADDL is positive",
                               fun=function(x)x>0&x%%1==0,
                               events=findings,
                               new.rows.only=T,
                               dat=data[EVID%in%c(1,4)&!is.na(ADDL)&ADDL>0])
        findings <- listEvents("II","Must not be positive when ADDL is missing or zero",
                               fun=function(x)is.na(x)|x==0,
                               events=findings,
                               new.rows.only=T,
                               dat=data[EVID%in%c(1,4)&(is.na(ADDL)|ADDL==0)])
    }
    

######## End Default columns
    

    ## ID-level checks
### Warning if the same ID is in non-consequtive rows
    data[,ID.jump:=c(0,diff(get(rowint))),by=col.id]
    findings <- listEvents("ID.jump",colname=col.id,name="ID disjoint",fun=function(x) x<=1,events=findings,debug=FALSE)

    
    
### within ID, time must be increasing. Unless EVID%in% c(3,4) or events are jumped
    data[,newID:=get(col.id)]
    if(col.time%in%colnames(data)){
        
        data[,isnewID:=get(col.id)!=shift(get(col.id),n=1)]
        data[1,isnewID:=TRUE]
        data[,reset:=FALSE]
        if("EVID"%in%colnames(data)){
            data[,reset:=EVID%in%c(3,4)]
        }
        data[,newID:=cumsum(as.numeric(isnewID)+as.numeric(reset))]
        data[,checkTimeInc:=c(TRUE,diff(get(col.time))>=0),by=.(newID)]
        
        findings <- listEvents(col="checkTimeInc",name="Time decreasing",fun=function(x) x==TRUE,
                               colname=col.time,events=findings)
    }
    
### check for duplicate events
    col.cmt.found <- intersect(col.cmt,colnames(data))
    nruns <- length(col.cmt.found)
    if(length(col.cmt.found)==0){
        col.cmt.found <- NULL
        nruns <- 1
    }
    nfindings <- findings[,.N]
    for(count.cmt in 1:nruns){
        
        data[,Nrep:=.N,by=intersect(c("newID",col.cmt.found[count.cmt],col.evid,col.time,cols.dup),colnames(data))]
        findings <- listEvents(col="Nrep",name="Duplicated event",function(x) x<2,colname=paste(c(col.id,col.cmt.found[count.cmt], col.evid, col.time, cols.dup),collapse=", "),events=findings)
    }
    nfindings.new <- findings[,.N]
    if(count.cmt>1 && nfindings.new>nfindings){
        messageWrap("Due to multiple CMT columns, duplicate events may be reported with respect to each of these columns.",fun.msg=message)
    }
    
    if(col.evid%in%colnames(data)){
### subjects without doses
        all.ids <- data[,unique(get(col.id.orig))]
        tab.evid.id <- data[,.N,by=c(col.id.orig,col.evid)]
        ids.no.doses <- setdiff(all.ids,tab.evid.id[EVID%in%c(1,4),get(col.id.orig)])

        if(length(ids.no.doses)>0){
            findings <- rbind(findings ,
                              data.table(check="Subject has no doses",column="EVID",ID=ids.no.doses,level="ID") ,
                              fill=TRUE)
        }
### subjects without observations    
        ids.no.obs <- setdiff(all.ids,tab.evid.id[EVID%in%c(0),get(col.id.orig)])
        ids.no.sim <- setdiff(all.ids,tab.evid.id[EVID%in%c(2),get(col.id.orig)])
        if(type.data=="est"){
            if(length(ids.no.obs)>0){
                findings <- rbind(findings
                                 ,
                                  data.table(check="Subject has no obs",column="EVID",ID=ids.no.obs,level="ID")
                                 ,fill=TRUE)
            }
        }
        if(type.data=="sim"){
            if(length(ids.no.sim)>0){
                findings <- rbind(findings
                                 ,
                                  data.table(check="Subject has no sim records",column="EVID",ID=ids.no.obs,level="ID")
                                 ,fill=TRUE)
            }
        }
    }


##### Section start: Covariates ####

    
### Subject-level
    list.findings.covs <- lapply(covs,function(cov){
        ncomb.cov <- unique(data[,c(col.id,cov),with=FALSE])[,.N,by=c(col.id)]
        non.covs <- ncomb.cov[N>1,c(col.id),with=FALSE]
        non.covs[,`:=`(column=cov,check="Cov not unique within ID",level="ID")]
        non.covs        
    })
    if(length(list.findings.covs)>0){
        findings <- rbind(findings,rbindlist(list.findings.covs),fill=TRUE)
    }
    

### Occasion-level
    
    ##  syntax:   covs.occ=list(PERIOD=c("FED"))
    all.occ.covs <- rbindlist(lapply(names(covs.occ),function(occ)data.table(occ=occ,cov=covs.occ[[occ]])))
    all.occ.covs[,ROW:=.I]
    if(nrow(all.occ.covs)>0){
        findings.occ <- all.occ.covs[,{
            obs.cov <- data[,c(col.id,occ,cov),with=F]
            ncomb.cov <- unique(obs.cov)[,.N,by=c(col.id,occ)]
            ncomb.cov <- ncomb.cov[N>1,c(col.id),with=FALSE]
            ncomb.cov[,`:=`(column=paste(occ,cov,sep=", "),level="ID-occasion",check="Cov not unique within ID-occ")]
            ncomb.cov
        },by=.(ROW)]
        
        ## findings.occ <- rbindlist(list.findings.occ)
        findings.occ[,ROW:=NULL]
        if(length(findings.occ)>0){
            findings <- rbind(findings,findings.occ,fill=TRUE)
        }    
    }
    
### Section end: Covariates


#### Section start: usubjid ####

    if(!missing(col.usubjid)&&!is.null(col.usubjid)){
        findings <- listEvents(col=col.usubjid,name="Missing value",
                               fun=function(x){
                                   !is.na(x) &
                                       !(as.character(x)%in%unique(c("",na.strings)))
                               },
                               events=findings)

        ## check . All ID values must be represented exactly once. If not, report an ID-level finding.
        ## how many usubjids for each ID?
        data[,NUID:=uniqueN(col.usubjid),by=c(col.id)]
        ## how many IDs for each usubjid?
        data[,NidByUID:=uniqueN(get(col.id)),by=c(col.usubjid)]
        data[,usubjNotUnique:=max(NidByUID)>1,by=c(col.id)]
        
        if(data[,sum(NUID>1)]){
            findings <- rbind(findings ,
                              data.table(check="Unique subject ID not unique within ID",column=col.usubjid,ID=data[NUID>1,get(col.id)],level="ID") ,
                              fill=TRUE)
        }

        if(data[,any(usubjNotUnique)]){
            findings <- rbind(findings,
                              data.table(check="Subject ID not unique within unique subject IDs",column=col.id,ID=data[usubjNotUnique==TRUE,get(col.id)],level="ID"),
                              fill=TRUE)
        }
        
    }
    
### Section end: usubjid


    

    return(reportFindings(findings=findings,data=data,col.id=col.id,c.row=c.row,col.row=col.row,col.row.orig=col.row.orig,col.id.orig=col.id.orig,return.summary=return.summary,as.fun=as.fun,quiet=quiet))

}


