### method not specified
## simplest function call - default
if( missing(merge.by.filters) && missing(col.row)){
    merge.by.filters
    search.for.col.row
}

if(missing(merge.by.filters) && is.null(col.row) ){
    merge by filters and do not search for col.row
}


### method specified

## merge.by.filters specified - col.row can be NULL too
if(merge.by.filters && missing(col.row) ){
    merge.by.filters
}
if(!merge.by.filters && missing(col.row) ){
    input data not used
}

## col.row specified
if(missing(merge.by.filters) && col.row=="row" ){
    merge by row
}


## merge.by.filters and col.row specified
if(!merge.by.filters && col.row=="row" ){
    merge by row
}

### redundant specification

if(merge.by.filters && col.row=="row" ){
    stop()
}


#### Section start: Step 2 - more coding ####

##' @param merge.by.filters Default is to merge by filters if col.row is either missing or NULL. However, explicitly specifying merge.by.filters is recommended if that is the wanted behaviour. If not, NMscanData will search for potential columns to merge by and report. See col.row as well.

use.input <- TRUE
search.col.row <- FALSE
merge.by.filters <- FALSE
merge.by.row <- FALSE

### method not specified
## simplest function call - default
if( missing(merge.by.filters) && missing(col.row)){
    merge.by.filters <- TRUE
    search.col.row <- TRUE
    col.row <- NULL
} else if(missing(merge.by.filters) && !missing(col.row) && is.null(col.row) ){
    merge.by.filters <- TRUE


### method specified
    ## merge.by.filters specified - col.row can be NULL too
} else if(merge.by.filters && (missing(col.row) || is.null(col.row))){
    merge.by.filters <- TRUE
} else if(!merge.by.filters && (missing(col.row) || is.null(col.row))){
    use.input <- FALSE
    ## col.row specified
} else if(missing(merge.by.filters) && !missing(col.row) && !is.null(col.row) ){
    merge.by.row <- TRUE
    ## merge.by.filters and col.row specified
} else if(!merge.by.filters && !missing(col.row) && !is.null(col.row) ){
    merge.by.row <- TRUE

### redundant specification
} else if(merge.by.filters && !missing(col.row) && !is.null(col.row) ){
    stop("merge.by.filters cannot be TRUE and col.row non-NULL at the same time.")
} else {
    stop("A non-recognized combination of merge.by.filters and col.row. Please see the documenation of those two arguments.")
}

if(merge.by.filters && merge.by.row) {stop("This is a bug. Please report.")}

### now code must use search.col.row, merge.by.filters and merge.by.row

### Section end: Step 2 - more coding

