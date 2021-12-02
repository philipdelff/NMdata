##' stamp a dataset or any other object
##'
##' Dataset metadata can be valuable, eg. by tracing an archived dataset
##' back to the code that generated it. The metadata added by
##' NMstamp can be accessed using the function NMinfo.
##'
##' NMstamp modifies the meta data by reference. See example.
##'
##' @param data The dataset to stamp.
##' @param script path to the script where the dataset was generated.
##' @param time the time stamp to attach. Default is to use cpu clock.
##' @param ... other named metadata elements to add to the dataset. Example:
##'     Description="PK data for ph1 trials in project".
##' @seealso NMinfo
##' @family DataCreate
##' @return data with meta data attached. Class unchanged.
##' @examples
##' x=1
##' NMstamp(x,script="example.R",description="Example data")
##' NMinfo(x)
##' @export


NMstamp <- function(data,script,time=Sys.time(),...){

    writeNMinfo(data,
                list(dataCreate=list(
                         DataCreateScript=script,
                         CreationTime=time,
                         ...
                     )),byRef=TRUE
                )
    return(invisible(data))

    
}

