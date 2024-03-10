## extract upper triangle of a matrix an return as a data.table
mat2dt <- function(x,triangle="lower"){
    
    names.i <- rownames(x)
    if(is.null(names.i)) names.i <- as.character(1:nrow(x))
    names.j <- colnames(x)
    if(is.null(names.j)) names.j <- as.character(1:ncol(x))
    
    dt <- copy(as.data.table(x))
    
    colnames(dt) <- as.character(1:ncol(dt))
    dt[,i:=.I]
    
    dt <- melt(dt,id.vars="i",variable.name="j")
    dt[,j:=as.numeric(j)]

    dt <- mergeCheck(dt,
                     data.table(i=1:length(names.i),parameter.i=names.i) ,
                     by="i")
    dt <- mergeCheck(dt,data.table(j=1:length(names.j),parameter.j=names.j),by="j")

    dt <- switch(triangle,
                 upper=dt[j<=i],
                 lower=dt[i<=j]
                 )
    dt
}
