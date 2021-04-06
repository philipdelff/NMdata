if(F){
    load_all()

    newlines <- "$INPUT ROW ID TIME EVID CMT AMT DV FLAG STUDY EFF0"
    section <- "input"
   NMwriteSection( system.file("examples/nonmem/xgxr011.mod", package = "NMdata"),section=section,
                  newlines=newlines,newpath=NULL)

    ## same, with list approach
   NMwriteSection( system.file("examples/nonmem/xgxr011.mod", package = "NMdata"),
                  list.section=list(input=newlines),newpath=NULL
                  )


    text["INPUT"]
    ## with output from NMwriteData
   NMwriteSection( system.file("examples/nonmem/xgxr011.mod", package = "NMdata"),
                  list.section=text["INPUT"],newpath=NULL
                  )



}
