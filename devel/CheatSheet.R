library(data.table)
library(flextable)
library(magrittr)

dt <- fread("CheatSheet.csv",header=T)
dt[,ROWF:=.I]

dt.groups <- fread(text="GRP,group,Task
1,general,General
3,dataCreate,Data set creation
4,DataRead,Read Nonmem results
5,Nonmem,Nonmem editing
6,debug,Debugging
7,misc,Miscellaneous")
    
dt2 <- merge(dt,dt.groups,by="Task")
setnames(dt2,"function","fun")

## dt[,group:=factor(group,levels=c("general","dataCreate","DataRead","Nonmem","debug","misc"))]
setorder(dt2,GRP,ROWF)



gdt <- as_grouped_data(x =
                           dt2[,.(fun,description,Task)]
                     , groups = c("Task"))
head(gdt)

rows.header2 <- which(!is.na(gdt$Task))

zz <- flextable::as_flextable(gdt) %>%
  bold(j = 1,i=~!is.na(Task), bold = TRUE, part = "body") %>%
##   bold(part = "header", bold = TRUE) %>%
    colformat_double(i = ~ is.na(Task), j = "fun", digits = 0, big.mark = "") %>%
##     merge_at(i=~!is.na(Task),j=1:2) %>%
    delete_part(part="header") %>%
    delete_part(part="footer") %>%
#    autofit()
width(j=1,width=2)  %>%
width(j=2,width=5)
zz

getwd()
save_as_pptx(zz,path="Cheatsheet1.pptx")
