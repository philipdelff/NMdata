library(NMdata)
lapply(
    list.files(NMdata_filepath("examples/nonmem"),pattern="\\.lst$",full.names=T)
       ,NMgetSection,section="TABLE")
