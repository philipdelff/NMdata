library(NMdata)

lsts <- list.files(NMdata_filepath("examples/nonmem"),pattern="\\.lst$",full.names=T)
l.tabs <- lapply(lsts,
   NMgetSection,section="TABLE"
   )
names(l.tabs) <- basename(lsts)

l.tabs
