

#### Section start: A list with an overview of certain sections from different models ####

library(NMdata)

lsts <- list.files(NMdata_filepath("examples/nonmem"),pattern="\\.lst$",full.names=T)
l.tabs <- lapply(lsts,
   NMgetSection,section="TABLE"
   )
names(l.tabs) <- basename(lsts)

l.tabs

l.prob <- lapply(lsts,
   NMgetSection,section="PROBLEM"
   )
names(l.prob) <- basename(lsts)

l.prob


l.all <- lapply(lsts,
                function(lst){
                    list(
                        NMgetSection(lst,section="PROBLEM")
                       ,NMgetSection(lst,section="DATA")
                       ,NMgetSection(lst,section="TABLE")
                    )
                }
   )
names(l.all) <- basename(lsts)

l.all


###  Section end: A list with an overview of certain sections from different models


