### addTAD tests
setcolorder(dpk2.new,colnames(dpk2))
expect_equal(dpk2,dpk2.new)
all.equal(dpk2,dpk2.new)
setindex(dpk2,NULL)
all.equal(dpk2,dpk2.new)

dpk2[,.N,by=.(EVID,is.na(NDOSPERIOD))]
dpk2.new[,.N,by=.(EVID,is.na(NDOSPERIOD))]

dpk2[is.na(NDOSPERIOD),summary(TIME)]
dpk2.new[is.na(NDOSPERIOD),summary(TIME)]

ndos <- mergeCheck(dpk2,dpk2.new[,.(REC,NDOSPERIOD,NTPD,NTAD)],by="REC")
ndos[,.N,by=NDOSPERIOD.x==NDOSPERIOD.y]
ndos[,.N,by=NTAD.x==NTAD.y]
ndos[,.N,by=NTPD.x==NTPD.y]
### addTAD tests end
