### dsa.Rdata additional actions
dsa$Chapter<-as.numeric(as.roman(dsa$Chapter))
dsa[is.na(dsa)]=0
dsa$ChapterTXT<-1
for (j in 1:nrow(chapter))
  for (i in 1:nrow(dsa))
    if (dsa$Chapter[i]==chapter$chn[j]) dsa$ChapterTXT[i]<-chapter$chap[j]

dsa<-select(dsa,1:5,ChapterTXT,6:108)

dsa$RELTOT<-0
dsa$TOTPROV<-0
dsa$IMPTOT<-0
for (i in 1:nrow(dsa))
{
 
  dsa$IMPTOT[i]<-sum(
    dsa$LIFEIMP[i],
    dsa$IMP[i],
    dsa$RESTOL[i],
    dsa$DISBAT[i],
    dsa$ARREST[i],
    dsa$CORRW[i],
    dsa$SRVRSTR[i],
    dsa$PUBLW[i],
    dsa$FINE[i],
    dsa$DEPR[i])
  dsa$RELTOT[i]<-sum(dsa$PROB[i],dsa$RELAMN[i],dsa$RELOTHR[i])
}


dsa$ChapterR<-as.character(as.roman(as.numeric(dsa$Chapter)))
save (dsa,file = "RESULT/crimes_dsa_ukr.RData")

dsaPR<-dsa
for (yr in 2013:2022)
{
  art_yr<-unique(dsaPR$Article[dsaPR$Year==yr])
  for (art_n in 1:length(art_yr))
  {
    for (indx in 7:112)
      {base2<-sum(dsa[dsa$Year==yr&dsa$Article==art_yr[art_n],indx])
    dsaPR[dsaPR$Year==yr&dsaPR$Article==art_yr[art_n],indx]<-round(dsa[dsa$Year==yr&dsa$Article==art_yr[art_n],indx]/base2*100,2)}
    
  }
}
save (dsa, dsaPR, file = "RESULT/crimes_dsa_ukr.RData")