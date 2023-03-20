#Creation of sumtable by year

by.one<-subset(crimes, Year==2050)
for (cls in 5:117)
{by.one[1,cls]<-sum(crimes[,cls],na.rm = TRUE)}
save (by.one, file="by_one.RData")


by.year<-subset(crimes, Year==2050)
for (Yr in 2013:2022)
{

by.year[Yr-2012,3]<-Yr
Cur_Year<-subset(crimes, Year==Yr)
for (cls in 5:117)
{by.year[Yr-2012,cls]<-sum(Cur_Year[,cls],na.rm = TRUE)}

}
save (by.year, file="by_year.RData")


chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","LX") 
by.chapter<-subset(crimes, Year==2050)
for (chpt in 1:21)
{
  
  by.chapter[chpt,4]<-chapter_ord[chpt]
  Cur_chpt<-subset(crimes, Chapter==chapter_ord[chpt])
  for (cls in 5:117)
  {by.chapter[chpt,cls]<-sum(Cur_chpt[,cls],na.rm = TRUE)}
  
}
save (by.chapter, file="by_chapter.RData")



by.chpt.y<-subset(crimes, Year==2050)
idx<-1
for (yr in 2013:2022)
{
crimesY<-subset(crimes, Year==yr)
  for (chpt in 1:21)
{
  
  by.chpt.y[idx,4]<-chapter_ord[chpt]
  by.chpt.y[idx,3]<-yr
  Cur_chpt<-subset(crimesY, Chapter==chapter_ord[chpt])
  for (cls in 5:117)
  {by.chpt.y[idx,cls]<-sum(Cur_chpt[,cls],na.rm = TRUE)}
  idx<-idx+1
}

}
  save (by.chpt.y, file="by_chpt_y.RData")








article_nbrs<-unique(crimes$Article)
by.art<-subset(crimes, Year==2050)


for (artn in 1:length(article_nbrs))
    {by.art[artn,1]<-article_nbrs[artn]
Cur_art<-subset(crimes, Article==article_nbrs[artn])
by.art[artn,4]<-Cur_art$Chapter[1]
by.art[artn,2]<-Cur_art$nazva[1]
for (cls in 5:117)
{by.art[artn,cls]<-sum(Cur_art[,cls],na.rm=TRUE)}
  
}

save (by.art, file="by_art.RData")


articles2<-select(by.art, c(4,1,2))
names (articles2)<-names(articles_ukr)
articles2$chapt<-as.numeric(as.roman(articles2$chapt))
articles2<-arrange (articles2, short_n, chapt, by_group=TRUE)
save (articles2, file="articles.RData")