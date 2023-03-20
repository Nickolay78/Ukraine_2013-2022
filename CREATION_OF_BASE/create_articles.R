#Creation of sumtable by year


chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","LX") 



article_nbrs<-unique(crimes$Article)
by.art<-subset(crimes, Year==2050)


for (artn in 1:length(article_nbrs))
    {by.art[artn,1]<-article_nbrs[artn]
Cur_art<-subset(crimes, Article==article_nbrs[artn])
by.art[artn,4]<-Cur_art$Chapter[1]
by.art[artn,2]<-Cur_art$nazva[1]
for (cls in 6:119)
{by.art[artn,cls]<-sum(Cur_art[,cls],na.rm=TRUE)}
  
}

save (by.art, file="by_art.RData")


articles2<-select(by.art, c(4,1,2))
names (articles2)<-names(articles_ukr)
articles2$chapt<-as.numeric(as.roman(articles2$chapt))
articles2<-arrange (articles2, short_n, chapt, by_group=TRUE)
save (articles2, file="RESULT/articles.RData")