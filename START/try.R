library(tidyverse)
library (markdown)
library(shiny)
library(readr)
library(shinythemes)
library(DT)

#graph_t <- read_delim("graph.csv", delim = ";", escape_double = FALSE)
#rozd<-read_delim("rozd.csv", delim = ";", escape_double = FALSE)
chapter<-read_delim("chapter.csv", delim = ";", escape_double = FALSE)
rule<-read_delim("rule.csv", delim = ";", escape_double = FALSE)
#articles<-read_delim("article.csv", delim = ";", escape_double = FALSE)
#art_vis<-read_delim("art_vis.csv", delim = ";", escape_double = FALSE)
#comment<-read_delim("comment.csv", delim = ";", escape_double = FALSE)
load ("crimes_ukr.RData")
load ("articles.RData")
articles<-articles2
name_n<-names(crimes)[5:115]  
crimes$Chapter<-as.numeric(as.roman(crimes$Chapter))
crimes[is.na(crimes)]=0
crimes$ChapterTXT<-1
for (j in 1:nrow(chapter))
  for (i in 1:nrow(crimes))
    if (crimes$Chapter[i]==chapter$chn[j]) crimes$ChapterTXT[i]<-chapter$chap[j]
bcrimes<-select(crimes,1:4,ChapterTXT,5:115)
crimes<-bcrimes

crimes$RELTOT<-0
crimes$TOTPROV<-0
crimes$IMPTOT<-0
for (i in 1:nrow(crimes))
{
  crimes$TOTPROV[i]<-sum(crimes$INDICM[i],crimes$REL[i],crimes$MED[i],crimes$EDU[i])
  crimes$IMPTOT[i]<-sum(
    crimes$LIFEIMP[i],
    crimes$IMP[i],
    crimes$RESTOL[i],
    crimes$DISBAT[i],
    crimes$ARREST[i],
    crimes$CORRW[i],
    crimes$SRVRSTR[i],
    crimes$PUBLW[i],
    crimes$FINE[i],
    crimes$DEPR[i])
  crimes$RELTOT[i]<-sum(crimes$PROB[i],crimes$RELAMN[i],crimes$RELOTHR[i])
}


crimes$ChapterR<-as.character(as.roman(as.numeric(crimes$Chapter)))
field_n<-read_delim("field_n.csv", delim = ";", escape_double = FALSE)
chapter_label <- c("I","II","III","IV","V","VI","VII","VIII","IX","X",
                   "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","КК 1960")


crimesCHPT<-select(crimes,1:3,5:120)
crimesCHPT<-crimesCHPT %>% group_by(Year, ChapterR, ChapterTXT) %>% summarise_if(is.numeric,sum)
crimesCHPT[is.na(crimesCHPT)]=0
crimesYR<-select(crimes,1:3,5:120)
crimesYR<-crimesYR %>% group_by(Year) %>% summarise_if(is.numeric,sum)

crimesCHPR<-crimesCHPT
for (yr in 2013:2022)
  
    for (ind in 4:117)
    {
      basevar<-sum(crimesCHPT[crimesCHPT$Year == yr,ind])
      crimesCHPR[crimesCHPR$Year == yr,ind]<-crimesCHPT[crimesCHPT$Year == yr,ind]/basevar*100}

