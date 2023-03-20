
library ("tidyverse")
load ("dsa_clean.RData")
dsa_clean$REG<-row.names(dsa_clean)#це ключове поле номери рядків у dsa_clean
# це фрейм для збирання даних про санкції КК
new.frame<-data.frame("REG"=dsa_clean$REG,
                       "RED"=dsa_clean$Year,
                      "Chapter"=dsa_clean$Chapter,
                      "Article"=dsa_clean$Article,
                      "Part"=dsa_clean$Part,
                      "ShortN"=dsa_clean$nazva,
                      "LIMP"=0,
                      "IMPMAX"=0,
                      "IMPMIN"=0,
                      "DISBATMAX"=0,
                      "DISBATMIN"=0,
                      "RESTOLMAX"=0,
                      "RESTOLMIN"=0,
                      "ARMAX"=0,
                      "ARMIN"=0,
                      "LIMMAX"=0,
                      "LIMMIN"=0,
                      "CORWMAX"=0,
                      "CORWMIN"=0,
                      "CIVMAX"=0,
                      "CIVMIN"=0,
                      "DEPRMAX"=0,
                      "DEPRMIN"=0,
                      "FINEMAX"=0,
                      "FINEMIN"=0,
                      "CONF"=0,
                      "DEPRDOP"=0,
                      "DEPRDOPMAX"=0,
                      "DEPRDOPMIN"=0,
                      "FINEDOP"=0,
                      "FINEDOPMAX"=0,
                      "FINEDOPMIN"=0,
                      "GRADEMAN"=0,
                      "GRADEAUTO"=0)
write.csv(new.frame,file = "blank_frame.csv")

