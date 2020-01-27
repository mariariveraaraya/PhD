## ---- load-pkg
library(here)
library(dplyr)
library(purrr)
library(data.table)
library(tidyr)


## ---- tb-one-pre
dates_table<-read.csv(here("experiments", "exp_radiocarbon","data", "Compiled_Aug_19.csv"),na.strings=c("NA","#DIV/0!",""))


dates_table$ID<- gsub("SAN", "", dates_table$ID)

dates_table$OZCode<- gsub(" ", "", dates_table$OZCode,fixed = TRUE)

colnames(dates_table)[colnames(dates_table)=="ID"] <- "Identifier"

dates_table$Identifier<- as.numeric(as.character(dates_table$Identifier))

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

correctdepths$Identifier<- as.numeric(as.character(correctdepths$Identifier))

setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/experiments/exp_radiocarbon/data')


temp = list.files(pattern="*.txt")

myfun <- function(x) {
        read.table(x,skip = 2,fill=TRUE)
}

myfiles = lapply(temp, myfun)

#data55 <- rbindlist( myfiles, fill = TRUE )



org<-as.data.frame(myfiles[[1]])
org<-org[1:7,]

cellulose<-as.data.frame(myfiles[[2]])
cellulose<-cellulose[1:1,]

hypy<-as.data.frame(myfiles[[3]])
hypy<-hypy[1:13,]

pollen<-as.data.frame(myfiles[[4]])
pollen<-pollen[1:2,]

charcoal<-as.data.frame(myfiles[[5]])
charcoal<-charcoal[1:4,]

data522<- rbind(hypy,pollen,charcoal,org,cellulose)

data522<-data522%>%
        plyr::rename(c("V1"="OZCode","V2"="from_67","V3"="to_67","V4"="from_95","V5"="to_95"))

all_first_table<-data522%>%
        left_join(dates_table)%>%
        left_join(correctdepths)%>%
        select(OZCode,Depth,Conv_RC,Conv_RC_error,to_95,from_95,Carbon_fraction,Pretreatment)


all_first_table2<-all_first_table%>%
        unite("Conventional radiocarbon dates",3:4, sep=" \u00B1 ")%>%
        unite("Calibrated age range (95 %)",4:5,sep=" \u002D ")%>%
        rename("Laboratory Code"="OZCode", "Carbon fraction"="Carbon_fraction")

## ---- tb-one

all_first_table2%>%
        arrange(`Depth`)%>%
        knitr::kable(booktabs = TRUE, caption = "(ref:tb2-sub)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "hold_position")

all_first_table2%>%
        arrange(`Depth`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb2-sub)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "hold_position")
setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/writeup')


## ---- tb- two

all<-data522%>%
        left_join(dates_table)%>%
        mutate(Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(dates_table$Carbon_fraction)))%>%
        left_join(correctdepths,by="Identifier")%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction)    

