


## ---- load-02

library(here)
library(tidyverse)

## ---- fire-map
############################101 graph
## preprocessing

sand_silt_clay<-read.csv(here("experiments", "exp_grain","data", "GS_04_10_19_av_sand.csv"),na.strings=c("NA","#DIV/0!",""))


### CLean original names


sand_silt_clay$Sample.Name <- gsub("- Average", "", sand_silt_clay$Sample.Name)
#final$Sample.Name <- gsub("-Average", "", final$Sample.Name)
#final$Sample.Name <- gsub("SAN", "", final$Sample.Name)
#final$Sample.Name <- gsub("", "", final$Sample.Name)
#final$Sample.Name <- gsub(" trial", "", final$Sample.Name)
#final$Sample.Name <- gsub(" March 19", "", final$Sample.Name)
#final$Sample.Name <- gsub("-2", "", final$Sample.Name)
#final$Sample.Name <- gsub(" 2", "", final$Sample.Name)



#Change 10FD to surface

#final$Sample.Name[10:13]=0

##Take out "trial"
#final10<-final[-c(9,14),]
str(sand_silt_clay)
#COnvert sample name to numeric/Identifier
sand_silt_clay$Sample.Name<- as.numeric(as.character(sand_silt_clay$Sample.Name))

colnames(sand_silt_clay)[colnames(sand_silt_clay)=="Sample.Name"] <- "Identifier"

#Calculate means grouping by identifier and taking out NAs derived from trials

sand_silt_clay2<-sand_silt_clay%>%
        rename("0.01_to_7_mic"= Result.0.01µm.7.00µm, "7_to_63_mic"= Result.7.00µm.63.00µm, "63_to_1000_mic"=Result.63.00µm.1000.00µm)%>%
        group_by(Identifier) %>% 
        summarize(Clay = mean(`0.01_to_7_mic`),Silt=mean(`7_to_63_mic`), Sand=mean(`63_to_1000_mic`)) %>%
        filter(!is.na(Identifier))

#?summarize
#?aggregate
#?group_by
#summarize used after groupby


###########MAYBE include correction to account for more sand than silt in 107-112....135 178 etc
#Include a correction given the inconsistent use of NaOH to eliminate silica....samples <70 have overcounting of Si in the Silt fraction
#so they were included in the clay fraction
#final22<-final2%>%
        #mutate(Siltclay=Silt + Clay)%>%
        #mutate(Silt2=ifelse(Identifier<70,(Siltclay*0.1), Silt))%>%
        #mutate(Clay2=ifelse(Identifier<70,(Siltclay*0.9), Clay))

#Transform data so it can be used in strat.plots or area graphs
sand_silt_clay3<-gather(sand_silt_clay2,Fraction,Percentage,-Identifier)

graphpp<-ggplot(sand_silt_clay3, aes(x=Identifier,y=Percentage,fill=Fraction))+ geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=12),
                                                                                                                                  axis.title=element_text(size=12,face="bold"))
print(graphpp)

###See changes in sizes need to put first row as header! then gather?

sizes<-read.csv(here("experiments", "exp_grain","data", "GS_04_10_19_av.csv"),na.strings=c("NA","#DIV/0!",""))
sizes$Sample.Name <- gsub("- Average", "", sizes$Sample.Name)


sizes2<-sizes%>%
        select(-(2:5))
sizes3<-mutate(sizes2,sum=sum(3,6:53))



#####AGES

ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_4/SAN8_2019_4_35_ages.txt",skip=1)

colnames(ages5)<-c("Depth","max","min","median","mean")

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))

grain_age<-left_join(sand_silt_clay3,agedepth)
grain_age2<-filter(grain_age,mean!="NA")
grain_age2$median <- as.numeric(grain_age2$median)

library(ghibli)
##problem with area graph and switching axis????


graphpp<-ggplot(grain_age2, aes(x=median,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=12),
                                                                                                                                           axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 5000))
print(graphpp)
#str(grain_age2)
#str(sand_silt_clay3)

graphpp2<-ggplot(grain_age2, aes(x=median,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=18),
                                                                                                                                                                                     axis.title=element_text(size=18,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 5000))

graphpp2


graphpp22<-ggplot(grain_age2, aes(x=Depth,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=18),
                                                                                                                                                                                      axis.title=element_text(size=18,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 175, by = 10))

graphpp22

