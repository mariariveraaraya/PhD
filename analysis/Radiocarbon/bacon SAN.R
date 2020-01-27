##THis spreadsheet is for the processing of both Bacon and Oxcal samples.
library(ggplot2)
library(dplyr)
library(rbacon)
library(readxl)

##Oxcal better for individual samples, maybe all with Oxcal????
###Bacon with hypy dates (March 2019)
#setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon")
Bacon(core="SAN8_2019_2",d.min=40)


setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon")

Bacon("SAN8_2019_2", cc=3)

Bacon("SAN8_2019_2",cc=3,depths.file=TRUE)

Bacon("SAN8_2019_3", cc=3)

setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon")

###New model was done on Oct 23th 2019

Bacon("SAN8_2019_5", cc=3,depths.file=TRUE)


setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon")

###New model was done on Oct 23th 2019

Bacon("SAN8_2019_6", cc=3,depths.file=TRUE)


#With depths/ages for ITRAX

library(here)
data2<-read.csv(here("data", "SAN8_2019_compare_rainy2.csv"))



###The following graph compares the previous dates we have for SAN with mines.

compare_rainy2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/SAN8_2019_compare_rainy2.csv")

compare_rainy2<-compare_rainy2[1:14,]

compare_rainy2$When <- as.character(as.numeric(compare_rainy2$When))


g10<-ggplot(compare_rainy2, aes(x=depth,y=age, group=When ,color=When ))+geom_line()+geom_point()+theme_bw()
print(g10)

####Radiocarbon dates

Rc.dates<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Results/Book1.csv")

###cleaning names and taking out prefixes and suffixes

Rc.dates$ID<- gsub("SAN", "", Rc.dates$ID)
Rc.dates$ID<- gsub("- U1", "", Rc.dates$ID)
Rc.dates$ID<- gsub("- U2", "", Rc.dates$ID)
Rc.dates$ID<- gsub("\\s", "", Rc.dates$ID)
Rc.dates$Sample.Type<- gsub("Sediment", "", Rc.dates$Sample.Type)


###Include identifier so it can be merged
colnames(Rc.dates)[colnames(Rc.dates)=="ID"] <- "Identifier"

###Include real depths

merge.Rc.depths<-merge(Rc.dates,replacement_depths_itrax,by="Identifier")

str(replacement_depths_itrax)
str(Rc.dates)
Rc.dates$Identifier <- as.factor(as.factor(Rc.dates$Identifier))

###prepare for Oxcal Name, Date, Uncertainty, Depth
##Oxcal is not user friendly. To use it: go to file, open the project, go the "code" view (younger dates are at the bottom) and then file and run.
##Once it is finished, the output can be taken from View and tab delimited (txt file) or go to the menu in the right and select raw data then click save (csv) or plot, to do a personalized plot.

##This section of the code was intented to be used to import txt and csv to Oxcal..but it did not work,
##Select just Hypy
oxcal.hypy <- merge.Rc.depths[which(merge.Rc.depths$Sample.Type=="Hypy  "), ]
##Select columns of interest
oxcal.hypy2<-select(oxcal.hypy,Code,RCA,RCA.error,Real.depth)

##Change column names so it could be imported to Oxcal (it did work at the end)

colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="Code"] <- "Name"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="RCA"] <- "Date"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="RCA.error"] <- "Uncertainty"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="Real.depth"] <- "Depth"


###Delete commas in the dates

oxcal.hypy2$Date<- gsub(",", "", oxcal.hypy2$Date)

write.csv(oxcal.hypy2, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Results/oxcal.hypy2.csv',row.names = F)

###Now bulk organics

oxcal.org <- merge.Rc.depths[which(merge.Rc.depths$Sample.Type=="Bulk organics"), ]
oxcal.org22<-select(oxcal.org,Code,RCA,RCA.error,Real.depth)

###Combine bulk organics with hypy (rbind is used when you have the same columns)
comb.hypy.org<-rbind(oxcal.hypy,oxcal.org)##uncalibrated









###Compare bulk organics/hypy#calibrated#these files are the outputs from Oxcal

compare_org<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANOrg101U22.csv")
compare_hypy<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANHypy101U22.csv")

compare_pollen<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/Pollen.csv")

#compare_hypy1<-subset(compare_hypy,name!="depthModel"& z!=160)

##Delete modelled dates and stay just with the ones I need from the depths I measured
compare_hypy1<-subset(compare_hypy,name!="depthModel")
compare_hypy1<-mutate(compare_hypy1,Fraction="Hypy")
compare_org1<-subset(compare_org,name!="depthModel")
compare_org1<-mutate(compare_org1,Fraction="Organics")

compare_both<-rbind(compare_hypy1,compare_org1)

##Calculate the median
compare_both<-mutate(compare_both,mediana=((from_95_4)+(to_95_4))/2)

library(ggplot2)
###this is the one finalgraph
g3<-ggplot(compare_both)+ geom_errorbar(data = compare_both, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction, width = 7))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()

print(g3)

###Delete X column
compare_both<-compare_both[,-5]
####
###Charcoal

charcoal.dates<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANcharcoal.txt")

colnames(charcoal.dates)[colnames(charcoal.dates)=="V1"] <- "name"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V2"] <- "from_67"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V3"] <- "to_67"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V4"] <- "from_95_4"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V5"] <- "to_95_4"


charcoal.dates<-mutate(charcoal.dates,Fraction=ifelse(name=="OZX765U1", "Charcoal_250","Charcoal_63"))
charcoal.dates<-mutate(charcoal.dates,z=6)
charcoal.dates<-charcoal.dates[,-2]
charcoal.dates<-charcoal.dates[,-2]


compare_both2<-compare_both[,-6]

compare.all<-rbind(charcoal.dates,compare_both2)

compare_all2<-rbind(compare.all,compare_pollen)

compare_mean2<-compare_all2%>%
        mutate(mean_2=((from_95_4)+ (to_95_4))/2)

g4<-ggplot(compare.all)+ geom_errorbar(data = compare_all2, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction, size=0.1, width = 0.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()


print(g4)

g5<-ggplot(compare.all)+ geom_errorbar(data = compare_all2, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction,width = 6))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()


print(g5)

g10<-ggplot(compare_mean2,aes(x=z,y=mean_2))+xlab("Depth(cm)")+ylab("Calibrated date BP")

g11<-g10+geom_point(aes(group=Fraction,color=Fraction))

g12<-g11+geom_point(aes(shape=Fraction,size=6))

g13<-g12+guides(size=FALSE)

g13
#g10<-ggplot(compare.all)+ geom_errorbar(data = compare_mean2, aes(x=z,y=mean_2,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction,width = 6))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
       # scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()

print(g11)

#install.packages("tabulizer")
#install.packages("rJava")
#library(rJava)
#library(tabulizer)
#Sys.getenv("JAVA_HOME")

