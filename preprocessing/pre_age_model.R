

library(tidyr)
library(dplyr)

#14.08.19 This file is trying to clean variables and make the code more efficient! probably stick with just the depths for now?
#Create a file with dates and depths so it can be used in other analyses

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)

#Wrong!! do not use!!!!!
#ages2<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_2/SAN8_2019_2_35_ages.txt",skip=1)

#ages2<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_3/SAN8_2019_3_31_ages.txt",skip=1)

#ages3<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_3/SAN8_2019_3_31_ages.txt",skip=1)
#ages2<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/R analysis/SAN8_2019_24_ages.txt",skip=1)

##Last model 27/8/19
#ages4<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_2/SAN8_2019_2_35_ages.txt",skip=1)


##Last model 23/9/19
#ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_5/SAN8_2019_5_35_ages.txt",skip=1)


#colnames(ages2)<-c("Depth","max","min","median","mean")
#colnames(ages3)<-c("Depth","max","min","median","mean")
#colnames(ages4)<-c("Depth","max","min","median","mean")
#colnames(ages5)<-c("Depth","max","min","median","mean")


#agedepth2<-left_join(correctdepths,ages3,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax
#agedepthb<-merge(correctdepths,ages3,by="Depth")
#agedepth<-left_join(correctdepths,ages3,by="Depth")
#agedepth2<-left_join(correctdepths,ages5,by="Depth")
#write.csv(agedepth2,file="agedepth6.csv")     
#merge with age-depth model#MUST change this file with new dates!

#radiocarbon<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/Radiocarbon.csv", header=TRUE)
#colnames(radiocarbon)<-c("ID","Depth (cm)","Conventional age (yr BP)","Error","pMC","Calibrated age (cal yr BP)","Error")

#ages_final<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
#colnames(ages_final)<-c("Depth","max","min","median","mean")
#colnames(ages4)<-c("Depth","max","min","median","mean")
#colnames(ages_final)<-c("Depth","max","min","median","mean")


#agedepth2<-left_join(correctdepths,ages3,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax
#agedepthb<-merge(correctdepths,ages3,by="Depth")
#agedepth_final<-left_join(correctdepths,ages_final,by="Depth")
#agedepth_final$Identifier<-as.numeric(as.character(agedepth_final$Identifier))
#agedepth2<-left_join(correctdepths,ages5,by="Depth")
#write.csv(agedepth2,file="agedepth6.csv")  



## Age model 
correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)%>%
        filter(Depth <14 | Depth>21)



ages_final<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
colnames(ages_final)<-c("Depth","max","min","median","mean")


agedepth<-merge(correctdepths,ages_final,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))


agedepth2<-mutate(agedepth,Identifier=ifelse(Depth==47, (75), Identifier))
agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))


## Age model ITRAX

ages_final_ITRAX<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
colnames(ages_final_ITRAX)<-c("Depth","max","min","median","mean")

ages_final_ITRAX2<-ages_final_ITRAX%>%
        mutate(Depth.mm=Depth*10)