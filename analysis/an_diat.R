## ---- load-pkg-dia

library(here)
library(tidyverse)
library(dplyr)
library(tidyr)
library(analogue)

#main reference: mcz064_suppl Nomenclature 2.0, Neumann et al.
##Should include depths with low counts????
#Should reorganize to include the corrected version with concentration
#Stratplot with corrected counts by conc is ready and also diat, phy and sp conc
#Should group the counts by gssc/poaceae, woody (Dicotyle), 3D=blocky, mono vs dico difference?

#See proske for interpretation - before and after 5000 (Frustulia/Brachisyra vs Pinnularia/Caloneis)
#age model in exp_radiocarbon suggests that accumulation was always slow, but very quick around 22-24 ka BP


## ---- age

#or need to include read_chunk from a central file/better
ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_4/SAN8_2019_4_35_ages.txt",skip=1)

colnames(ages5)<-c("Depth","max","min","median","mean")

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))




## ---- dia-pre

diatoms_counts<-read.csv(here("experiments", "exp_diatoms","data", "Counts_diatoms_08_08_19.csv"))

diatoms_photo<-read.csv(here("experiments", "exp_diatoms","data", "Photos_silica.csv"))

###Concentration of diatoms
conc<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Diatoms spicules/Concentration diatoms.csv")


##include concentrations of biogenic silica. No phytoliths peaty layer? why? accumulation is different? different vegetation - groups that produce more or less phytoliths

######NEED TO check, cunt and include phytoliths <20 in all samples!!! Maybe SPIC species

#par(mfrow=c(1,1))


#####92???? COrrect 93 for counts and transects
#CHeck superficial, 2H or other, check fractions <20 um, 28 (frustulia vs Brachisyra), do maybe 30 and 55

#New column putting together Genus and species

diatoms_counts$Species2 <- paste(diatoms_counts$Genus,diatoms_counts$Species)
#phy_2$final_morphotype <- paste(phy_2$Morphotype,phy_2$Morphotype_b)
diatoms_counts$final_morphotype <- paste(diatoms_counts$Morphotype,diatoms_counts$Morphotype_b)


#diat_sp<-diatoms_counts %>%
# filter(Type=='Diatom')%>% 
#roup_by(Species2)

#photos<-anti_join(diat_sp,diatoms_photo)

#str(diat_sp$Species2)
#diat_sp$Species2<-as.factor(diat_sp$Species2)
#str(diatoms_photo$Species2)
#character to factor

#####Correcting by volume and amount of sediment used


#diatoms_counts2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Diatoms spicules/Counts_diatoms_08_08_19.csv")

#diatoms_counts2$Species2 <- paste(diatoms_counts2$Genus,diatoms_counts2$Species)


#Merging counts and concentration
diat_merge<-merge(diatoms_counts,conc,by="Identifier")
diat_merge$Species2[diat_merge$Species2 == "Caloneis 999"] <- "Caloneis"
diat_merge$Species2[diat_merge$Species2 == "Caloneis "] <- "Caloneis"


#diat_merge2<-diat_merge%>%
#      select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
#        filter(Type=='Diatom')%>% 
#        group_by(Identifier)%>%
#        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#mutate(Corrected_counts2=Counts/sum(Total_sediment_analysed_g))%>%
#        mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#        mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#        mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
#        mutate(corr_trans= countT2/Number_of_transects)%>%
#        filter(countT2 > 0)%>%
#       mutate(per2=paste0(round(100*Counts/countT2,2)))



#diat_merge22<-diat_merge%>%
#select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transec#ts)%>%
#       filter(Type=='Diatom')%>% 
#      group_by(Identifier)%>%
#      mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#       mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#   mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#  mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
# filter(d_g_wet_sed < 300000)


#plot(diat_merge22$Identifier,diat_merge22$d_g_wet_sed)
#diat_333 <- ggplot(diat_merge2, aes(x=Identifier,y=d_g_wet_sed))+geom_point() 
#diat_333
diat_merge3<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(Type=='Diatom')%>% 
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        filter(countT2 > 0)


diat_merge4<- diat_merge3%>%
        spread(Species2, per) 


diat_merge20<-select(diat_merge4,Identifier,11:29)

#diat_merge5<- aggregate(x=diat_merge4[,2:20], by=list(Identifier=diat_merge4$Identifier), min, na.rm = TRUE)
diat_merge11<- aggregate(x=diat_merge20[,2:20], by=list(Identifier=diat_merge4$Identifier), min, na.rm = TRUE)

diat_new<-left_join(diat_merge11,agedepth)

diat_new2<-select(diat_new,-(3:4))

diat_merge10<- aggregate(x=diat_new[,2:20], by=list(median=diat_new$median), min, na.rm = TRUE)
diat_merge100<- aggregate(x=diat_new[,2:20], by=list(Depth=diat_new$Depth), min, na.rm = TRUE)
###Need to merge with new dates and model them with 0.5 cm resolution 

#str(diat_c2)

#diat_merge5[, c(1:20)] <- sapply(diat_merge5[, c(1:20)], as.numeric)

#diat_merge5[is.na(diat_merge5)] <- 0

#str(diat_c2)


#BAR_merge <- Stratiplot(Identifier ~ . , data= diat_merge5, type = c("h","g","l"), sort = "wa")
#BAR2_merge <- Stratiplot(Identifier ~ . , data= chooseTaxa(diat_merge5, max.abun = 5, n.occ = 1),
#                   type = c("h","g"), sort = "wa")


diat_merge10[, c(1:20)] <- sapply(diat_merge10[, c(1:20)], as.numeric)

diat_merge10[is.na(diat_merge10)] <- 0

#diat_merge10<-diat_merge10[,-(1)]
diat_merge10<-diat_merge10[,-(20)]


diat_merge100[, c(1:19)] <- sapply(diat_merge100[, c(1:19)], as.numeric)

diat_merge100[is.na(diat_merge100)] <- 0

#diat_merge10<-diat_merge10[,-(1)]
diat_merge100<-diat_merge100[,-(20)]

#BAR_merge2 <- Stratiplot(median ~ . , data= diat_merge10, type = c("h","g","l"), sort = "wa")


con_diat_1<-diat_merge3%>%
        merge(agedepth)%>%
        filter(Identifier >0.1)

con_diat_2<-ggplot(con_diat_1,aes(y=median,(x=sqrt(d_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 5000)) + scale_x_continuous(limits=c(0, 600), breaks=c(0,200,400,600)) + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

#### --- gr-diat-rel

BAR2_merge2 <- Stratiplot(median~ . , data= chooseTaxa(diat_merge10, max.abun = 5, n.occ = 1),
                          type = c("h","g"), sort = "wa",xlab="Relative percentage (%)",ylab="Age (cal yr BP)")

#BAR2_merge22 <- Stratiplot(Depth~ . , data= chooseTaxa(diat_merge100, max.abun = 5, n.occ = 1),
 #                         type = c("h","g"), sort = "wa",xlab="Relative percentage (%)",ylab="Age (cal yr BP)")

#Leave surface samples out for concentration calcs
#con_diat_2<-filter(diat_new2,Identifier >0.1)

#con_diat<-ggplot(con_diat_2,aes(x=con_diat_2$Identifier,(y=sqrt(d_g_wet_sed))))+geom_point() + xlab("age")+ ylab("concentration")+ theme_bw()

#print(con_diat)


#### ---- gr-diat-conc

print(con_diat_2)


#con_diat3<-ggplot(diat_new2,aes(x=diat_new2$Identifier,(y=sqrt(d_g_wet_sed))))+geom_point() + xlab("age")+ ylab("concentration")+ theme_bw()

#print(con_diat3)


######################################spicules

#diat_merge<-merge(diatoms_counts,conc,by="Identifier")


#spi_2<-diat_merge%>%
#     select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
#      filter(Type=='Spicule')%>% 
#     group_by(Identifier)%>%
#    mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#   mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#  mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
# mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
#mutate(corr_trans= countT2/Number_of_transects)%>%
#filter(countT2 > 0)

## ---- spi-pre

spi_2<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(Type=='Spicule')%>% 
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(spi_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        filter(countT2 > 0)

#diat_merge22<-diat_merge%>%
#select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transec#ts)%>%
#       filter(Type=='Diatom')%>% 
#      group_by(Identifier)%>%
#      mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#       mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#   mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#  mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
# filter(d_g_wet_sed < 300000)


#plot(diat_merge22$Identifier,diat_merge22$d_g_wet_sed)
#diat_333 <- ggplot(diat_merge2, aes(x=Identifier,y=d_g_wet_sed))+geom_point() 
#diat_333
#spi_3<-spi_2%>%
#       filter(Type=='Spicule')%>% 
#          group_by(Identifier)%>%
#     mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#    mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#   mutate(spi_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
#  filter(countT2 > 0)

spi_222<-left_join(spi_2,agedepth)

spi_333 <- ggplot(spi_222,aes(y=median,(x=sqrt(spi_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 5000))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


#### ---- spi-conc

print(spi_333)

#spi_3333 <- ggplot(spi_222,aes(y=Depth,(x=sqrt(spi_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 175, by = 10))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

#spi_3333


