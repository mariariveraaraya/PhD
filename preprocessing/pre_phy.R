
library(here)
library(tidyverse)
#### agedepth file final!!!! include
##Include phy class for modern phyt
##group them by plant group and meaning see other thesis

#Paleoecological potential of phytoliths from lake sediment records from the tropical lowlands of Bolivia


#main reference: mcz064_suppl Nomenclature 2.0, Neumann et al.
##Should include depths with low counts????
#Should reorganize to include the corrected version with concentration
#Stratplot with corrected counts by conc is ready and also diat, phy and sp conc
#Should group the counts by gssc/poaceae, woody (Dicotyle), 3D=blocky, mono vs dico difference?

morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)


#next step is to count "wood" or globular granulate phytoliths
#check thesis to see what it means to have the different types ??? of phytoliths


diatoms_counts<-read.csv(here("experiments", "exp_diatoms","data", "Counts_diatoms_08_08_19.csv"))

diatoms_photo<-read.csv(here("experiments", "exp_diatoms","data", "Photos_silica.csv"))

###Concentration of diatoms
conc<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Diatoms spicules/Concentration diatoms.csv")
diatoms_counts$Species2 <- paste(diatoms_counts$Genus,diatoms_counts$Species)
diatoms_counts$final_morphotype <- paste(diatoms_counts$Morphotype,diatoms_counts$Morphotype_b)


#Merging counts and concentration
diat_merge<-merge(diatoms_counts,conc,by="Identifier")


#####Modern reference

#### --- phy-pre

#Phytoliths
#phy_2<-diat_merge%>%
#     #select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
#      filter(Type=='Phytoliths')%>% 
#     group_by(Identifier)%>%
#    mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#   mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#   mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#   mutate(phy_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
#   mutate(corr_trans= countT2/Number_of_transects)%>%
#  filter(countT2 > 0)

morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)

#next step is to count "wood" or globular granulate phytoliths
#check thesis to see what it means to have the different types ??? of phytoliths


phy_2<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects,final_morphotype)%>%
        filter(Type=='Phytoliths')%>% 
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(phy_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        filter(countT2 > 0)


phy_2$final_morphotype<- gsub("3D","blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("blocky faceted","Blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("cylindroid","Elongate entire cylindrical", phy21$final_morphotype)
phy_2$final_morphotype<- gsub("999","Unidentified", phy21$final_morphotype)

#silica skeleton/elongate psilate


phy_21<-phy_2
#gssc<-c("Saddle","Bilobate","Trapezoid oblong")
#poaceae<-c("bulliform","Cuneiform bulliform")
#non_diag<-c("Acicular","Elongate echinate", "Elongate sinuous", "Elongate sinuate","Tracheids")
#grasses_sedges<-c("cylindroid", "Elongate entire", "Elongate psilate", "Elongate facetated")
#woody<-c("Globular granulate", "3D")

#globular granulate = globular decorated

#elongate blocky = elongate psilate = elongate rugose = elongate entire

####Transform to ICPN 2.0
#https://academic.oup.com/aob/article/124/2/189/5537002


phy21$final_morphotype<- gsub("Saddle","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Bilobate","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Trapezoid oblong","gssc", phy21$final_morphotype)



phy21$final_morphotype<- gsub("bulliform","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Cuneiform bulliform","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Cuneiform poaceae","poaceae", phy21$final_morphotype)


phy21$final_morphotype<- gsub("Acicular","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate echinate","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate sinuous","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate sinuate","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Tracheids","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Unidentified","non_diag", phy21$final_morphotype)


phy21$final_morphotype<- gsub("Elongate entire cylindrical","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate entire","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate psilate","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Blocky faceted","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate blocky","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate rugose","grasses_sedges", phy21$final_morphotype)

phy21$final_morphotype<- gsub("Globular granulate","woody", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate facetated","woody", phy21$final_morphotype)

phy21$final_morphotype<- gsub("Elongate","non_diag", phy21$final_morphotype)

phy21$final_morphotype<- gsub("grasses_sedges ","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("non_diag ","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("gssc ","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("poaceae ","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub(" poaceae","poaceae", phy21$final_morphotype)

#Graph by morphotype
phy_222<-left_join(phy_2,agedepth)%>%
        filter(final_morphotype!="non_diag")
phy_223<-ggplot(phy_222, aes(x=phy_222$median,y=Corrected_counts, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

phy_223

library(plotly)

ggplotly(phy_223)

#str(phy_222)
phy_222$final_morphotype<-as.factor(phy_222$final_morphotype)


#Concentration graph
phy_444<-ggplot(phy_222, aes(y=median,(x=sqrt(phy_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 5000))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

phy_444


phy_445<-ggplot(phy_222, aes(y=Depth,(x=sqrt(phy_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 175, by = 10))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

phy_445


######Phytoliths just globular granulate
phy_4<-diat_merge%>%
        #select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(final_morphotype=='Globular granulate')%>% 
        group_by(Identifier)%>%
        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
        mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
        mutate(glob_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        mutate(corr_trans= countT2/Number_of_transects)%>%
        filter(countT2 > 0)

phy_44<-left_join(phy_4,agedepth)
phy_443<-ggplot(phy_44, aes(x=phy_44$median,y=sqrt(glob_g_wet_sed)))+geom_point()

phy_443

phy_333 <- ggplot(phy_222, aes(x=phy_222$median,y=spi_g_wet_sed))+geom_point() 
phy_333


# Modern reference

phy_modern<-read.csv(here("experiments", "exp_phy","data","Phytolith_modern.csv"))


phy_modern2<-phy_modern%>%
        filter(Sci_name!="")

length(unique(phy_modern2$Sci_name))

# 20 different species
phy_modern3<-phy_modern%>%
        filter(Sci_name=="")
#no scientific name: 2 poaceae, Melaleuca, Eucalyptus y Acacia; orange creeper?

phy_modern4<-phy_modern%>%
        filter(Phy_Presence==1)
#https://davekimble.net/rainforest/pandanus.htm
#with phytoliths/ 2 poaceae, pandanus, palm
print(phy_modern4$Family)

#table include Commom_name, Family, Sci_name, Phy_abundance, Modern_reference,Phy_presence
#add morphotype from pics same morphotypes in thesis_sci



      