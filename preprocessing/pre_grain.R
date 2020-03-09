


## ---- load-02

library(here)
library(tidyverse)
library(janitor)

## ---- fire-map
############################101 graph
## preprocessing

########################## add gravel!!!!

###include old with oold samples!!!



source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')
source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_ITRAX.R')
source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_MAR.R')
source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_EA.R')


#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))
setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD')


sand_silt_clay<-read.csv(here("experiments", "exp_grain","data", "GS_04_10_19_av_sand.csv"),na.strings=c("NA","#DIV/0!",""))

old<-read.csv(here("experiments", "exp_grain","data", "GS_old.csv"),na.strings=c("NA","#DIV/0!",""))


### CLean original names


sand_silt_clay$Sample.Name <- gsub("- Average", "", sand_silt_clay$Sample.Name)
old$Sample.Name <- gsub("- Average", "", old$Sample.Name)
old$Sample.Name <- gsub("SAN", "", old$Sample.Name)
old$Sample.Name <- gsub(" -Average", "", old$Sample.Name)
old$Sample.Name <- gsub(" March 19", "", old$Sample.Name)


old2<-old%>%
        janitor::row_to_names(1)%>%
        filter(Sample.Name!="Trial ",Sample.Name!="10FD ",Sample.Name!="179 trial ",Sample.Name!="149 trial",Sample.Name!="149 trial 2",Sample.Name!=" 10FD ",Sample.Name!=" 97-2 ")
        
     

old2$Sample.Name <- gsub(" - ", "", old2$Sample.Name)

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

old2$Sample.Name<- as.numeric(as.character(old2$Sample.Name))

colnames(old2)[colnames(old2)=="Sample.Name"] <- "Identifier"

#Calculate means grouping by identifier and taking out NAs derived from trials

sand_silt_clay2<-sand_silt_clay%>%
        rename("0.01_to_7_mic"= Result.0.01µm.7.00µm, "7_to_63_mic"= Result.7.00µm.63.00µm, "63_to_1000_mic"=Result.63.00µm.1000.00µm)%>%
        group_by(Identifier) %>% 
        summarize(Clay = mean(`0.01_to_7_mic`),Silt=mean(`7_to_63_mic`), Sand=mean(`63_to_1000_mic`)) %>%
        filter(!is.na(Identifier))%>%
        mutate(Total=Clay+Silt+Sand)%>%
        mutate(Sand2=ifelse(Total!=(100),(Sand+(100-Total)),Sand))%>%
        select(1,2,3,6)


#agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))
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

sizes_10<-sizes%>%
        janitor::row_to_names(1)

colnames(sizes_10)[1] <- "Sample.Name"
colnames(sizes_10)[2] <- "d.0.1"       
colnames(sizes_10)[3] <- "d.0.5"
colnames(sizes_10)[4] <- "d.0.9"
#colnames(sizes) <- as.character(unlist(sizes[1,]))
#sizes = sizes[-1, ]


sizes_10$Sample.Name <- gsub("- Average", "", sizes_10$Sample.Name)


#sizes2<-sizes_10%>%
 #       select((1:4))
#sizes3<-colSums(sizes,6:73)


sizes_10$Sample.Name<- as.numeric(as.character(sizes_10$Sample.Name))
sizes_10$sumdepth <- rowSums(sizes_10[c(1:42),c(6:70)])
sizes_10$sumdepth68 <- rowSums(sizes_10[c(1:42),c(6:68)])
sizes_10$sumdepth52 <- rowSums(sizes_10[c(1:42),c(6:67)])
sizes_10$sumdepth34 <- rowSums(sizes_10[c(1:42),c(6:64)])
sizes_10$sumdepth2 <- rowSums(sizes_10[c(1:42),c(6:44)])
sizes_10$sumdepth69 <- rowSums(sizes_10[c(1:42),c(45:69)])
sizes_10$sumdepth_sand <- rowSums(sizes_10[c(1:42),c(70:105)])



sizes3<-sizes_10%>%
        rename("Identifier"="Sample.Name")%>%
        left_join(agedepth2)


sizes4<-sizes3%>%
        select(Depth,median,sumdepth2,sumdepth69,sumdepth_sand)%>%
        gather(Fraction,Percentage,-Depth,-median)


sizes5<-sizes3%>%
        select(Depth,median,sumdepth2,sumdepth69,sumdepth_sand)


fixed<-ggplot(sizes4, aes(x=Depth,y=Percentage,color=Fraction))+ geom_point() + geom_line()

fixed
ggplotly(fixed)

fixed_age<-ggplot(sizes4, aes(x=median,y=Percentage,color=Fraction))+ geom_point() + geom_line()

fixed_age
ggplotly(fixed_age)
##do not know sizes
#sizes4<-filter(sizes3,Identifier!='NA')


#d_70_f<-sizes3%>%
      #  filter(sumdepth34<100)


d50<-ggplot(sizes3, aes(x=Depth,y=d.0.5))+ geom_point() 

d50

d501<-ggplot(sizes3, aes(x=median,y=d.0.5))+ geom_point() 

d501


ggplotly(d501)

d_90<-ggplot(sizes3, aes(x=Depth,y=d.0.9))+ geom_point() 
d_90

d_70<-ggplot(sizes3, aes(x=Depth,y=sumdepth34))+ geom_point() 
d_70
ggplotly(d_70)


d_70_all2<-ggplot(sizes3, aes(x=median,y=sizes3$sumdepth34))+ geom_point()+geom_line() + ylab("% of grains which size is below 35 um")+ xlab("Age (cal kyr BP)")+ theme_bw()+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
d_70_all2


d_2<-ggplot(sizes3, aes(x=median,y=sizes3$sumdepth2))+ geom_point()+geom_line() + ylab("% of grains which size is below 35 um")+ xlab("Age (cal kyr BP)")+ theme_bw()+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
d_2

d_69<-ggplot(sizes3, aes(x=median,y=sizes3$sumdepth69))+ geom_point()+geom_line() + ylab("% of grains which size is below 35 um")+ xlab("Age (cal kyr BP)")+ theme_bw()+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
d_69

d_sand<-ggplot(sizes3, aes(x=median,y=sizes3$sumdepth_sand))+ geom_point()+geom_line() + ylab("% of grains which size is below 35 um")+ xlab("Age (cal kyr BP)")+ theme_bw()+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
d_sand

#d_70_all<-ggplot(sizes3, aes(x=median,y=sqrt(sizes3$sumdepth34)))+ geom_point() 
#d_70_all

#d_70_all<-ggplot(sizes3, aes(x=median,y=log(sizes3$sumdepth34)))+ geom_point() 
#d_70_all


old3<-old2%>%
        select(-(2:25))%>%
        janitor::row_to_names(1)

colnames(old3)[1] <- "Sample.Name"
colnames(old3)[2] <- "d.0.1"       
#colnames(old3)[3] <- "d.0.5"
#colnames(old3)[4] <- "d.0.9"
#colnames(sizes) <- as.character(unlist(sizes[1,]))
#sizes = sizes[-1, ]


#old3$Sample.Name <- gsub("- Average", "", old3$Sample.Name)


#sizes2<-old3%>%
#       select((1:4))
#sizes3<-colSums(sizes,6:73)

old4<-old3%>%
        filter(Sample.Name!=150,Sample.Name!=57,Sample.Name!=54)%>%
        


old3$Sample.Name<- as.numeric(as.character(old3$Sample.Name))
old3$sumdepth <- rowSums(old3[c(1:22),c(6:70)])
old3$sumdepth68 <- rowSums(old3[c(1:22),c(6:68)])
old3$sumdepth52 <- rowSums(old3[c(1:22),c(6:67)])
old3$sumdepth34 <- rowSums(old3[c(1:22),c(6:64)])
old3$sumdepth2 <- rowSums(old3[c(1:22),c(6:44)])
old3$sumdepth69 <- rowSums(old3[c(1:22),c(45:69)])
old3$sumdepth_sand <- rowSums(old3[c(1:22),c(70:105)])



old4<-old3%>%
         filter(Sample.Name!=150,Sample.Name!=57,Sample.Name!=54)

       



        left_join(agedepth2)





library(plotly)
ggplotly(d_70_all2)

dss<-sizes2%>%
        gather(Size,Percentage,-Sample.Name)%>%
        filter(Percentage!=1173.484)%>%
        rename("Identifier"="Sample.Name")
dss$Identifier<- as.numeric(as.character(dss$Identifier))
dss<- left_join(dss,agedepth2)

ds<-ggplot(dss, aes(x=Depth,y=Percentage,color=Size))+ geom_point() +ylab("Size")
                                                                                                                                           
ds
ggplotly(ds)
#####AGES

#ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_4/SAN8_2019_4_35_ages.txt",skip=1)

#colnames(ages5)<-c("Depth","max","min","median","mean")

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)

#agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))

grain_age<-left_join(sand_silt_clay3,agedepth2)
grain_age2<-filter(grain_age,mean!="NA")
grain_age2$median <- as.numeric(grain_age2$median)

percentages<-ggplot(grain_age2, aes(x=median,y=Percentage,color=Fraction))+ geom_point() +geom_line()
percentages2<-ggplot(grain_age2, aes(x=Depth,y=Percentage,color=Fraction))+ geom_point() +geom_line()
percentages3<-ggplot(grain_age2, aes(x=Percentage,y=median,color=Fraction))+ geom_point() +geom_line()+scale_y_reverse()


percentages

percentages3
ggplotly(percentages2)

#check<-grain_age2%>%
 #       group_by(Identifier)%>%
  #      mutate(Total=sum(Percentage))
        

#library(ghibli)
##problem with area graph and switching axis????


#graphpp<-ggplot(grain_age2, aes(x=median,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=12),
#                                                                                                                                           axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 5000))
#print(graphpp)
#str(grain_age2)
#str(sand_silt_clay3)

#graphpp2<-ggplot(grain_age2, aes(x=median,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=18),
 #                                                                                                                                                                                    axis.title=element_text(size=18,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 5000))

#graphpp2


#graphpp22<-ggplot(grain_age2, aes(x=Depth,y=Percentage,fill=Fraction))+ theme_bw()+ theme(panel.border = element_blank()) +geom_area() +  scale_fill_ghibli_d("MarnieMedium1")+ theme(axis.text=element_text(size=18),
  #                                                                                                                                                                                    axis.title=element_text(size=18,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 175, by = 10))

#graphpp22


graphpp333<-ggplot(grain_age2, aes(x=median,y=Percentage,fill=Fraction))+ geom_area()+ theme_bw()+ theme(panel.border = element_blank()) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                     axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
print(graphpp333)



tog_per<-sand_silt_clay2%>%
        left_join(agedepth2)%>%
        select(median,Clay,Silt,Sand2)

#graph_toge<-ggplot(together, aes(x=median,y=Percentage,fill=Fraction))+ geom_point()+ theme_bw()

clay<-filter(grain_age2,Fraction=="Clay")
graph_clay<-ggplot(clay, aes(x=median,y=Percentage))+ geom_point()+ theme_bw()+ theme(panel.border = element_blank()) + theme(axis.text=element_text(size=12),
                                                                                                                                                 axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous(breaks = seq(0, 32500, by = 2500))
print(graph_clay)


sed<-select(merged.datamass4,median,sedrate.mm,Water,Dry_bulk_density2,Depth)

#together<-san_final_model10%>%
 #       select('median',NorTi,NorAl,NorSi,NorSr,NorRb,NorCa,NorFe)%>%
  #      left_join(sed)%>%
   #     left_join(sizes4)%>%
        #left_join(grain_age2)%>%
    #    select(median,Depth,NorTi,NorSi,NorAl,NorSr,NorRb,NorCa,Water,Dry_bulk_density2,Fraction,Percentage,sedrate.mm)
        


#%>%
 #       left_join(EA2)
sizes33<-sizes3%>%
        select(Identifier,d.0.5)


together<-ages_final_ITRAX2%>%
        merge(san_final_model10,all = TRUE)%>%
        
       # san_final_model10%>%
        select(Depth,'median',NorTi,NorAl,NorSi,NorSr,NorRb,NorCa, NorFe)%>%
        merge(sed,all=TRUE)%>%
        merge(sizes4,all=TRUE)%>%
        merge(con_diat_to_merge,all=TRUE)%>%
        merge(EA2,all = TRUE)%>%
        merge(sizes33,all=TRUE)%>%
        merge(prc_curve_scores2,all=TRUE)%>%
        merge(con_diat_1,all=TRUE)%>%
        mutate(age_k= median/1000, Fe_Ti=NorFe/NorTi,Si_Ti=NorSi/NorTi,Si_Al=NorSi/NorAl)%>%
        filter(age_k>0.120)
     #   merge(grain_age2,all=TRUE)
        
correlation<-together%>%
        select(5:11,27:28)%>%
        drop_na()

cor2<-cor(correlation)
corrplot(cor2,method = "number")

#tog_per has the wrong percentages clay = 7 um

#grid.arrange(graph_clay,TI,nrow=2,ncol=1)

library(cowplot)
library(egg)

plot1 <- together %>%
        select(median, Percentage) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = median, y = Percentage), size = 1, alpha = 0.75) +
        ylab("Clay") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot2 <- together %>%
        select(Depth, NorTi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75)+
        ylab("NorTI") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank())
plot2
plot3 <- together %>%
        select(Depth, NorSi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = NorSi), size = 1, alpha = 0.75) + geom_path(aes(x = Depth, y = NorSi))+
        ylab("NorSi") +
        theme_minimal() +
        theme(axis.title.x = element_blank())

plot4 <- together %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_line(aes(x = median, y = sedrate.mm), size = 1, alpha = 0.75) +
        ylab("sed.rate") +
        theme_minimal() +
        theme(axis.title.x = element_blank())
ggplotly(plot4)
plot5 <- together %>%
        select(Depth, Water) %>%
        na.omit() %>%
        ggplot() +
        geom_line(aes(x = Depth, y = Water), size = 1, alpha = 0.75) +
        ylab("Water") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot6 <- together %>%
        select(Depth, Fraction,Percentage) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = Percentage,color=Fraction), size = 1, alpha = 0.75) + geom_line(aes(x = Depth, y = Percentage,color=Fraction), size = 1, alpha = 0.75)+
        scale_x_continuous(breaks=seq(0, 173, by=25))+ylab("Percentage") +
        theme_minimal()
print(plot6)
plot7 <- together %>%
        select(Depth, NorAl) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = NorAl), size = 1, alpha = 0.75) +
        ylab("NorAl") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


plot8 <- together %>%
        select(Depth, Dry_bulk_density2) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = Dry_bulk_density2), size = 1, alpha = 0.75) + geom_line(aes(x = Depth, y = Dry_bulk_density2), size = 1, alpha = 0.75)+
        ylab("Dry bulk density") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot9 <- together %>%
        select(Depth, NorAl) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = NorAl), size = 1, alpha = 0.75) + geom_path(aes(x = Depth, y = NorAl))+
        ylab("NorAl") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())




cowplot::plot_grid(plot1, plot2, align = "v", ncol = 1, rel_heights = c(0.50, 0.50))
egg::ggarrange(plot1, plot2, heights = c(0.50, 0.50))

cowplot::plot_grid(plot6, plot8,plot5, align = "v", ncol = 1, rel_heights = c(0.33, 0.33,0.33))
egg::ggarrange(plot6, plot7,plot2, plot3,heights = c(0.25, 0.25,0.25,0.25))

b<-cowplot::plot_grid(plot7, plot5, align = "v", ncol = 1, rel_heights = c(0.5,0.5))


########WORKS!
egg::ggarrange(plot8,plot5,plot6,heights = c(0.33,0.33,0.33))
#cowplot::plot_grid(plot5, plot6,plot8, align = "v", ncol = 1, rel_heights = c(0.33,0.33,0.33))


egg::ggarrange(plot9,plot2,plot3,heights = c(0.33,0.33,0.33))


#####with time

plot22 <- together %>%
        select(age_k, NorTi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = NorTi), size = 1, alpha = 0.75)+
        ylab("NorTI") +
        theme_minimal() +
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
ggplotly(plot66)
plot33 <- together %>%
        select(age_k, NorSi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = NorSi), size = 1, alpha = 0.75) +
        ylab("NorSi") +
        theme_minimal() +
        scale_x_continuous(breaks=seq(0, 32.5, by=2))

plot99 <- together %>%
        select(age_k, NorAl) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = NorAl), size = 1, alpha = 0.75) +
        ylab("NorAl") +
        theme_minimal() + 
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot66 <- together %>%
        select(age_k, Fraction,Percentage) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = Percentage,color=Fraction), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = Percentage,color=Fraction), size = 1, alpha = 0.75)+
        ylab("Percentage (%)") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))

plot66
plot11 <- together %>%
        select(age_k, NorFe) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = NorFe), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = NorFe), size = 1, alpha = 0.75)+
        ylab("NorFe") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot111 <- together %>%
        select(age_k, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = sedrate.mm), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = sedrate.mm), size = 1, alpha = 0.75)+
        ylab("Sed rate mm/yr)") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot1111 <- together %>%
        select(age_k, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = sedrate.mm), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = sedrate.mm), size = 1, alpha = 0.75)+
        ylab("Sed rate mm/yr)") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))
       # theme(axis.title.x = element_blank(),
        #      axis.text.x = element_blank())
ggplotly(plot1111)

plot_N <- together %>%
        select(age_k, averaged.N) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = averaged.N), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = averaged.N), size = 1, alpha = 0.75)+
        ylab("% N") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot_dN <- together %>%
        select(age_k, averaged.d15N) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = averaged.d15N), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = averaged.d15N), size = 1, alpha = 0.75)+
        ylab("d15N") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot_feti <- together %>%
        select(age_k, Fe_Ti) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = log(Fe_Ti)), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = log(Fe_Ti)), size = 1, alpha = 0.75)+
        ylab("FeTi ratio") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
ggplotly(plot_22)
plot_50 <- together %>%
        select(age_k, d.0.5) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = d.0.5), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = d.0.5), size = 1, alpha = 0.75)+
        ylab("Median grain size (um)") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot_d13c <- together %>%
        select(age_k, averaged.d13C) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = averaged.d13C), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = averaged.d13C), size = 1, alpha = 0.75)+
        ylab("d13C") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot_CN <- together %>%
        select(age_k, averaged.C.N) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = averaged.C.N), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = averaged.C.N), size = 1, alpha = 0.75)+
        ylab("C:N") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot_SiTi <- together %>%
        select(age_k, Si_Ti) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = Si_Ti), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = Si_Ti), size = 1, alpha = 0.75)+
        ylab("Si/Ti") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())


plot_SiAl <- together %>%
        select(age_k, Si_Al) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = Si_Al), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = Si_Al), size = 1, alpha = 0.75)+
        ylab("Si/Al") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot_C <- together %>%
        select(age_k, averaged.C) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = averaged.C), size = 1, alpha = 0.75) + geom_line(aes(x = age_k, y = averaged.C), size = 1, alpha = 0.75)+
        ylab("%C") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))
        #theme(axis.title.x = element_blank(),
            #  axis.text.x = element_blank())


plot_prc <- together %>%
        select(age_k, PrC) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = PrC), size = 1, alpha = 0.75) + geom_path(aes(x = age_k, y = PrC))+
        ylab("PrC") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))


plot_conc <- together %>%
        select(age_k, d_g_wet_sed) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = age_k, y = sqrt(d_g_wet_sed))) + geom_line(aes(x = age_k, y = sqrt(d_g_wet_sed)))+
        ylab("Diatom conc") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32.5, by=2))
#theme(axis.title.x = element_blank(),
 # axis.text.x = element_blank())

plot_conc

ggplotly(plot22)


plot_ink <- together %>%
        select(age_k, averaged.C) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = averaged.C, y = age_k)) + geom_line(aes(x = averaged.C, y = age_k))+
        ylab("Age (cal ka)") +
        theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
        scale_y_reverse(breaks=seq(0, 32.5, by=2))

plot_ink

plot_ink2 <- together %>%
        select(Depth, age_k) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = age_k)) + geom_line(aes(x = Depth, y = age_k))+
        xlab("Depth (cm)") +
        ylab("Age (cal ka)")+
        theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_y_reverse(breaks=seq(0, 32.5, by=2))+
        scale_x_continuous(breaks=seq(0, 172.5, by=10))

plot_ink2
#grid.arrange(plot_SiTi,plot_C,nrow=2,ncol=1)

################### almost final plots

cowplot::plot_grid(plot_d13c,  plot_CN,plot_dN,plot_C,align = "v", ncol = 1, rel_heights = c(0.25, 0.25,0.25,0.25))


cowplot::plot_grid(plot_feti,plot33,plot22,plot11,plot_prc,align = "v", ncol = 1, rel_heights = c(0.20, 0.20,0.20,0.20,0.20))


cowplot::plot_grid(plot111,plot_50,plot_conc,align = "v", ncol = 1, rel_heights = c(0.33, 0.33,0.33))

#percentages of sand,clay,slit

#####################




mod_d13C <- gam(averaged.d13C ~ s(median,k=15), data = together, method = "REML")

plot(mod_d13C)


mod_d15N <- gam(averaged.d15N ~ s(median,k=15), data = together, method = "REML")

plot(mod_d15N)
#elements

a<-egg::ggarrange(plot11,plot99,plot22,plot33,heights = c(0.25,0.25,0.25,0.25))



##sed rate + grain size
egg::ggarrange(plot111,plot66,heights = c(0.5,0.5))


egg::ggarrange(plot_50,plot66,heights = c(0.5,0.5))
#cowplot::plot_grid(plot_50, plot66, align = "v", ncol = 1, rel_heights = c(0.50, 0.50))

ggplotly(plot33)

plot2 <- together %>%
        select(Depth, NorTi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Depth, y = NorTi), size = 1, alpha = 0.75) + 
        ylab("NorTI") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
ggplotly(plot33)


###Not many values!!!!

together2<-together%>%
        select(3:11,14)%>%
        merge(sizes5,all=TRUE)%>%
        merge(EA2,all=TRUE)%>%
        merge(prc_curve_scores2)%>%drop_na()%>%
        #unique()%>%
        select(-Depth,-median,-Identifier,-mean,-max,-min)

together3_depth<-ages_final_ITRAX2%>%
        merge(san_final_model10,all = TRUE)%>%
        merge(prc_curve_scores2,all=TRUE)%>%
        filter(NorAl>0,NorSr>0,Depth>42)%>%
        mutate(Fe_Ti=NorFe/NorTi,Si_Ti=NorSi/NorTi,Si_Al=NorSi/NorAl,Rb_Sr=NorRb/NorSr)

together3<-ages_final_ITRAX2%>%
        merge(san_final_model10,all = TRUE)%>%
        merge(prc_curve_scores2,all=TRUE)%>%
        filter(NorAl>0,NorSr>0)%>%
        mutate(Fe_Ti=NorFe/NorTi,Si_Ti=NorSi/NorTi,Si_Al=NorSi/NorAl,Rb_Sr=NorRb/NorSr)



together33<-together3%>%
        select(median,NorTi,NorAl,NorSi,NorSr,NorRb,NorCa, NorFe,PrC,Fe_Ti,Si_Ti,Si_Al,Rb_Sr)%>%
        
        drop_na()

together333<-together3%>%
        select(NorTi,NorAl,NorSi,NorSr,NorRb,NorCa, NorFe,PrC,Fe_Ti,Si_Ti,Si_Al,Rb_Sr)%>%
        
        drop_na()

prcurve_ITRAX333<-prcurve(together333,method='ca',trace = TRUE,plotit = TRUE,vary = TRUE,penalty=1.4,smoother = smoothGAM)

varExpl(prcurve_ITRAX333)
# Extract pricipal curve scores
scrs<-scores(prcurve_ITRAX333)
# Plot curve vs. Age
plot(scrs~together33$median, type="l")




together4<-ages_final_ITRAX2%>%
        merge(san_final_model10,all = TRUE)%>%
        merge(prc_curve_scores2,all=TRUE)%>%
        merge(sizes5,all = TRUE)%>%
        select(NorTi,NorAl,NorSi,NorSr,NorRb,NorCa, NorFe,NorK, PrC,sumdepth_sand,sumdepth69,sumdepth2)%>%
        rename("Rb"="NorRb","Si"="NorSi","Ti"="NorTi","Ca"="NorCa","K"="NorK","Al"="NorAl","Fe"="NorFe","Sr"="NorSr","Sand"="sumdepth_sand","Silt"="sumdepth69","Clay"="sumdepth2")%>%
        drop_na()
        # san_final_model10%>%
        #select(Depth,'median',NorTi,NorAl,NorSi,NorSr,NorRb,NorCa, NorFe)%>%
        
m<-cor(together2)
library(corrplot)
corrplot(m, method = "number")
str(together2)

m2<-cor(together333)
library(corrplot)
corrplot(m2, method = "number")

m4<-cor(together4)
library(corrplot)
corrplot(m4, method = "number")

plot(together3$median~together3$Rb_Sr)
plot(together4$NorTi~together4$sumdepth2)

#################CCHAP 4 graphs with just depth

plot_w <- together %>%
        select(Depth, Water) %>%
        na.omit() %>%
        filter(Water>0, Water<60)%>%
        ggplot() +
        geom_point(aes(x = Water, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Water, y = Depth), size = 1, alpha = 0.75)+
        xlab("Water (%)") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
ggplotly(plot_w)
plot_bd <- together %>%
        select(Depth, Dry_bulk_density2) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75)+
        xlab("Bulk density (g/cm3)") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_C <- together %>%
        select(Depth, averaged.C) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75)+
        xlab("Carbon (%)") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_05 <- together %>%
        select(Depth, d.0.5) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75)+
        xlab("Median grain size (um)") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
ggplotly(plot_05)
plot_05

cowplot::plot_grid(plot_w, plot_bd, plot_C, align = "h", nrow = 1, rel_heights = c(0.33,0.33,0.33))

cowplot::plot_grid(plot_w, plot_bd, plot_C, plot_05,align = "h", nrow = 1, rel_heights = c(0.25,0.25,0.25,0.25))


plot_frac <- together %>%
        select(Depth, Fraction,Percentage) %>%
        na.omit() %>%
        unique()%>%
        ggplot() +
        geom_point(aes(x = Percentage , y = Depth, color=Fraction))+ geom_path(aes(x = Percentage, y = Depth, color=Fraction))+
        xlab("Percentage (%)") +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme_minimal()+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(), legend.position="none")+ annotate("text",x=60,y=20,label="Silt",size=8,color="blue")+
        annotate("text",x=40,y=100,label="Clay",size=8,color="green")+
        annotate("text",x=35,y=160,label="Sand",size=6,color="red")
plot_frac
graphpp333<-ggplot(plot_frac, aes(x=Percentage,y=Depth,fill=Fraction))+ geom_area()+ theme_bw()+ theme(panel.border = element_blank()) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous()


graphpp333   
aa<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/other/table1.txt",header=TRUE,sep = ",")


###Fig2


plot_N <- together %>%
        select(Depth, averaged.N)%>% 
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = averaged.N, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.N, y = Depth), size = 1, alpha = 0.75)+
        xlab("Nitrogen (%)") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())


plot_d13C <- together %>%
        select(Depth, averaged.d13C) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = averaged.d13C, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d13C, y = Depth), size = 1, alpha = 0.75)+
        xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)"))) +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
#expression(paste(delta^{13}, C[VPDB],"(???)")))

"\u03B4 ^13^ C[VPDB]\u2030"
plot_d15N <- together %>%
        select(Depth, averaged.d15N) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = averaged.d15N, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d15N, y = Depth), size = 1, alpha = 0.75)+
        xlab(expression(paste(delta^{15}, N[VPDB],"(\u2030)")))  +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())



plot_sed <- together %>%
        select(Depth, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = Depth), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_05 <- together %>%
        select(Depth, d.0.5) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75)+
        xlab("Median grain size (um)") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())


plot_conc_2 <- together %>%
        select(Depth, d_g_wet_sed) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(y = Depth, x = sqrt(d_g_wet_sed))) + geom_path(aes(y = Depth, x = sqrt(d_g_wet_sed)))+
        ylab("Diatom conc") +
        theme_minimal()+
        scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc_2

cowplot::plot_grid(plot_sed, plot_C, plot_N, plot_d13C,plot_d15N, align = "h", nrow = 1, rel_heights = c(0.2,0.2,0.2,0.2,0.2))

grid.arrange(plot_sed, plot_C, plot_d13C,ncol=3,nrow=1)

grid.arrange(plot_d15N, plot_N, ncol=2,nrow=1)

grid.arrange(plot_d15N, plot_N, plot_05,ncol=3,nrow=1)
             
#grid.arrange(plot_sed, plot_C, plot_d13C,plot_05, ncol=4,nrow=1)


###Fig3


plot_Ti <- together %>%
        select(Depth, NorTi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75)+
        xlab("Ti") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_Si <- together %>%
        select(Depth, NorSi) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = NorSi, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorSi, y = Depth), size = 1, alpha = 0.75)+
        xlab("Si") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_Fe <- together %>%
        select(Depth, NorFe) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = NorFe, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorFe, y = Depth), size = 1, alpha = 0.75)+
        xlab("Fe") +
        theme_minimal() +
        scale_y_reverse()+
        scale_x_continuous(breaks = seq(0, 1, by = 0.2))+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_FeTi <- together %>%
        select(Depth, Fe_Ti) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Fe_Ti, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Fe_Ti, y = Depth), size = 1, alpha = 0.75)+
        xlab("Fe:Ti ratio") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
plot_SiTi <- together %>%
        select(Depth, Si_Ti) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = Si_Ti, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Si_Ti, y = Depth), size = 1, alpha = 0.75)+
        xlab("Si:Ti ratio") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_prc <- together %>%
        select(Depth, PrC) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = PrC, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = PrC, y = Depth), size = 1, alpha = 0.75)+
        xlab("Principal curve") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_Al <- together %>%
        select(Depth, NorAl) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = NorAl, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorAl, y = Depth), size = 1, alpha = 0.75)+
        xlab("Al") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())



grid.arrange(plot_Ti, plot_Si, plot_Fe, plot_FeTi, plot_SiTi, plot_prc,ncol=6,nrow=1)


grid.arrange(plot_Ti, plot_Si, plot_Fe, plot_FeTi, plot_SiTi, plot_prc, plot_Al,ncol=7,nrow=1)


grid.arrange(plot_Ti, plot_Si, plot_Fe,ncol=3,nrow=1)

plot_C
