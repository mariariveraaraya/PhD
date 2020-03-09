
library(here)
library(tidyverse)
library(dbplyr)
library(gridExtra)
library(plotly)


source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')


##MAR calculation

bulk <- read.csv(here("experiments","exp_EA","data","bulk density 04 11 18 master cleaned.csv"))


bulk2<-bulk

bulk2<-bulk2[,-2]
bulk2<-bulk2[-(1:10),-2]

bulk2<-mutate(bulk2,Length.of.piece..cm.= ifelse(Comments=='small piece' & is.na(Length.of.piece..cm.), (0.5), Length.of.piece..cm.))

bulk2$Length.of.piece..cm.[is.na(bulk2$Length.of.piece..cm.)] <- 1

bulk2$Aluminium.paper[is.na(bulk2$Aluminium.paper)] <- 1.2

bulk2$Bag..g.[is.na(bulk2$Bag..g.)] <- 5.0

#select just variables of interest

bulk3<-select(bulk2,Identifier,Bag..g.,Wet.sediment.bag.g.,Dry.sediment..g..bag,Length.of.piece..cm.,aluminium.,Bag.aluminium.paper,Aluminium.paper)


#problem with missing volumes, because there is no info of length of piece, maybe assign 0.5 to some of them....
#First convert length of piece to numeric
bulk3$Length.of.piece..cm. <- as.numeric(as.character(bulk3$Length.of.piece..cm.))
#All NA or empty values to 0.5 (be careful with this, not all of them would be 0.5)
bulk3$Length.of.piece..cm.[is.na(bulk3$Length.of.piece..cm.)] <- 0.5

#Calculate volume of tube

bulk4=mutate (bulk3, Volume=(3.14*(3*3)*(Length.of.piece..cm.)/2))

#Calculate grams of dry sediment per sample (if the sample was collected with aluminium paper it needed to be substracted)

bulk5<-mutate(bulk4,Totaldrysediment= ifelse(aluminium. =="y", (Dry.sediment..g..bag-Bag..g.-Aluminium.paper), (Dry.sediment..g..bag-Bag..g.)))

#Calculation of bulk density ######Values needed to be corrected - no missing bc need to be  used for PYC calcs

bulk5<-mutate (bulk5, Dry_bulk_density=((Totaldrysediment/Volume)))
bulk5$Identifier <- as.numeric(as.character(bulk5$Identifier))


#Manual addition of values measured after the Excel spreadsheet

newdensity<-mutate(bulk5,Dry_bulk_density2=ifelse(Identifier==105, (0.818133333), Dry_bulk_density))
newdensity1<-mutate(newdensity,Dry_bulk_density2=ifelse(Identifier==85, (0.910633333), Dry_bulk_density2))
newdensity2<-mutate(newdensity1,Dry_bulk_density2=ifelse(Identifier==176, (0.908916667), Dry_bulk_density2))
newdensity3<-mutate(newdensity2,Dry_bulk_density2=ifelse(Identifier==123, (0.90255), Dry_bulk_density2))
newdensity4<-mutate(newdensity3,Dry_bulk_density2=ifelse(Identifier==76, (0.997916667), Dry_bulk_density2))
newdensity5<-mutate(newdensity4,Dry_bulk_density2=ifelse(Identifier==75, (1.02505), Dry_bulk_density2))
newdensity6<-mutate(newdensity5,Dry_bulk_density2=ifelse(Identifier==113, (0.984333333), Dry_bulk_density2))
newdensity7<-mutate(newdensity6,Dry_bulk_density2=ifelse(Identifier==96, (0.973), Dry_bulk_density2))
newdensity7<-mutate(newdensity6,Dry_bulk_density2=ifelse(Identifier==99, (0.973), Dry_bulk_density2))
newdensity8<-mutate(newdensity7,Dry_bulk_density2=ifelse(Identifier==186, (0.954666667), Dry_bulk_density2))
newdensity9<-mutate(newdensity8,Dry_bulk_density2=ifelse(Identifier==121, (0.913066667), Dry_bulk_density2))
newdensity10<-mutate(newdensity9,Dry_bulk_density2=ifelse(Identifier==107, (0.915833333), Dry_bulk_density2))
newdensity11<-mutate(newdensity10,Dry_bulk_density2=ifelse(Identifier==171, (0.929092), Dry_bulk_density2))
newdensity12<-mutate(newdensity11,Dry_bulk_density2=ifelse(Identifier==117, (0.9500), Dry_bulk_density2))
newdensity13<-mutate(newdensity12,Dry_bulk_density2=ifelse(Identifier==126, (0.9200), Dry_bulk_density2))
newdensity14<-mutate(newdensity13,Dry_bulk_density2=ifelse(Identifier==170, (0.9600), Dry_bulk_density2))
newdensity15<-mutate(newdensity14,Dry_bulk_density2=ifelse(Identifier==146, (0.9400), Dry_bulk_density2))
newdensity16<-mutate(newdensity15,Dry_bulk_density2=ifelse(Identifier==27, (0.02), Dry_bulk_density2))
newdensity17<-mutate(newdensity16,Dry_bulk_density2=ifelse(Identifier==30.5 | Identifier==30,(0.15), Dry_bulk_density2))

#newdensity18<-mutate(newdensity17,Dry_bulk_density2=ifelse(Dry_bulk_density2>1.1, (0.95), Dry_bulk_density2))
newdensity18<-mutate(newdensity17,Dry_bulk_density2=ifelse(Dry_bulk_density2>1.1, (0.99), Dry_bulk_density2))
newdensity19<-mutate(newdensity18,Dry_bulk_density2=ifelse(Identifier==62, (0.0882), Dry_bulk_density2))
newdensity20<-mutate(newdensity19,Dry_bulk_density2=ifelse(Identifier==67, (0.1077), Dry_bulk_density2))
newdensity21<-mutate(newdensity20,Dry_bulk_density2=ifelse(Identifier==58, (0.1563), Dry_bulk_density2))
newdensity22<-mutate(newdensity21,Dry_bulk_density2=ifelse(Identifier==54, (0.1785), Dry_bulk_density2))
newdensity23<-mutate(newdensity22,Dry_bulk_density2=ifelse(Identifier==63, (0.1101), Dry_bulk_density2))
newdensity24<-mutate(newdensity23,Dry_bulk_density2=ifelse(Identifier==61, (0.1021), Dry_bulk_density2))
newdensity25<-mutate(newdensity24,Dry_bulk_density2=ifelse(Identifier==38, (0.1640), Dry_bulk_density2))
newdensity26<-mutate(newdensity25,Dry_bulk_density2=ifelse(Identifier==34, (0.1714), Dry_bulk_density2))
newdensity27<-mutate(newdensity26,Dry_bulk_density2=ifelse(Identifier==29, (0.0431*4), Dry_bulk_density2))
newdensity28<-mutate(newdensity27,Dry_bulk_density2=ifelse(Identifier==29 | Identifier==35 | Identifier==39 | Identifier==50 | Identifier==55,(0.11), Dry_bulk_density2))
newdensity29<-mutate(newdensity28,Dry_bulk_density2=ifelse(Identifier==32 | Identifier==33 | Identifier==36,(0.13), Dry_bulk_density2))
newdensity30<-mutate(newdensity29,Dry_bulk_density2=ifelse(is.na(Dry_bulk_density2 ), (0.90), Dry_bulk_density2))


bulk6<-mutate(newdensity30,Water=((Wet.sediment.bag.g.-Dry.sediment..g..bag)/Wet.sediment.bag.g.)*100)
bulk7<-bulk6[bulk6$Totaldrysediment>= 0,]
bulk7<-bulk6%>%
        select(Identifier,Dry_bulk_density2,Water)%>%
        filter(Identifier!=26)

#bulk8<-mutate(bulk7,Water=ifelse(Water<0, (NA), Water))
#bulk9<-mutate(bulk8,Water=ifelse(Depth==86, (NA), Water))

bulkgr1<-left_join(bulk7,agedepth2)%>%
        filter(Identifier!=113)%>%
        filter(Water>0)

bulk_gr<-ggplot(bulkgr1, aes(x=Dry_bulk_density2,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(0,1.7))+ ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab(expression(paste(N^{15}, N[VPDB], "(/u2030)")))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

bulk_gr

water_gr<-ggplot(bulkgr1, aes(x=Water,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(0,75))+ ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab("Water content (%)")+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

water_gr



bulk_gr<-ggplot(bulkgr1, aes(x=Depth,y=Dry_bulk_density2))+geom_point() + theme_bw() 

bulk_gr

water_gr<-ggplot(bulkgr1, aes(x=Depth,y=Water))+geom_point() +  theme_bw() 

water_gr

plot4 <- together %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_line(aes(x = median, y = sedrate.mm), size = 1, alpha = 0.75) +
        ylab("sed.rate") +
        theme_minimal() +
        theme(axis.title.x = element_blank())

cowplot::plot_grid(bulk_gr, water_gr,align = "v", ncol = 1, rel_heights = c(0.5,0.5))
egg::ggarrange(bulk_gr,water_gr,heights = c(0.5,0.5))



###NEED AGES!

#agedepth<-read.csv(here("agedepth6.csv"),na.strings=c("NA","#DIV/0!",""))

#agedepth<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/agedepth6.csv')

#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))

#merge csv with bulk density with txt file with ages
#merged.datamass <- left_join(bulk7, agedepth)#######original has bulk5!!!!!!!!

merged.datamass <- right_join(bulk7, agedepth2)#######original has bulk5!!!!!!!!


#calculate age between samples, but it returns an error if run just like below
#merged.datamass2<-mutate(merged.datamass,yearbetsamples=mean[-1] - mean[-length(mean)])

#include an ifelse with the value of mean age in the first row, but IT NEEDS TO BE CHANGED accordingly

#merged.datamass2<-mutate(merged.datamass,yearbetsamples=ifelse(mean==4181, (999), (mean[-1] - mean[-length(mean)])))

merged.datamass2<-mutate(merged.datamass,yearbetsamples=ifelse(mean==242, (999), (mean[-1] - mean[-length(mean)])))

#Calculate sed.rate
merged.datamass3<-mutate(merged.datamass2,sedrate=(1/yearbetsamples))
merged.datamass3<-mutate(merged.datamass3,sedrate.mm=(1/yearbetsamples)*10)


#Calculate Mass acc rate
merged.datamass4<-mutate(merged.datamass3,MAR=(Dry_bulk_density2*sedrate)*1000) #so it is in mg
merged.datamass4<-mutate(merged.datamass4,sed.rate.inv=(1/sedrate))


#plot(merged.datamass4$sed.rate.inv~merged.datamass4$mean)
#plot(merged.datamass4$sedrate.mm~merged.datamass4$mean)
sed_rate<-ggplot(merged.datamass4, aes(x=median,y=sedrate.mm))+geom_line() +  scale_x_continuous(breaks = seq(0, 32500, by = 2500)) + scale_y_continuous(breaks = seq(0,4, by = 0.1))+ ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Sedimentation rate (mm/yr)")+theme_bw() + xlab("Age (cal kyr BP)")+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


sed_rate

MAR<-ggplot(merged.datamass4, aes(x=median,y=MAR))+geom_point() +  scale_x_continuous(breaks = seq(0, 32500, by = 2500)) + ggtitle("")+ labs(x= expression(bold(delta^13)),y = "MAR (g cm-2 yr-1)")+theme_bw() + xlab("Age (cal kyr BP)")+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

ggplotly(MAR)

#ggplotly(sed_rate)

####End of mass accumulation rate calculations
#setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/writeup")
