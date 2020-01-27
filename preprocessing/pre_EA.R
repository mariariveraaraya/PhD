
####### this has EA and MAR, but for MAR the final ages are needed!!!

## ---- load-02

library(here)
library(tidyverse)
library(dbplyr)
library(gridExtra)

#############################156 graph
#library("dplyr")
#library("tidyr")
#library("tidylog", warn.conflicts = FALSE)

## ---- fire-map

##EA preprocessing
#setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC")

firstrun<-read.csv(here("experiments", "exp_EA","data", "EA.180614.csv"),na.strings=c("NA","#DIV/0!",""))

secondrun<-read.csv(here("experiments", "exp_EA","data", "Copy of Maria 180831 reduction.csv"),na.strings=c("NA","#DIV/0!",""))

thirdrun<-read.csv(here("experiments", "exp_EA","data","Copy of Maria reduction 22.05.18.csv"),na.strings=c("NA","#DIV/0!",""))

#Has N values
fourthrun<-read.csv(here("experiments", "exp_EA","data","Copy of Maria reduction 180910.csv"),na.strings=c("NA","#DIV/0!",""))


colnames(fourthrun)[colnames(fourthrun)=="?13C...VPDB."] <- "X.13C...VPDB."
colnames(fourthrun)[colnames(fourthrun)=="?15......R."] <- "X.15......R."

colnames(fourthrun)[colnames(fourthrun)=="ë13C...VPDB."] <- "X.13C...VPDB."
colnames(fourthrun)[colnames(fourthrun)=="ë15......R."] <- "X.15......R."
fourthrun
#Has N values
fifthrun<-read.csv(here("experiments", "exp_EA","data","Copy of Maria reduction 180913.csv"),na.strings=c("NA","#DIV/0!",""))

#rename(fifthrun, X.13C...VPDB.= X.13C..?VPDB.)

#rename(fifthrun, id2=Identifier.2)

colnames(fifthrun)[colnames(fifthrun)=="X.13C..?VPDB."] <- "X.13C...VPDB."
colnames(fifthrun)[colnames(fifthrun)=="X.15...?..R."] <- "X.15......R."


colnames(fifthrun)[colnames(fifthrun)=="X.13C..äVPDB."] <- "X.13C...VPDB."
colnames(fifthrun)[colnames(fifthrun)=="X.15...ä..R."] <- "X.15......R."

##Has N values
sixthrun<-read.csv(here("experiments", "exp_EA","data","Copy of Maria reduction 180914.csv"),na.strings=c("NA","#DIV/0!",""))


colnames(sixthrun)[colnames(sixthrun)=="X.13C..?VPDB."] <- "X.13C...VPDB."
colnames(sixthrun)[colnames(sixthrun)=="X.15...?..R."] <- "X.15......R."

colnames(sixthrun)[colnames(sixthrun)=="X.13C..äVPDB."] <- "X.13C...VPDB."
colnames(sixthrun)[colnames(sixthrun)=="X.15...ä..R."] <- "X.15......R."

seven<-read.csv(here("experiments", "exp_EA","data","Copy of Maria_15.05.18_EA.csv"),na.strings=c("NA","#DIV/0!",""))

eight<-read.csv(here("experiments", "exp_EA","data","EA_14_10_19.csv"),na.strings=c("NA","#DIV/0!",""))
colnames(eight)[colnames(eight)=="Î.13C..â..VPDB."] <- "X.13C...VPDB."
colnames(eight)[colnames(eight)=="Î.15Î...â..Î.Î.R."] <- "X.15......R."
colnames(eight)[colnames(eight)=="ï..Line"] <- "Line"


eight2<-filter(eight,'Identifier.2' !='posthypy')


#setdiff(seven, eight2)
#HERE you add the next spreadsheets (raw)
#str(eight2)
#str(seven)
#str(sixthrun)
#to combine all the spreadsheets use rbind (because all the spreadsheets have the same vatiables, if not use merge)
rawcombined<-rbind(firstrun,secondrun,thirdrun,fourthrun,fifthrun,sixthrun,seven,eight2)
#try<-filter(rawcombined,Identifier==32)
#rawcombinedjoin<-full_join(firstrun,secondrun)

#maybe merge all original files by identifier and then do next section

colnames(rawcombined)[colnames(rawcombined)=="X.13C...VPDB."] <- "d13C"
colnames(rawcombined)[colnames(rawcombined)=="X.15......R."] <- "d15N"
colnames(rawcombined)[colnames(rawcombined)=="Identifier.1"] <- "Identifier"


rawcombined.1<-select(rawcombined,Identifier,Ampl..44,d13C,d15N,X.C.1,X.N.1,C.N)

rawsubsetted<-subset(rawcombined.1,Identifier!="LOC"& Identifier!="HOC"& Identifier!="Flush"& Identifier!="Blank"& Identifier!="Taipan"& Identifier!="177_off"& Identifier!="Sorghum")
write.csv(rawsubsetted,file="rawsubsetted.csv")

rawsubsetted.1<-subset(rawsubsetted,Ampl..44>3000 & Ampl..44<30000)
rawsubsetted.1$Identifier<- as.numeric(as.character(rawsubsetted.1$Identifier))

mean_by_Paper2 <- rawsubsetted.1 %>%
        group_by(Identifier) %>%
        summarize(averaged.d13C = mean(d13C),averaged.C.N=mean(C.N), averaged.d15N=mean(d15N),averaged.C=mean(X.C.1),averaged.N=mean(X.N.1))


##Last ages

ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_4/SAN8_2019_4_35_ages.txt",skip=1)

colnames(ages5)<-c("Depth","max","min","median","mean")

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))


#str(agedepth)

mean_by_Paper2
EA2<-left_join(mean_by_Paper2,agedepth)


#EA<-left_join(mean_by_Paper2,correctdepths)

maria2<-ggplot(EA2, aes(y=averaged.C,x=mean))+geom_point()+ ggtitle("")+ylab("%Carbon")+xlab("Identifier")+geom_point()+ theme_bw() +
        theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))

maria2

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+xlab("\u03B4^13")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(x= expression("\u03B4"^13), y="Age")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab(expression(paste(delta^{13}, C[VPDB], "(\u2030)")))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


#print(p10)

#p1 <- ggplot(merged.ages, aes(x=d13C,y=mean))+geom_point() +  scale_y_reverse() + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")

#p1 <- ggplot(merged.ages, aes(x=mean,y=d13C))+geom_point() + scale_y_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")



maria4 <- ggplot(EA2, aes(x=averaged.C,y=mean))+geom_point()+  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) +scale_x_continuous(breaks = seq(0, 50, by = 10)) +xlab("% C")+ylab("Calibrated date BP")+ggtitle("")+theme_bw()+ theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                                                      axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))



maria4_depth <- ggplot(EA2, aes(x=averaged.C,y=Depth))+geom_point()+  scale_y_reverse(breaks = seq(0, 175, by = 10)) +scale_x_continuous(breaks = seq(0, 50, by = 10)) +xlab("% C")+ylab("Depth (cm)")+ggtitle("")+theme_bw()+ theme(
                                                                                                                                                                                                                                          axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

#print(p2)
#grid.arrange(p10,p2,ncol=2)

###to delete y axis not neccesary here (maybe)

maria5<-ggplot(EA2, aes(x=averaged.C.N,y=mean))+geom_point()+ scale_x_continuous(limits=c(0, 40), breaks=seq(0,40,by=10)) + ggtitle("") +xlab("C/N ratio")+ylab("cal yr BP")+theme_bw()+ scale_y_reverse(breaks = seq(0, 32500, by = 2500))+theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                                                              axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))
length(EA2$Identifier)
EA2_t<-EA2%>%
        select(Depth,Identifier,averaged.C,averaged.C.N,averaged.d13C)%>%
        round(digits=3)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-sub)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "hold_position")

#EA2_t

#pyc_gr<-Hypy.selected5%>%
        #select(Identifier,PyCxMAR)


#EA_pyC<-left_join(EA2,pyc_gr)


#PyCxMAR.graph<-ggplot(EA_pyC,aes(x=PyCxMAR,y=median))+geom_point() + xlab(expression("PyC MAR"(mu~g~mm^{-2}~yr^{-1})))+ ylab("Calibrated date (BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) + scale_x_continuous(breaks=seq(0,0.6,by=0.2))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=9,face="bold"),axis.title.y=element_blank(),
                                                                                                                                                                                                                                                                                   #axis.text.y=element_blank())


#print(p3)
########Most recent

grid.arrange(maria3,maria4,maria5,ncol=3,nrow=1)

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
bulk7<-select(bulk6,Identifier,Dry_bulk_density2,Water)

#bulk8<-mutate(bulk7,Water=ifelse(Water<0, (NA), Water))
#bulk9<-mutate(bulk8,Water=ifelse(Depth==86, (NA), Water))


###NEED AGES!

agedepth<-read.csv(here("agedepth6.csv"),na.strings=c("NA","#DIV/0!",""))

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))

#merge csv with bulk density with txt file with ages
merged.datamass <- left_join(bulk7, agedepth)#######original has bulk5!!!!!!!!

#calculate age between samples, but it returns an error if run just like below
#merged.datamass2<-mutate(merged.datamass,yearbetsamples=mean[-1] - mean[-length(mean)])

#include an ifelse with the value of mean age in the first row, but IT NEEDS TO BE CHANGED accordingly

merged.datamass2<-mutate(merged.datamass,yearbetsamples=ifelse(mean==4181, (999), (mean[-1] - mean[-length(mean)])))

#Calculate sed.rate
merged.datamass3<-mutate(merged.datamass2,sedrate=(1/yearbetsamples))
merged.datamass3<-mutate(merged.datamass3,sedrate.mm=(1/yearbetsamples)*100)


#Calculate Mass acc rate
merged.datamass4<-mutate(merged.datamass3,MAR=(Dry_bulk_density2*sedrate)*1000) #so it is in mg


####End of mass accumulation rate calculations
#setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/writeup")
