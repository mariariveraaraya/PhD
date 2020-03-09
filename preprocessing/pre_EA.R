
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

source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')


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

#ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_4/SAN8_2019_4_35_ages.txt",skip=1)

#colnames(ages5)<-c("Depth","max","min","median","mean")

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)

#agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))


#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)%>%
#        filter(Depth <14 | Depth>21)



#ages_final<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
#colnames(ages_final)<-c("Depth","max","min","median","mean")


#agedepth<-merge(correctdepths,ages_final,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))


#agedepth2<-mutate(agedepth,Identifier=ifelse(Depth==47, (75), Identifier))
#agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))


#str(agedepth)

mean_by_Paper2
EA2<-left_join(mean_by_Paper2,agedepth2)


#EA<-left_join(mean_by_Paper2,correctdepths)

maria2<-ggplot(EA2, aes(y=averaged.C,x=mean))+geom_point()+ ggtitle("")+ylab("%Carbon")+xlab("Identifier")+geom_point()+ theme_bw() +
        theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))

maria2

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+xlab("/u03B4^13")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(x= expression("/u03B4"^13), y="Age")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab(expression(paste(delta^{13}, C[VPDB], "(/u2030)")))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


#print(p10)

#p1 <- ggplot(merged.ages, aes(x=d13C,y=mean))+geom_point() +  scale_y_reverse() + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")

#p1 <- ggplot(merged.ages, aes(x=mean,y=d13C))+geom_point() + scale_y_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")



maria4 <- ggplot(EA2, aes(x=averaged.C,y=mean))+geom_point()+  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) +scale_x_continuous(breaks = seq(0, 50, by = 10)) +xlab("% C")+ylab("Calibrated date BP")+ggtitle("")+theme_bw()+ theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                                                      axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

ggplotly(maria4)

maria4_depth <- ggplot(EA2, aes(x=averaged.C,y=Depth))+geom_point()+  scale_y_reverse(breaks = seq(0, 175, by = 10)) +scale_x_continuous(breaks = seq(0, 50, by = 10)) +xlab("% C")+ylab("Depth (cm)")+ggtitle("")+theme_bw()+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

mod_C <- gam(averaged.C ~ s(mean,k=15), data = EA2, method = "REML")

plot(mod_C)
                                                                                                                                                                                                                                                         
maria4_depth
ggplotly(maria4_depth)

maria4_depth_N <- ggplot(EA2, aes(x=averaged.N,y=Depth))+geom_point()+  scale_y_reverse(breaks = seq(0, 175, by = 10)) +xlab("% C")+ylab("Depth (cm)")+ggtitle("")+theme_bw()+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))
        


ggplotly(maria4_depth_N)


#print(p2)
#grid.arrange(p10,p2,ncol=2)

###to delete y axis not neccesary here (maybe)

maria5<-ggplot(EA2, aes(x=averaged.C.N,y=mean))+geom_point()+ scale_x_continuous(limits=c(0, 40), breaks=seq(0,40,by=10)) + ggtitle("") +xlab("C/N ratio")+ylab("cal yr BP")+theme_bw()+ scale_y_reverse(breaks = seq(0, 32500, by = 2500))+theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                                                            axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))
maria5

ggplotly(maria5)

maria_d13c<-ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point()+ scale_x_continuous(limits=c(-29, -18)) + ggtitle("") +xlab("C/N ratio")+ylab("cal yr BP")+theme_bw()+ scale_y_reverse(breaks = seq(0, 32500, by = 2500))+theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                                                  axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))
ggplotly(maria_d13c)

maria6<-ggplot(EA2, aes(x=averaged.d15N,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(0, 10))+ ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab(expression(paste(N^{15}, N[VPDB], "(/u2030)")))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


maria6
ggplotly(maria6)

maria7<-ggplot(EA2, aes(x=averaged.N,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(0, 2))+ ggtitle("")+ labs(x= expression(bold(delta^13)),y = "Age (cal yr BP)")+theme_bw() + xlab(expression(paste(N^{15}, N[VPDB], "(/u2030)")))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))
maria7
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
ggplotly(maria4)

#print(p3)
########Most recent

grid.arrange(maria3,maria4,maria5,ncol=3,nrow=1)

##MAR calculation

#pre_MAR

maria55<-ggplot(EA2, aes(x=averaged.C.N,y=mean))+geom_point()+ scale_x_continuous(limits=c(0, 40), breaks=seq(0,40,by=10)) + ggtitle("") +xlab("C/N ratio")+ylab("cal yr BP")+theme_bw()+ scale_y_reverse(breaks = seq(0, 32500, by = 2000))+theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

maria55                                                                                                                                                                                                                                        
