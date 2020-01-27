
library(tidyverse)

#Hypy section

#replacement_depths_itrax_2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_2.csv")
#In the first part of the analysis, the percentage of pyrogenic carbon in the sample is calculated.
#the second part involves the correction of de d13C values. Emma/Jordahna provided a script.

#Hypy rawreadxl#This is posthypy
#will need to merge with prehypy?
Hypy.post<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/Ea runs/Copy of Maria 180831 reduction(19306).csv',na.strings=c("NA","#DIV/0!",""))


Hypy.post1<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/Ea runs/190314.PostHypy.csv',na.strings=c("NA","#DIV/0!",""))


colnames(Hypy.post1)[colnames(Hypy.post1)=="Row"] <- "Line"
colnames(Hypy.post1)[colnames(Hypy.post1)=="rArea.Flash.TCD"] <- "Area.44"



last<-read.csv(here("experiments", "exp_EA","data","EA_14_10_19.csv"),na.strings=c("NA","#DIV/0!",""))

last22<-filter(last,Identifier.2!='prehypy')
colnames(last22)[colnames(last22)=="Î.13C..â..VPDB."] <- "X.13C...VPDB."
colnames(last22)[colnames(last22)=="Î.15Î...â..Î.Î.R."] <- "X.15......R."
colnames(last22)[colnames(last22)=="ï..Line"] <- "Line"


merged.Hypypost<-rbind(Hypy.post,Hypy.post1,last22)


#rest of sample weights are in Hy py 17.08.18 (already included in cals 04.04)

#HERE you add the next spreadsheets (raw)


#to combine all the spreadsheets use rbind (because all the spreadsheets have the same vatiables, if not use merge)
#Hypyrawcombined<-rbind(firstrun,secondrun,thirdrun,fourthrun,fifthrun,sixthrun)

#maybe merge all original files by identifier and then do next section

#setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/")


colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.13C...VPDB."] <- "d13CpostHypy"
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.15......R."] <- "d15N"
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="Identifier.1"] <- "Identifier"
##need to change the column anme clarifying this is post Hypy
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.C.1"] <- "X.C.1postHypy"





##select column of interest
Hypyrawcombined.1<-select(merged.Hypypost,Identifier,Ampl..44,d13CpostHypy,d15N,X.C.1postHypy,X.N.1,C.N)


Hypyrawcombined.1<-subset(Hypyrawcombined.1,Identifier!="LOC"& Identifier!="HOC"& Identifier!="Flush"& Identifier!="Blank"& Identifier!="Taipan"& Identifier!="177_off"& Identifier!="Sorghum")

Hypyrawcombined.1$Identifier<- as.numeric(as.character(Hypyrawcombined.1$Identifier))


#Hypyrawcombined.2<-merge(Hypyrawcombined.1,agedepth,by="Identifier") ###Losing some samples bc agedepth do not include them!: 32, 98,150,156,157,158,159,160

#colnames(Hypyrawcombined.2)[colnames(Hypyrawcombined.2)=="Real.depth"] <- "Depth"

#dim(Hypyrawcombined.2)






#This has pre and post merged
#prepluspost<-merge(Hypyrawcombined.1,rawsubsetted,by="Identifier")
#prepluspost2<-merge(Hypyrawcombined.1,rawsubsetted.1,by="Identifier",all=TRUE)

rawsubsetted<-read.csv(here("rawsubsetted.csv"),na.strings=c("NA","#DIV/0!",""))


prepluspost3<-merge(Hypyrawcombined.1,rawsubsetted,by="Identifier")


merge_both <- prepluspost3%>% 
        select(Identifier,X.C.1postHypy,X.C.1,d13C,d13CpostHypy)%>%
        group_by(Identifier) %>% 
        summarize(av_d13C = mean(d13C),av_C=mean(X.C.1),av_Cpost=mean(X.C.1postHypy),av_d13Cpost=mean(d13CpostHypy))

#prepluspost.subsetted<-subset(prepluspost,Identifier!="LOC"& Identifier!="HOC"& Identifier!="Flush"& Identifier!="Blank"& Identifier!="Taipan"& Identifier!="177_off"& Identifier!="Sorghum")

#rawsubsetted.1<-subset(rawsubsetted,Ampl..44>1000 & Ampl..44<30000)

#now we have a data frame with pre and post info, now we use the calculations
dim(prepluspost)
dim(prepluspost.subsetted)

raw = read_excel('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/Copy of HypyCalcs_Rainy_modified_04.04.xlsx')

raw2 <- raw[c(1:70),c(1:9)]



#raw3<-merge(raw2,prepluspost,by="Identifier")
raw3<-merge(raw2,merge_both,by="Identifier",all=TRUE)
#raw3<-subset(raw2[1:51,1:9])
#raw3 <- raw3[c(1:76),c(1:9,26,32)]
#raw3 <- raw[c(1:76),c(1:9,26,32)]


##fine?

colnames(raw3)[colnames(raw3)=="Sample Weight (g)"] <- "Sample.weight"
colnames(raw3)[colnames(raw3)=="Catalyst (g)"] <- "Catalyst"
colnames(raw3)[colnames(raw3)=="Pre HyPy Sample Weight (g)"] <- "Pre.Sample.Weight" 
colnames(raw3)[colnames(raw3)=="Post HyPy Sample Weight (g)"] <- "Post.Sample.Weight" 
#raw10<-subset(raw3[-12,])
#raw11<-subset(raw10[-28,])

####Need to put avoid all the NAs!



raw3<-mutate(raw3,PreHypy.X.C=((Sample.weight/100)*av_C/(Sample.weight+Catalyst))*100)
raw3<-mutate(raw3,C.pre.mg=((PreHypy.X.C/100)*Pre.Sample.Weight)*1000)
raw3<-mutate(raw3,C.post.mg=((av_Cpost/100)*Post.Sample.Weight)*1000)
raw3<-mutate(raw3,Ratio.BCTOC=(C.post.mg/C.pre.mg)*100)
raw3<-mutate(raw3,BlackCarbon.Perc=av_C*(Ratio.BCTOC/100))

raw3<-mutate(raw3,Corrected=(((BlackCarbon.Perc/av_C)/1.02)-0.004)*av_C)


raw3$Identifier<-as.numeric(raw3$Identifier)
agedepth$Identifier<-as.numeric(agedepth$Identifier)

#blackcarbon<-ggplot(raw3, aes(x=Corrected,y=Identifier))+geom_point(size=2.5) + theme_bw() + theme(axis.text=element_text(size=12),
#axis.title=element_text(size=12,face="bold"))

#print(blackcarbon)

#str(raw3)

#dim(raw3)
#str(raw3)

###with age

#merge with age-depth model
#ages.april.hypy<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_2/SAN8_2019_2_31_ages.txt",skip=1)

#colnames(ages.april.hypy)<-c("Depth","max","min","median","mean")

#merge csv with ea with txt file with ages
merged.hypy.ages <- left_join(raw3,agedepth)

#colnames(merged.hypy.ages)[colnames(merged.hypy.ages)=="Real.depth"] <- "Depth"

#merged.hypy.ages2<-merge(merged.hypy.ages,ages.april.hypy, by="Depth")

#merged.hypy.ages2<- merged.hypy.ages2 %>% rename(median.x=median)

#value34<-subset(merged.hypy.ages2,Identifier=="34")

#blackcarbon.age2<-ggplot(merged.hypy.ages, aes(x=Corrected,y=median))+geom_point(size=2.5) + theme_bw() + theme(axis.text=element_text(size=12),
#axis.title=element_text(size=12,face="bold"))

#print(blackcarbon.age2)


#blackcarbon.age3<-ggplot(merged.hypy.ages, aes(x=Corrected,y=median))+ xlab("% pyrogenic carbon")+ ylab("Calibrated date (BP)")+geom_point()+ geom_path()+ theme_bw()  +scale_y_reverse(breaks=seq(0, 32000, by=2500))+ theme(axis.text=element_text(size=12),
#axis.title=element_text(size=12,face="bold"))

#print(blackcarbon.age3)

#######

#ages.hypy<-subset(merged.datamass4, select=c(Identifier,median))

#merged.data.Hypy <- merge(ages.hypy, raw3, by="Identifier")
#dim(merged.data.Hypy)

#blackcarbon.age<-ggplot(merged.data.Hypy, aes(x=Corrected,y=median))+geom_point(size=2.5) + theme_bw() + theme(axis.text=element_text(size=12),
#axis.title=element_text(size=12,face="bold"))

#print(blackcarbon.age)

#Need to merge with actual EA spreadsheets to include %C and D13C before and after Hypy


###Correction for isotopic values/taken from other file

tbl.corre.d13c2<-merged.hypy.ages%>%
        select(av_C,av_d13C,av_Cpost,av_d13Cpost,Identifier)%>%
        filter(Identifier!=103,Identifier!=120,Identifier!=104)

#colnames(Hypyrawcombined.2)[colnames(Hypyrawcombined.2)=="Real.depth"] <- "Depth"



#tbl.correc.d13C<-select(tbl.corre.d13c2,averaged.C,averaged.d13C,X.C.1postHypy,d13CpostHypy,Depth,Identifier.x)

colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_C"] <- "CT"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_d13C"] <- "dT"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_Cpost"] <- "CR"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_d13Cpost"] <- "dR"


dat<-tbl.corre.d13c2

#dat<-dat[-39,]

# Total carbon in sample (%)
C_T <- dat$CT

# Error in total carbon measurement (%)
s.e.C_T <- 0.02*C_T

# d13C of C_T (per mil)
d_T <- dat$dT
dat.length <- length(d_T) # length of data (number of rows)

# Error in d13C of C_T (taken as constant at 1.0 per mil)
s.e.d_T <- rep(1.0,dat.length)

# Carbon in residue (%)
C_R <- dat$CR

# Error in residual carbon measurement (%)
s.e.C_R <-0.02*C_R

# d13C of C_R (per mil)
d_R <- dat$dR

# Error in d13C of d_R (taken as constant at 0.1 per mil)
s.e.d_R <- rep(0.1,dat.length)

Identifier<-dat$Identifier

# Bind new variables into one dataset
dat.bind <- cbind(C_T, s.e.C_T, d_T, s.e.d_T, C_R, s.e.C_R, d_R, s.e.d_R)

# Create blank vectors for quantile data created in loop later
lc.result <- vector("numeric", dat.length)
med.result <- vector("numeric", dat.length)
uc.result <- vector("numeric", dat.length)

# loop through each measurement, simulating 10000 values 
# assuming a normal distribution for C_T, d_T, C_R and d_R.
# phi (percentage of labile carbon remaining in residue) is simulated using a previously fit beta distribution (fig S1 above)

for (i in 1:dat.length){
        C_T.norm <- rnorm(10000,dat.bind[i,1], dat.bind[i,2])
        d_T.norm <- rnorm(10000,dat.bind[i,3], dat.bind[i,4])
        C_R.norm <- rnorm(10000,dat.bind[i,5], dat.bind[i,6])
        d_R.norm <- rnorm(10000,dat.bind[i,7], dat.bind[i,8])
        phi <- (rbeta(10000,1.18776,3.08183))*2.0
        # equation (1)in main text; phi converted to fraction
        d_P <- (C_R.norm*d_R.norm- phi/100*d_T.norm*C_T.norm)/(C_R.norm - phi/100*C_T.norm)
        # take quantiles and record in previously created blank vectors
        lc <- quantile(d_P, probs = 0.16, names = FALSE)
        med <- quantile(d_P, probs = 0.5, names = FALSE)
        uc<- quantile(d_P, probs = 0.84, names = FALSE)
        lc.result[i] <- lc
        med.result[i] <- med
        uc.result[i] <- uc}

# Combine quantiles into single data frame and print
stats <- cbind(lc.result,med.result,uc.result)
stats2<-cbind(stats,tbl.corre.d13c2)
stats3<-merge(stats2,merged.hypy.ages)
stats4<-stats3[,1:26]
#print(stats2)

# Save data frame (quantiles)
#write.csv(stats, file='/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/d13corr2.csv',row.names = F) 


###after correction (refer to file D13C-correction-SAN)
#d13cHypycorr<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/d13corr.csv',na.strings=c("NA","#DIV/0!",""))

#d13cHypycorr2<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/d13corr2.csv',na.strings=c("NA","#DIV/0!",""))


#merged.hypy.ages3<-merged.hypy.ages2[-39,]

#d13cHypycorr2<-merge(tbl.corre.d13c2,d13cHypycorr2,by="row.names")

#d13cHypycorr2<-mutate(d13cHypycorr2,veg= ((med.result + 17.4125)/ -2.8805))

#d13cHypycorr2<-mutate(d13cHypycorr2, ratio=(exp(veg)))





#graph.ratio<-ggplot(d13cHypycorr2, aes(x=ratio,y=median))+geom_point(size=2)+ xlab("Total Arboreal:Poaceae")+ ylab("Calibrated date (BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) + theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

#print(graph.ratio)

#graph.corrected<-ggplot(d13cHypycorr2, aes(x=med.result,y=median))+geom_point(size=2) + xlab("Residue d13C")+ ylab("Calibrated date (BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) + theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

#print(graph.corrected)

#dim(d13cHypycorr2)


#Hypy processed

#Hypy<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/Copy of HyPyCalcs_Rainy_modified.csv',na.strings=c("NA","#DIV/0!",""),skip=1)


#Hypy.selected<-select(Hypy,Identifier,Corrected..Wurster.et.al..2012.)
#Hypy.selected2<-merge(merged.datamass4,raw3,by="Identifier")
merged.datamass4$Identifier<- as.numeric(as.character(merged.datamass4$Identifier))
merged.hypy.ages$Identifier<- as.numeric(as.character(merged.hypy.ages$Identifier))


Hypy.selected4<-left_join(stats4, merged.datamass4,by='Identifier')
Hypy.selected4<-mutate(Hypy.selected4,PyCxMAR=((MAR*(Corrected)/100)/100)*1000) #ug/mm2yr
Hypy.selected4<-subset(Hypy.selected4,Identifier!=177)


Hypy.selected5<-Hypy.selected4%>%
        
        group_by(Identifier) %>%
        slice(1)

dim(Hypy.selected5)
Hypy.selecna<-Hypy.selected5%>%
        filter(is.na(PyCxMAR))
#Hypy.selected3<-merge(merged.datamass4,Hypy,by="Identifier")
#merged.datamass4 is defined later in a chunk, it has the bulk density, MAR, sed rate. Need to add more points to the original database
#Hypy.selected2<-mutate(Hypy.selected2,PyCxMAR=(MAR*(Corrected)/100)*1000)
#Hypy.selected2<-subset(Hypy.selected2,Identifier!=177)

#plot(Hypy.selected2$Corrected,Hypy.selected2$mean)

#PyCxMAR.graph<-ggplot(Hypy.selected4,aes(x=PyCxMAR,y=mean.x))+geom_point(size=2) + xlab("Pyc Mass accumulation rate")+ ylab("Calibrated date (BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) + scale_x_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

#summary(Hypy.selected4$PyCxMAR)


#high_pyc<-subset(Hypy.selected4,PyCxMAR > 2)

#print(PyCxMAR.graph)

#merged.data.Hypy <- merge(replacement_depths_itrax, Hypy_for_R_Dec_18, by="Identifier")


#plot(merged.data.Hypy$`Real depth`,merged.data.Hypy$`Corrected (Wurster et al. 2012)`,ylim=c(0,3))
#axis(side=1,at=seq(20,45,70))
#plot(merged.data.Hypy$`Real depth`,merged.data.Hypy$`d13c pre Hypy`)
#lot(merged.data.Hypy$`Real depth`,merged.data.Hypy$`d13 post hyoy`)

####Together

#Age in x

PyCxMAR.graph<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,0.6,by=0.2))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

PyCxMAR.graph2<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))


plot(PyCxMAR.graph2)

pydepth<-ggplot(Hypy.selected5,aes(x=PyCxMAR,y=median))+geom_point() + xlab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ ylab("Depth")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 180, by=10)) + scale_x_continuous(breaks=seq(0,0.6,by=0.2))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

pydepth
#NAs<-filter(Hypy.selected5,PyCxMAR=="NA")
#NAs


blackcarbon.age3<-ggplot(Hypy.selected5, aes(x=Corrected,y=median))+ xlab("% PyC")+ ylab("Calibrated date (BP)")+geom_point()+ theme_bw()  +scale_y_reverse(breaks=seq(0, 32000, by=2500))+ theme(axis.title.y=element_blank(),
                                                                                                                                                                                                    axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12))
#Age in y

graph.corrected<-ggplot(Hypy.selected5, aes(x=med.result,y=median))+geom_point(size=2) + xlab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ ylab("Age (yr cal BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) +  scale_x_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

#Age in x
graph.corrected2<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

graph.corrected3<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

plot(graph.corrected3)

grid.arrange(PyCxMAR.graph, blackcarbon.age3, graph.corrected,nrow=1,ncol=3)
grid.arrange(graph.corrected, blackcarbon.age3, ncol=2)


PyCxMAR.graph3<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+ annotate("text",x=16000,y=0.45,label="Fire",size=20,color="red")
graph.corrected4<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+ annotate("text",x=21000,y=-23,label="Vegetation",size=20,color="green4")

PyCxMAR.graph3
graph.corrected4

##log%PYC vs %C

Cvslog<-ggplot(Hypy.selected5, aes(x=log(av_C),y=log(Corrected)))+geom_point(size=2) + ylab(expression(paste("log % PyC")))+ xlab("% C")+ theme_bw() + theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))
Cvslog
