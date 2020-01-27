##Hypy and others paper
##Compile_Aug_19 is main file, merge with replacement depths file to include depths and then use in Bacon/Oxcal
#Merge Table 1 Chronology with Compile_Aug_19 to get final tables???? not anymore
#calibrated dates with Bacon or Oxcal

####W Complete table then it can be filtered
library(here)
library(dplyr)

## ---- all

library(here)
library(dplyr)
library(purrr)
library(data.table)

dates_table<-read.csv(here("experiments", "exp_radiocarbon","data", "Compiled_Aug_19.csv"),na.strings=c("NA","#DIV/0!",""))


dates_table$ID<- gsub("SAN", "", dates_table$ID)

dates_table$OZCode<- gsub(" ", "", dates_table$OZCode,fixed = TRUE)

colnames(dates_table)[colnames(dates_table)=="ID"] <- "Identifier"

dates_table$Identifier<- as.numeric(as.character(dates_table$Identifier))


correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

correctdepths$Identifier<- as.numeric(as.character(correctdepths$Identifier))

model_ages_charcoal<-dates_table%>%
        left_join(correctdepths)%>%
        select(OZCode,Identifier,Conv_RC,Conv_RC_error,Depth,Carbon_fraction)%>%
        filter(Carbon_fraction=='Charcoal >63 um')

model_ages_pollen<-dates_table%>%
        left_join(correctdepths)%>%
        select(OZCode,Identifier,Conv_RC,Conv_RC_error,Depth,Carbon_fraction)%>%
        filter(Carbon_fraction=='Pollen')

model_ages_bulk<-dates_table%>%
        left_join(correctdepths)%>%
        select(OZCode,Identifier,Conv_RC,Conv_RC_error,Depth,Carbon_fraction)%>%
        filter(Carbon_fraction=='Bulk organics')

model_ages_cellulose<-dates_table%>%
        left_join(correctdepths)%>%
        select(OZCode,Identifier,Conv_RC,Conv_RC_error,Depth,Carbon_fraction)%>%
        filter(Carbon_fraction=='Cellulose')



compare_org<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/Bulk_organics_17_12_19.txt",skip=2,fill=TRUE)


compare_org2<-compare_org%>%
        plyr::rename(c("V1"="OZCode","V2"="from_67","V3"="to_67","V4"="from_95","V5"="to_95"))

compare_org2<-compare_org2[1:7,]


      




setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/experiments/exp_radiocarbon/data')

##This calls the files and makes a list with them....for some reason Cr tube file 1 do have same column names
#Pick the elements that you are interested in first!

#files <- (Sys.glob("*.csv"))

#filenames <- list.files("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/ITRAX/Raw files", pattern="*.csv", full.names=TRUE)
#ldf <- map_df(filenames, read.csv, stringsAsFactors = FALSE)

#str(temp)
temp = list.files(pattern="*.txt")

myfun <- function(x) {
        read.table(x,skip = 2,fill=TRUE)
}

myfiles = lapply(temp, myfun)

#data55 <- rbindlist( myfiles, fill = TRUE )



org<-as.data.frame(myfiles[[1]])
org<-org[1:7,]

cellulose<-as.data.frame(myfiles[[2]])
cellulose<-cellulose[1:1,]

hypy<-as.data.frame(myfiles[[3]])
hypy<-hypy[1:13,]

pollen<-as.data.frame(myfiles[[4]])
pollen<-pollen[1:2,]

charcoal<-as.data.frame(myfiles[[5]])
charcoal<-charcoal[1:4,]

data522<- rbind(hypy,pollen,charcoal,org,cellulose)


data522<-data522%>%
        plyr::rename(c("V1"="OZCode","V2"="from_67","V3"="to_67","V4"="from_95","V5"="to_95"))

#str(data522)
#str(dates_table)
#all<-data522%>%
 #       left_join(dates_table)%>%
  #      left_join(correctdepths)%>%
   #     select(OZCode,Depth,from_95,to_95,Carbon_fraction)


str(all)
all<-data522%>%
        left_join(dates_table)


all11<-mutate(all,Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(all$Carbon_fraction)))
              
             
              

all2<-all11%>%
        left_join(correctdepths,by="Identifier")%>%
        mutate(mean_2=((from_95)+ (to_95))/2)%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction,mean_2)



library(ggplot2)
g5<-ggplot(all2)+ geom_errorbar(data = all2, aes(x=Depth,ymin = to_95, ymax = from_95, color=Carbon_fraction,size=0.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous()+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()


print(g5)

#g6<-g5+ theme(  legend.key.size = unit(1.5, "cm") +legend.key.size
#g6

g10<-ggplot(all2,aes(x=Depth,y=mean_2,colour=Carbon_fraction))+geom_point()

str(all2)

g11<-g10 + geom_errorbar(aes(ymin = to_95, ymax = from_95,size=2))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=round(all2$Depth))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()+xlab("Depth (cm)")

g11

cbbPalette <- c("#000000", "#2A00E5", "#56B4E9", "#1FC910", "#69D3A4", "#C91025", "#D55E00", "#CC79A7")

#scale_colour_manual(values=cbbPalette)

g12<-ggplot(all2,aes(x=Depth,y=to_95,ymin= to_95,ymax=from_95,colour=Carbon_fraction))+geom_point(size=5)+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(3,6,12,43,67,76,82,90,105,114,137,146,162))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()+xlab("Depth (cm)")+ scale_colour_manual(values=cbbPalette)

g12+ guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))

#all2$Depth



#library(plotly)

#g12<-ggplotly(g11)

#g12

##############For model
#out: 
for_model<-all2%>%
        filter(OZCode!="OZX132",OZCode!="OZY758",OZCode!="OZY423",OZCode!="OZX768",OZCode
               !="OZX769",OZCode!="OZX766",OZCode!="OZX767",OZCode!="OZY132")%>%
        distinct()

    
g103<-ggplot(for_model,aes(x=Depth,y=mean_2))+geom_point()

#library(plotly)
 
#ggplotly(g103)

 #       filter(Carbon_fraction!="Bulk organics"& Depth > 5)%>%
  #      filter(Carbon_fraction!="Cellulose")

#all2$OZCode
 #       filter(Carbon_fraction!="Pollen"& Depth !=146)%>%
  #      filter(Carbon_fraction!="Charcoal >63 um"& Depth  !=82)

#?round
#qplot(x    = Depth ,
 #     y = mean_2,
  #    data = all2,colour=Carbon_fraction) +
        
   #     geom_errorbar(aes(
    #            ymin  = to_95,
     #           ymax  = from_95,
      #          width = 1, size=0.5))



#Pick the elements that you are interested in first! 
#clean<-cr1[-1,]%>%
 #       select(2,3, 9,12:14,17:20,40:41)

#names(clean) <- as.matrix(clean[1, ])
#clean <- clean[-1, ]
#clean[] <- lapply(clean, function(x) type.convert(as.character(x)))

#clean<- cbind(clean, "Depth.mm"=1:nrow(clean)) 

#cleanb<-filter(clean,kcps>30000)

#colnames(compare_org)[colnames(charcoal.dates)=="V1"] <- "name"
#colnames(compare_org)[colnames(charcoal.dates)=="V2"] <- "from_67"
#colnames(compare_org)[colnames(charcoal.dates)=="V3"] <- "to_67"
#colnames(compare_org)[colnames(charcoal.dates)=="V4"] <- "from_95_4"
#colnames(charcoal.dates)[colnames(charcoal.dates)=="V5"] <- "to_95_4"





compare_hypy<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANHypy101U22.csv")

compare_pollen<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/Pollen.csv")


#merge with agedepth file to get calibrated ages, also order by depth/fraction

#dates_cal<-read.csv(here("agedepth6.csv"),na.strings=c("NA","#DIV/0!",""))

#dates_cal$Identifier<- as.numeric(as.character(dates_cal$Identifier))



#dates_merge<-left_join(dates_table,dates_cal,by="Identifier")


#dates_merge%>%
 #       arrange(Depth)%>%
  #      knitr::kable(booktabs = TRUE, caption = "(ref:tb-sub)", linesep = "") %>%
   #     kableExtra::kable_styling(position = "center", latex_options= "hold_position")

###More in analysis?
#Possible structure
#-	Figure 1 – location of lake - img
#-	Table 1 – Dates info/table - table
#-	Figure 2 – Lithology / organic content - img
#-	Figure 3 – dot plot of dates from diff fractions - graph

#####################youngest/oldest
within <- function(x){
        max(x)-min(x)
}

#######incorrect
young_old<-all2%>%
        group_by(Depth)%>%
        summarize(max_depth= max(mean_2),min_depth=min(mean_2))%>%
        mutate(diff=max_depth-min_depth)%>%
        filter(diff!=0)
###Correct
young_old_corr<-all2%>%
        group_by(Depth)%>%
        unique()%>%
        summarize(max_depth= max(from_95),min_depth=min(to_95),length_Car=n())%>%
        mutate(diff=max_depth-min_depth)%>%
        filter(diff!=0)%>%
        merge(young_old,by="Depth")%>%
        select(1:5)

youn_old2_corr<-young_old_corr%>%
        left_join(all2)%>%
        select(Depth,min_depth.x,Carbon_fraction,to_95,length_Car)%>%
        filter(to_95==min_depth.x)%>%
        mutate(Fraction='Min')


##Incorrect
youn_old2<-young_old%>%
        left_join(all2)%>%
        select(Depth,min_depth,mean_2,Carbon_fraction)%>%
        filter(mean_2==min_depth)%>%
        mutate(Fraction='Min')

#youn_old22<-young_old%>%
 #       left_join(all2)%>%
  #      select(Depth,max_depth,mean_2,Carbon_fraction,lenghts)%>%
   #     filter(mean_2==max_depth)%>%
    #    mutate(Fraction='Max')


youn_old22_corr<-young_old_corr%>%
        left_join(all2)%>%
        select(Depth,max_depth.x,mean_2,Carbon_fraction,from_95,length_Car)%>%
        filter(from_95==max_depth.x)%>%
        mutate(Fraction='Max')

#ff<-merge(youn_old2,youn_old22,by="Depth")

fff<-merge(youn_old2_corr,youn_old22_corr,by="Depth")

ff2<-fff%>%
        select(Depth,min_depth.x,Carbon_fraction.x,max_depth.x,Carbon_fraction.y,length_Car.x)


ff3<-ff2%>%
        merge(young_old,by="Depth")%>%
        select(Depth,min_depth.x,Carbon_fraction.x,max_depth.x,Carbon_fraction.y,diff,length_Car.x)%>%
        unique()


per_Car<-all2%>%
        group_by(Carbon_fraction)%>%
        unique()%>%
        summarize(length_Car=n())%>%
        rename("Carbon fraction" = "Carbon_fraction","Number of samples" = length_Car)
per_Car%>%
        arrange(desc(`Number of samples`))%>%
        knitr::kable(booktabs = TRUE, caption = "(ref:tb2-sub)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "hold_position")
#setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/writeup')
#radiocarbon `r per_Car[which(per_Car$"Carbon fraction"=="SPAC"),2]`
young_old_corr_depth<-all2%>%
        group_by(Depth)%>%
        unique()
