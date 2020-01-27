## ---- load-pkg
library(here)
library(dplyr)
library(purrr)
library(data.table)
library(tidyr)
library(ggplot2)
library(magick)
library(kableExtra)

## ---- tb-one-pre
#dates_table<-read.csv(here("experiments", "exp_radiocarbon","data", "Compiled_Aug_19.csv"),na.strings=c("NA","#DIV/0!",""))
dates_table<-read.csv('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/experiments/exp_radiocarbon/data/Compiled_Aug_19.csv')

dates_table$ID<- gsub("SAN", "", dates_table$ID)

dates_table$OZCode<- gsub(" ", "", dates_table$OZCode,fixed = TRUE)

colnames(dates_table)[colnames(dates_table)=="ID"] <- "Identifier"

dates_table$Identifier<- as.numeric(as.character(dates_table$Identifier))

dates_table<-dates_table[-15,]

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")


correctdepths<-correctdepths %>% rename(Depth=Real.depth)

correctdepths$Identifier<- as.numeric(as.character(correctdepths$Identifier))

correctdepths<-filter(correctdepths, Depth < 14 | Depth > 15.5)


setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/experiments/exp_radiocarbon/data')


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

all_first_table<-data522%>%
        left_join(dates_table)%>%
        left_join(correctdepths)%>%
        select(OZCode,Depth,Conv_RC,Conv_RC_error,to_95,from_95,Carbon_fraction,Pretreatment)
        


all_first_table2<-all_first_table%>%
        unite("Conventional radiocarbon dates",3:4, sep=" \u00B1 ")%>%
        unite("Calibrated age range (95 %)",4:5,sep=" \u002D ")%>%
        rename("Laboratory Code"="OZCode", "Carbon fraction"="Carbon_fraction")%>%
        filter("Conventional radiocarbon dates" != "25,450 ± 170")
        


## ---- tb-one

#all_first_table2%>%
 #       arrange(`Depth`)%>%
  #      knitr::kable(booktabs = TRUE, caption = "(ref:tb2-sub)", linesep = "") %>%
   #     kableExtra::kable_styling(position = "center", latex_options= "hold_position")

all_first_table2%>%
        arrange(`Depth`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-one)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")
        #as_image(all_first_table2)
#setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD-thesis-VC/writeup')
        
#all_first_table3<-all_first_table2%>%
 #               arrange(`Depth`)%>%
  #              knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-one)", linesep = "")

## ---- tb- two

all<-data522%>%
        left_join(dates_table)%>%
        mutate(Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(dates_table$Carbon_fraction)))%>%
        left_join(correctdepths,by="Identifier")%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction)    

## ---- tb-three
all<-data522%>%
        left_join(dates_table)


all11<-mutate(all,Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(all$Carbon_fraction)))


all2<-all11%>%
        left_join(correctdepths,by="Identifier")%>%
        mutate(mean_2=((from_95)+ (to_95))/2)%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction,mean_2)
young_old<-all2%>%
        group_by(Depth)%>%
        summarize(max_depth= max(mean_2),min_depth=min(mean_2))%>%
        mutate(diff=max_depth-min_depth)%>%
        filter(diff!=0)
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
        unique()%>%
        arrange(`Depth`)%>%
        rename("Depth (cm)"= "Depth", "Minimum age"="min_depth.x", "Carbon fraction (min)"="Carbon_fraction.x","Maximum age"="max_depth.x","Carbon fraction (max)"="Carbon_fraction.y","Offset"="diff","Number of dates"="length_Car.x")

cbbPalette <- c("#000000", "#2A00E5", "#56B4E9", "#1FC910", "#69D3A4", "#C91025", "#D55E00", "#CC79A7")

#scale_colour_manual(values=cbbPalette)

g12<-ggplot(all2,aes(x=Depth,y=to_95,ymin= to_95,ymax=from_95,colour=Carbon_fraction))+geom_point(size=5)+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(3,6,12,43,67,76,82,90,105,114,137,146,162))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()+xlab("Depth (cm)")+ scale_colour_manual(values=cbbPalette)

g12+ guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))



## ---- tb-four
#ff3%>%
 #       arrange(`Depth (cm)`)%>%
  #      knitr::kable(booktabs = TRUE, caption = "(ref:tb4-sub)", linesep = "") %>%
   #     kableExtra::kable_styling(position = "center", latex_options= "hold_position")

ff3%>%
        arrange(`Depth (cm)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-four)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")


##---- radiocarbon-gr1

print(g12)