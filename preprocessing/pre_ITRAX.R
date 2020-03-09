
library(purrr)
library(data.table)
library(tidyverse)

# line 299 graph itrax just with depth maybe stratigraphy chp

#read_chunk('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')

source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)

#ages_final_ITRAX<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
#colnames(ages_final_ITRAX)<-c("Depth","max","min","median","mean")

#colnames(ages_final)<-c("Depth","max","min","median","mean")
#agedepth<-merge(correctdepths,ages_final,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

#ages_final_ITRAX$Identifier<- as.numeric(as.character(agedepth$Identifier))


#agedepth2<-mutate(agedepth,Identifier=ifelse(Depth==47, (75), Identifier))
#agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))
##Analyse ITRAX raw files
#Until now elements were plotted and filtered (low counts and of interest), need to compare and merged with ICP, but they are looking a lot better

setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/ITRAX/Raw files')

##This calls the files and makes a list with them....for some reason Cr tube file 1 do have same column names
#Pick the elements that you are interested in first!

#files <- (Sys.glob("*.csv"))

#filenames <- list.files("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/ITRAX/Raw files", pattern="*.csv", full.names=TRUE)
#ldf <- map_df(filenames, read.csv, stringsAsFactors = FALSE)

#str(temp)
temp = list.files(path="C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/ITRAX/Raw files", pattern="*.csv")
myfiles = lapply(temp, read.csv)

#data5 <- rbindlist( myfiles, fill = TRUE )

#?list.files
##Call each data frame separately (starting with Cr tube)
#Structure: 1.convert to numeric 2. assign depths using row numbers 3. filter counts 4. plot
#this was done per section and not in loop as the first section was different
#TiSinorall uses the four sections, TiSinor 3 for Cr tube
#Monorall uses the four sections, TiSinor 3 for Mo tube
#select_final10 merges both tubes
#san_final_model10 merges with ages and creates csv file


cr1<-as.data.frame(myfiles[[1]])
#Pick the elements that you are interested in first! 
clean<-cr1[-1,]%>%
        select(2,3, 9,12:14,17:20,40:41)

names(clean) <- as.matrix(clean[1, ])
clean <- clean[-1, ]
clean[] <- lapply(clean, function(x) type.convert(as.character(x)))

clean<- cbind(clean, "Depth.mm"=1:nrow(clean)) 

cleanb<-filter(clean,kcps>30000)

gggg<-ggplot(cleanb,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(clean,aes(x=Depth.mm,y='sample surface'))+geom_point()
ggg3<-ggplot(cleanb,aes(x=Depth.mm,y=Ti))+geom_point()

gggg
ggg3
#Alot of variation in Ar counts and kcps for first one

#####3

cr3<-as.data.frame(myfiles[[3]])
clean33<-cr3[-1,]%>%
        select(2,3,9,12:14,17:20,44:45)


names(clean33) <- as.matrix(clean33[1, ])
clean33 <- clean33[-1, ]
clean33[] <- lapply(clean33, function(x) type.convert(as.character(x)))


clean33<- mutate(clean33, "Depth.mm"=(725+1:nrow(clean33))) 
cleancr3<-filter(clean33,kcps>40000 & Ar>100)

gcr3<-ggplot(cleancr3,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleancr3,aes(x=Depth.mm,y='sample surface'))+geom_point()
gcr3<-ggplot(cleancr2,aes(x=Depth.mm,y=Ar))+geom_point()

gcr3
ggg2
gcr3

gcr44<-ggplot(cleancr3,aes(x=Depth.mm,y=Ti))+geom_point()
gcr44

#####2

cr2<-as.data.frame(myfiles[[2]])
clean22<-cr2[-1,]%>%
        select(2,3,9,12:14,17:20,44:45)


names(clean22) <- as.matrix(clean22[1, ])
clean22 <- clean22[-1, ]
clean22[] <- lapply(clean22, function(x) type.convert(as.character(x)))


#clean22<- cbind(clean22, "Depth.mm"=1:nrow(clean22))
clean22<- mutate(clean22, "Depth.mm"=199+(1:nrow(clean22)))

cleancr2<-filter(clean22,kcps>40000 & Ar>100)

gcr2<-ggplot(cleancr2,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleancr2,aes(x=Depth.mm,y='sample surface'))+geom_point()
gcr3<-ggplot(cleancr2,aes(x=Depth.mm,y=Ar))+geom_point()

gcr2
ggg2
gcr3

gcr4<-ggplot(cleancr2,aes(x=Depth.mm,y=Ti))+geom_point()
gcr4

####4
cr4<-as.data.frame(myfiles[[4]])
clean44<-cr4[-1,]%>%
        select(2,3,9,12:14,17:20,44:45)


names(clean44) <- as.matrix(clean44[1, ])
clean44 <- clean44[-1, ]
clean44[] <- lapply(clean44, function(x) type.convert(as.character(x)))


clean44<- mutate(clean44, "Depth.mm"=(1242+1:nrow(clean44))) 
cleancr44<-filter(clean44,kcps>40000 & Ar>100)

gcr44<-ggplot(cleancr44,aes(x=Depth.mm,y=kcps))+geom_point()
ggg44<-ggplot(cleancr44,aes(x=Depth.mm,y='sample surface'))+geom_point()
gcr444<-ggplot(cleancr44,aes(x=Depth.mm,y=Ar))+geom_point()

gcr44
ggg2
gcr444

gcr45<-ggplot(cleancr44,aes(x=Depth.mm,y=Ti))+geom_point()
gcr45


####Merge the  tubes in this case just the last 3


Tiall<-rbind(cleancr3,cleancr2,cleancr44)

plot(Tiall$Depth.mm,scale(Tiall$Ti, center=T,scale=T))
plot(Tiall$Depth.mm,scale(Tiall$Si, center=T,scale=T))


plot(Tiall$Depth.mm,Tiall$si)
plot(Tiall$Depth.mm,Tiall$Ti)


###Normalized them
TI
K2
SI

Tiall$sumdepth <- rowSums(Tiall[c(1:1356),c(4:10)])
TiSinor<-Tiall%>%
        mutate(NorSi=Si/sumdepth)%>%
        mutate(NorTi=Ti/sumdepth)%>%
        mutate(NorCa=Ca/sumdepth)%>%
        mutate(NorK=K/sumdepth)%>%
        mutate(NorAl=Al/sumdepth)

par(mfrow=c(2,2))

plot(TiSinor$Depth.mm,TiSinor$NorSi)
plot(TiSinor$Depth.mm,TiSinor$NorTi)
plot(TiSinor$Depth.mm,TiSinor$NorCa)
plot(TiSinor$Depth.mm,TiSinor$NorAl)
plot(TiSinor$Depth.mm,TiSinor$NorK)
str(TiSinor)

###Merge wirh age if neccesary

ages44<-ages4%>%
        mutate(Depth.mm=Depth*10)

ages_final_ITRAX2<-ages_final_ITRAX%>%
        mutate(Depth.mm=Depth*10)

mg_ages<-merge(ages_final_ITRAX2,TiSinor)
plot(mg_ages$median,mg_ages$NorTi)
plot(TiSinor$Depth.mm,TiSinor$NorTi)




select_1<-mg_ages%>%
        select(1,2,5,10:18,20:24)


summa<-lapply(TiSinor,summary)

##########################################PCA
library(analogue)
library(rioja)

prcurve_ITRAX<-san_final_model10%>%
        select(13:17,28:31,35:36)%>%
        select(-7,-8)%>%
        drop_na()

prcurve_ITRAX_depth<-san_final_model10%>%
        select(13:17,28:32)%>%
        select(-7,-8)%>%
        drop_na()

prcurve_ITRAX3<-select(prcurve_ITRAX,(-(8:9)))

prcurve_ITRAX2<-prcurve(prcurve_ITRAX3,method='ca',trace = TRUE,plotit = TRUE,vary = TRUE,penalty=1.4,smoother = smoothGAM)


varExpl(prcurve_ITRAX2)
# Extract pricipal curve scores
scrs<-scores(prcurve_ITRAX2)
# Plot curve vs. Age
plot(scrs~prcurve_ITRAX$median, type="l")

?varExpl


prc_curve_scores<-as.data.frame(scrs,prcurve_ITRAX$median)
setDT(prc_curve_scores, keep.rownames = TRUE)[]
prc_curve_scores$rn<- as.numeric(as.character(prc_curve_scores$rn))

prc_curve_scores2<-rename(prc_curve_scores,"median"="rn")



d1s <- scale(prcurve_ITRAX3, center=T, scale=T)

pca <- prcomp(prcurve_ITRAX3, center=T, scale.=T)
dissim<- vegdist(prcurve_ITRAX3, method="bray")
plot(pca1$PC1~prcurve_ITRAX$mean, type="l")


pca1<-as.data.frame(pca$x)

#pca22<-merge(pca1,prcurve_ITRAX)

plot(pca22$PC1~pca22$mean,type="l")

#mod_pca <- gam(PC1~ s(mean,k=30), data = pca22, method = "REML")

#plot(mod_pca)
#############################################
mod_pc1 <- gam(PC1 ~ s(mean,k=30), data = pca22, method = "REML")


plot(mod_pc1)



pca <- prcomp(d1s, center=F, scale.=F)

#dissim<- vegdist(d1s, method="bray")

#dissimilarity matrix computation

clust <- chclust(dissim, method="coniss")

#?chclust
#?vegdist
# Ball and Stick
#groups<- bstick(clust, ng=50, plot=T) #determine the appropriate number of significant clusters
groups<- bstick(clust,ng=100)
ngroups<- groups$nGroups[which(groups$dispersion <= groups$bstick)]
ngroups<- ngroups[2]
cc<- cutree(clust, k = ngroups)

# STRAT plot
par(oma=c(2,1,1,1.2))
strat.plot(prcurve_ITRAX3, yvar=prcurve_ITRAX$mean, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="cal BP (yr)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0))
# check that strat plot is ok

x<-strat.plot(prcurve_ITRAX3, yvar=prcurve_ITRAX$mean, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="cal BP (yr)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))

z<-as.matrix(1:ngroups)

addClustZone(x, clust, nZone=ngroups, col=rainbow(length(z))[rank(z)])
c <- cutree(clust, k=ngroups)
data$clust <- c

title(main=strat_title, cex.main=2)



dissim<- vegdist(prcurve_ITRAX_depth_2, method="bray")
clust <- chclust(dissim, method="coniss")

groups<- bstick(clust,ng=50)
ngroups<- groups$nGroups[which(groups$dispersion <= groups$bstick)]
ngroups<- ngroups[2]
cc<- cutree(clust, k = ngroups)
par(oma=c(2,1,1,1.2))
strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))

strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))

# check that strat plot is ok
z<-as.matrix(1:ngroups)

x<-strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))
addClustZone(x, clust, nZone=ngroups, col=rainbow(length(z))[rank(z)])
c <- cutree(clust, k=ngroups)

library(FactoMineR)
library(factoextra)

res.pca <- PCA(prcurve_ITRAX3, graph = FALSE)
dimdesc(res.pca,axes=1)

plot(res.pca, choix = 'varcor')

res.pca$contrib

res <- PCA(prcurve_ITRAX3, scale.unit = FALSE, graph = FALSE)
loadings<-sweep(res$var$coord,2,sqrt(res$eig[1:5,1]),FUN="/")
print(loadings)

#Plot factormine package

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Scores with Factormine. Similar to x (in prcomp package). 

res.pca$ind$coord

#Contributions of variables to PC1

fviz_contrib(res.pca, choice='var', axes=1, top=26)

#Contributions of variables to PC2

fviz_contrib(res.pca, choice='var', axes=2, top=26)


d1ttt <- scale(prcurve_ITRAX3, center=T, scale=T)




library(mgcv)


mod_Si <- gam(NorSi ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_Fe <- gam(NorFe ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_Ti <- gam(NorTi ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_K <- gam(NorK ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")
        
plot(mod_Si)

#library(gratia)
#remotes::install_github("gavinsimpson/gratia")


#nsim <- 20
#small.d <- gratia::fderiv(mod_Ti)
#small.sint <- with(newYear,
 #                  cbind(confint(small.d, nsim = nsim,
  #                               type = "simultaneous"),
   #                      mean = mean))
#small_deriv_plt <- ggplot(small.sint, aes(x = mean, y = est)) +
 #       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
  #                  fill = "black") +
   #     geom_line() +
    #    labs(x = "Year CE", y = "First derivative")

#print(small_deriv_plt)



        # ## Create Pollen principle curve ## -------------------------------------
# Remove age column
#pollen<-BF_dat[-1]
# Run Principal curve 
#pollen.pc<-prcurve(pollen, method='ca',trace = TRUE,plotit = TRUE,vary = TRUE,penalty=1.4)


prcurve_ITRAX_depth_2<-select(prcurve_ITRAX_depth,(-8))
prcurve_ITRAX_depth_2<-rename(prcurve_ITRAX_depth_2, "Si"="NorSi","Ti"="NorTi","Ca"="NorCa","K"="NorK","Al"="NorAl","Fe"="NorFe","Sr"="NorSr")


#prc_curve_scores_2<-merge(prc_curve_scores,prcurve_ITRAX, by="row.names")

plot_pr <- prc_curve_scores %>%
        select(rn, PrC) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = rn, y = PrC), size = 1, alpha = 0.75)+ geom_path(aes(x = rn, y = PrC), size = 1, alpha = 0.75)+
        ylab("PrC") +
        theme_minimal()+
        scale_x_continuous(breaks=seq(0, 32500, by=5000))
        #theme(axis.title.x = element_blank(),
              #axis.text.x = element_blank())
str(prc_curve_scores)




ggplotly(plot_pr)

library(bcp)
library(forecast)
library(quantreg)
library(changepoint)


#cpa_ITRAX<-select(prcurve_ITRAX3,2)

#cpa<-bcp(cpa_ITRAX)

#cpaa<-sqrt(cpa_ITRAX)
#cpaa<-as_vector(cpaa)


#bcp.spor<-bcp(cpaa,w0=0.2,p0=0.01,burnin=1000,mcmc=10000,return.mcmc = TRUE)

#spma<-ma(cpaa,10);spor.ma<-spma[which(spma>0)]
#depth.ma<-prcurve_ITRAX$mean[which(spma>0)]
#plot(spma)

#plot(depth.ma,spor.ma,type="b")
#plot(bcp.spor)


#?bcp

#spore.mean.segn<-cpt.mean(cpaa,penalty="AIC",method="SegNeigh",Q=5)
#plot(spore.mean.segn)


#spore.mean.amoc<-cpt.mean(cpaa,method="AMOC")
#plot(spore.mean.amoc)
#summa2<-sapply(TiSinor,summary)
#summa2<-as.data.frame(summa)
#summa3<-as.matrix(summa)

#But Al and K very low counts. These are the only ones that can be used (Si, Ti, Ca)
Tiall$sumdepth2 <- rowSums(Tiall[c(1:1356),c(5,9,10)])
TiSinor<-Tiall%>%
        mutate(NorSi2=Si/sumdepth2)%>%
        mutate(NorTi2=Ti/sumdepth2)%>%
        mutate(NorCa2=Ca/sumdepth2)%>%
        mutate(NorK2=K/sumdepth2)%>%
        mutate(NorAl2=Al/sumdepth2)

par(mfrow=c(2,2))

plot(TiSinor$Depth.mm,TiSinor$NorSi2)
plot(TiSinor$Depth.mm,TiSinor$NorTi2)
plot(TiSinor$Depth.mm,TiSinor$NorCa2)

#sum<-sapply(TiSinor, function(x) summary(x)$coefficients[2,1:2])

#summary(TiSinor$Al)

############USE?????THis is with all sections, taking put everything less than 60 mm

Tiallall<-rbind(cleanb,cleancr3,cleancr2,cleancr44)


Tiallall$sumdepth <- rowSums(Tiallall[c(1:1518),c(4:10)])
TiSinorall<-Tiallall%>%
        mutate(NorSi=Si/sumdepth)%>%
        mutate(NorTi=Ti/sumdepth)%>%
        mutate(NorCa=Ca/sumdepth)%>%
        mutate(NorK=K/sumdepth)%>%
        mutate(NorAl=Al/sumdepth)%>%
        filter(Depth.mm > 60, NorTi !=0)


plot(TiSinorall$Depth.mm,TiSinorall$NorTi)

#### Mo tub

mo5<-as.data.frame(myfiles[[5]])

cleanmo5<-mo5[-1,]%>%
        select(2,3,9,22,23,26,28:30,44:45)

names(cleanmo5) <- as.matrix(cleanmo5[1, ])
cleanmo5 <- cleanmo5[-1, ]
cleanmo5[] <- lapply(cleanmo5, function(x) type.convert(as.character(x)))


cleanmo5<- mutate(cleanmo5, "Depth.mm"=1:nrow(cleanmo5)) 

cleanmo5a<-filter(cleanmo5,kcps>30000)

gggg<-ggplot(cleanmo5a,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleanmo5a,aes(x=Depth.mm,y='sample surface'))+geom_point()
ggg3<-ggplot(cleanmo5a,aes(x=Depth.mm,y=Fe))+geom_point()

gggg
ggg3

###Mo2

mo6<-as.data.frame(myfiles[[6]])

cleanmo6<-mo6[-1,]%>%
        select(2,3,9,22,23,26,28:30,44:45)

names(cleanmo6) <- as.matrix(cleanmo6[1, ])
cleanmo6 <- cleanmo6[-1, ]
cleanmo6[] <- lapply(cleanmo6, function(x) type.convert(as.character(x)))


cleanmo6<- mutate(cleanmo6, "Depth.mm"=(237+1:nrow(cleanmo6))) 

cleanmo6a<-filter(cleanmo6,kcps>40000)

gggg<-ggplot(cleanmo6a,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleanmo6a,aes(x=Depth.mm,y='sample surface'))+geom_point()
ggg3<-ggplot(cleanmo6a,aes(x=Depth.mm,y=Fe))+geom_point()

gggg
ggg3

###Mo7
mo7<-as.data.frame(myfiles[[7]])

cleanmo7<-mo7[-1,]%>%
        select(2,3,9,22,23,26,28:30,44:45)

names(cleanmo7) <- as.matrix(cleanmo7[1, ])
cleanmo7 <- cleanmo7[-1, ]
cleanmo7[] <- lapply(cleanmo7, function(x) type.convert(as.character(x)))


cleanmo7<- cbind(cleanmo7, "Depth.mm"=(763+1:nrow(cleanmo7)))

cleanmo7a<-filter(cleanmo7,kcps>40000)

gggg<-ggplot(cleanmo7a,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleanmo7a,aes(x=Depth.mm,y='sample surface'))+geom_point()
ggg3<-ggplot(cleanmo7a,aes(x=Depth.mm,y=Ar))+geom_point()

gggg
ggg3

##Mo4
mo8<-as.data.frame(myfiles[[8]])

cleanmo8<-mo8[-1,]%>%
        select(2,3,9,22,23,26,28:30,44:45)

names(cleanmo8) <- as.matrix(cleanmo8[1, ])
cleanmo8 <- cleanmo8[-1, ]
cleanmo8[] <- lapply(cleanmo8, function(x) type.convert(as.character(x)))


cleanmo8<- mutate(cleanmo8, "Depth.mm"=(1289+1:nrow(cleanmo8))) 

cleanmo8a<-filter(cleanmo8,kcps>42000)

gggg<-ggplot(cleanmo8a,aes(x=Depth.mm,y=kcps))+geom_point()
ggg2<-ggplot(cleanmo8a,aes(x=Depth.mm,y='sample surface'))+geom_point()
ggg3<-ggplot(cleanmo8a,aes(x=Depth.mm,y=Ar))+geom_point()

gggg
ggg3

##Using the four sections and normalizing

Moall<-rbind(cleanmo5a,cleanmo6a,cleanmo7a,cleanmo8a)
Moall$sumdepth <- rowSums(Moall[c(1:1679),c(4:9)])


Monorall<-Moall%>%
        mutate(NorFe=Fe/sumdepth)%>%
        mutate(NorBr=Br/sumdepth)%>%
        mutate(NorRb=Rb/sumdepth)%>%
        mutate(NorSr=Sr/sumdepth)%>%
        filter(Depth.mm> 60)


first<-filter(Monorall,Depth.mm <220)
plot(Monorall$Depth.mm,Monorall$NorFe)
plot(first$Depth.mm,first$NorFe)


Moallfilter<-filter(Moall,Depth.mm>200)

plot(Moallfilter$Depth.mm,Moallfilter$Fe)

plot(Moallfilter$Depth.mm,Moallfilter$Br)



Moallfilter$sumdepth <- rowSums(Moallfilter[c(1:1479),c(4:9)])
Monor<-Moallfilter%>%
        mutate(NorFe=Fe/sumdepth)%>%
        mutate(NorBr=Br/sumdepth)%>%
        mutate(NorRb=Rb/sumdepth)%>%
        mutate(NorSr=Sr/sumdepth)



plot(Monor$Depth.mm,Monor$NorBr)
plot(Monor$Depth.mm,Monor$NorFe)
plot(Monor$Depth.mm,Monor$NorRb)
#plot(Monor$Depth.mm,Monor$NorMg)
plot(Monor$Depth.mm,Monor$NorSr)
#plot(Monor$Depth.mm,Monor$NorBa)
#MofilterFe<-Moallfilter%>%
#       mutate(sumdepth=rowSums(Moallfilter[c(1:1259),c(17)]))



summaMo<-lapply(Monor,summary)
#summa2<-as.data.frame(summaMo)
#summa3<-unlist(summa)

#####Zn and Mn low counts

Moall2<-Moall



Moall2$sumdepth2 <- rowSums(Moall2[c(1:1679),c(4:9)])
Monor2<-Moall2%>%
        mutate(NorFe=Fe/sumdepth2)%>%
        mutate(NorBr=Br/sumdepth2)%>%
        mutate(NorRb=Rb/sumdepth2)%>%
        mutate(NorSr=Sr/sumdepth2)

plot(Monor$Depth.mm,Monor$NorBr)
plot(Monor$Depth.mm,Monor$NorFe)
plot(Monor$Depth.mm,Monor$NorRb)
#plot(Monor$Depth.mm,Monor$NorMg)
plot(Monor$Depth.mm,Monor$NorSr)


select_final<-merge(select_1,Monor2,by="Depth.mm")
san_final_model<-select_final%>%
        select(1:10,13:17,21:26,30:33)

######FINAL

select_final2<-merge(select_1,Monor2,by="Depth.mm",all=T)
san_final_model2<-select_final%>%
        select(1:10,13:17,21:26,30:33)


select_final3<-merge(Monor2,select_1,by="Depth.mm",all=T)
san_final_model3<-select_final%>%
        select(1:10,13:17,21:26,30:33)%>%
        merge(correctdepths,all=T)%>%
        filter(NorFe>0.5)

san_final_model4<-select_final%>%
        select(1:10,13:17,21:26,30:33)%>%
        merge(correctdepths,all=T)%>%
        filter(NorFe>0.5,Depth.mm>238 & Depth.mm<751)

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)


#ages5<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Bacon_runs/SAN8_2019_5/SAN8_2019_5_35_ages.txt",skip=1)


#colnames(ages4)<-c("Depth","max","min","median","mean")
#colnames(ages5)<-c("Depth","max","min","median","mean")


#agedepth2<-left_join(correctdepths,ages3,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax
#agedepthb<-merge(correctdepths,ages3,by="Depth")

#agedepth2<-left_join(correctdepths,ages5,by="Depth")

#agedepth4<-right_join(correctdepths,ages5,by="Depth")
#agedepth2$Identifier<- as.numeric(as.character(agedepth2$Identifier))

#correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")

#correctdepths<-correctdepths %>% rename(Depth=Real.depth)

#ages_final<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_10/SAN8_2019_10_35_ages.txt",skip=1)
#colnames(ages_final)<-c("Depth","max","min","median","mean")


#agedepth<-merge(correctdepths,ages_final,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

#agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))


#agedepth2<-mutate(agedepth,Identifier=ifelse(Depth==47, (75), Identifier))
#agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))


#agedepth3<-mutate(agedepth2,Depth.mm=Depth*10)


select_final10<-merge(TiSinorall,Monorall,by="Depth.mm",all=T)
san_final_model10<-select_final10%>%
        select(1:11,14:28,31:35)%>%
        left_join(ages_final_ITRAX2,all=T)

#san_final_model10<-select_final10%>%
#       select(1:11,14:28,31:35)%>%
#      merge(ages44,all=T)

#select_final10<-merge(TiSinorall,Monorall,by="Depth.mm",all=T)%>%
#       select(1:11,14:28,31:35)%>%
#      left_join(agedepth3)

par(mfrow=c(2,2))

TI<-ggplot(san_final_model10, aes(x=median,y=NorTi))+ xlab("Age (cal yr BP)")+ ylab("Nor Ti counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 34500, by=5000))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

TI

TI2<-ggplot(san_final_model10, aes(x=Depth,y=NorTi))+ xlab("Depth (cm)")+ ylab("Normalized Ti counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 175, by=20))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

TI2

FE2<-ggplot(san_final_model10, aes(x=Depth,y=NorFe))+ xlab("Depth (cm)")+ ylab("Nor Fe counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 175, by=20))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

FE2

FE<-ggplot(san_final_model10, aes(x=median,y=NorFe))+ xlab("Depth (cm)")+ ylab("Nor Fe counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))

FE

AL<-ggplot(san_final_model10, aes(x=median,y=NorAl))+ xlab("Depth (cm)")+ ylab("Nor Al counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

AL


AL2<-ggplot(san_final_model10, aes(x=Depth,y=NorAl))+ xlab("Depth (cm)")+ ylab("Nor Al counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 175, by=20))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

AL2

CA<-ggplot(san_final_model10, aes(x=median,y=NorCa))+ xlab("Depth (cm)")+ ylab("Nor Ca counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

CA

CA2<-ggplot(san_final_model10, aes(x=Depth,y=NorCa))+ xlab("Depth (cm)")+ ylab("Nor Ca counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 175, by=20))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

CA2

library(plotly)
ggplotly(CA)
ggplotly(SI)
ggplotly(TI)
SI2<-ggplot(san_final_model10, aes(x=Depth,y=NorSi))+ xlab("Depth (cm)")+ ylab("Nor Si counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 175, by=20))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

SI2

SI<-ggplot(san_final_model10, aes(x=median,y=NorSi))+ xlab("Age cal BP (yr)")+ ylab("Nor Si counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

SI


SR<-ggplot(san_final_model10, aes(x=median,y=NorSr))+ xlab("Age cal BP (yr)")+ ylab("Nor Si counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
SR

Rb<-ggplot(san_final_model10, aes(x=median,y=NorRb))+ xlab("Age cal BP (yr)")+ ylab("Nor Si counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
Rb
ggplotly(Rb)

K2<-ggplot(san_final_model10, aes(x=median,y=NorK))+ xlab("Depth (cm)")+ ylab("Nor K counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))
#Age in y

K2


Rb2<-ggplot(san_final_model10, aes(x=median,y=NorRb))+ xlab("Depth (cm)")+ ylab("Nor Rb counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))

Rb2

Br2<-ggplot(san_final_model10, aes(x=median,y=NorBr))+ xlab("Depth (cm)")+ ylab("Nor Br counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))

Br2

Sr2<-ggplot(san_final_model10, aes(x=median,y=NorSr))+ xlab("Depth (cm)")+ ylab("Nor Sr counts")+geom_point()+ theme_bw()  +scale_x_continuous(breaks=seq(0, 32500, by=2500))+theme_bw()+theme(axis.text.x=element_text(size=18),axis.title.x=element_text(size=18), axis.text.y=element_text(size=18),axis.title.y=element_text(size=18))

Sr2

?grid.arrange

grid.arrange(TI2, SI2,FE2,CA2,AL2, nrow=5)
par(mfrow=c(1,1))

plot(san_final_model10$Depth.mm,san_final_model10$NorFe)
abline(v=c(730, 1230), lty=1, col="red")

plot(san_final_model10$Depth.mm,san_final_model10$NorTi)
abline(v=c(730, 1230), lty=1, col="red")

par(mfrow=c(1,1))

plot(san_final_model10$median,san_final_model10$NorFe)
plot(san_final_model10$median,san_final_model10$NorTi)
abline(v=c(19614, 28000), lty=1, col="red")##Lines corresponding to the splitting of cores

plot(san_final_model10$mean,san_final_model10$NorSi)
#plot(san_final_model10$mean,san_final_model10$NorAl)
plot(san_final_model10$mean,san_final_model10$Mn)

plot(san_final_model10$Depth.mm,san_final_model10$NorTi)
abline(v=c(730, 1230), lty=1, col="red")

setwd('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD')