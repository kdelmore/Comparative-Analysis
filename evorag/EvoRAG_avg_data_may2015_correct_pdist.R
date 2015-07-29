############## LOAD LIBRARIES, DATA AND PREP

setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
library(EvoRAG) #use EvoRAG 2
#evodat<-read.csv("../working files/pair_hedges_avg_feb3.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))
evodat<-read.csv("../working files/pair_hedges_avg_jul24.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

models = c("BM_null", "OU_null")
           
# ## transforming original variables
# evodat["max_div10"]<-evodat$max/10
# evodat["tot_div100"]<-evodat$total/100
# evodat["p_distance_100"]<-evodat$p_distance*100
# evodat["max_song10"]<-evodat$max_song/10
# evodat["tot_song10"]<-evodat$total_song/10
# evodat["max_colour10"]<-evodat$max_colour/10
# evodat["tot_colour10"]<-evodat$total_colour/10
# evodat["max_morph10"]<-evodat$max_morph/10
# evodat["tot_morph10"]<-evodat$total_morph/10

## transforming avg variables
#evodat["p_distance_100"]<-evodat$p_distance*100
evodat["speed_coi_100"]<-evodat$speed_coi*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10

## set trait for all analyses
evodat=subset(evodat,evodat$mtdna_type!="no")
evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10

############## RUN MODEL ALLOWING NO SEPARATE RATES
e1<-evodat$trait
#t1<-evodat$p_distance_100
t1<-evodat$speed_coi_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs

plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)

############## RUN MODEL WITH SEPARATE RATES FOR PARALLELS AND PERPENDICULARS

## parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
#t2<-evodat_parallel$p_distance_100
t2<-evodat_parallel$speed_coi_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters

## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$speed_coi_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters

## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)

logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)