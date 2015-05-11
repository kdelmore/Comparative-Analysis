## load libraries and deal with data

library(lme4) 
library(MuMIn)
library(nlme)

setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
comp <- read.csv("../working files/pair_hedges_avg_feb3.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

comp$overlap_proportion_log <- log(1+comp$overlap_proportion) ## log the skewed variables
comp$breed_lat_avg_log <- log(comp$breed_lat_avg)
comp$distance_avg_log <- log(comp$distance_avg)
comp$p_distance_log <- log(comp$p_distance)
comp$mass_avg_log <- log(comp$mass_avg)
comp$avg_log <- log(comp$avg_2)
comp$avg_morph_log <- log(comp$avg_morph)
comp$avg_song_log <- log(comp$avg_song)
comp$avg_colour_log <- log(comp$avg_colour)

comp$fam<-as.factor(comp$family) ## convert characters to factors
comp$sp1_genus<-as.factor(comp$sp1_genus)
comp$migration_category<-as.factor(comp$migration_category)
comp$visual_official<-as.factor(comp$visual_official)
comp$visual_official_ordered<-ordered(comp$visual_official)
#comp$visual_official_2<-as.factor(comp$visual_official_2)
#comp$allo_sym<-as.factor(comp$allo_sym)
#comp$allo_sym_para_perp<-as.factor(comp$allo_sym_para_perp)

#### combined, visual then overlap

Global_Personal.Use<-lmer(avg_log ~migration_category+visual_official_ordered+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus) ,data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

Global_Personal.Use<-lmer(avg_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

#### colour, visual then overlap

Global_Personal.Use<-lmer(avg_colour_log ~migration_category+visual_official_ordered+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

Global_Personal.Use<-lmer(avg_colour_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

#### song, visual then overlap

Global_Personal.Use<-lmer(avg_song_log ~migration_category+visual_official_ordered+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

Global_Personal.Use<-lmer(avg_song_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

#### morph, visual then overlap

Global_Personal.Use<-lmer(avg_morph_log ~migration_category+visual_official_ordered+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

Global_Personal.Use<-lmer(avg_morph_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model

## plot two cats
as.factor(comp$allo_sym_para_perp)
palette(value=c("black","black","red","red"))
pch_lookup <- c(parallel_allopatric = 1, parallel_sympatric = 16, perpendicular_allopatric = 1, perpendicular_sympatric = 16)
plot(comp$p_distance,comp$avg_2,col=comp$allo_sym_para_perp,pch=pch_lookup[as.numeric(comp$allo_sym_para_perp)],xlab=("time since divergence"),ylab=("divergence"))

## plot three cats
as.factor(comp$visual_official_2)
palette(value=c("black","black","black","red","red","red"))
pch_lookup <- c(parallel_allo = 2, parallel_sym = 16, parallel_para = 1, perpendicular_allo = 2, perpendicular_sym = 16, perpendicular_para = 1)
plot(comp$p_distance,comp$avg_2,col=comp$visual_official_2,pch=pch_lookup[as.numeric(comp$visual_official_2)],xlab=("time since divergence"),ylab=("divergence"))
