############## LOAD LIBRARIES, DATA AND PREP

setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
library(EvoRAG) #use EvoRAG 2
evodat<-read.csv("../working files/pair_hedges_avg_feb3.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))
#evodat<-read.csv(file.choose(), stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA","") ) #pair_hedges_avg_feb3

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
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10

## set trait for all analyses
evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10

############## RUN MODEL ALLOWING NO SEPARATE RATES
e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs

plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)

############## RUN MODEL WITH SEPARATE RATES FOR PARALLELS AND PERPENDICULARS

## parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
t2<-evodat_parallel$p_distance_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters

## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$p_distance_100
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


############## RUN MODEL WITH SEPARATE RATES FOR ALLO, PARA AND SYM

## allopatric
evodat_allo<-subset(evodat,evodat$visual_official=="allo")
e4 <- evodat_allo$trait
t4<-evodat_allo$p_distance_100
l4<-evodat_allo$mass_avg
fit_allo <- model.test.sisters(e4,t4,l4,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t4,e4,xlim=c(0,10),ylim=c(0,1),main="allo")
print(fit_allo) #View model parameters

## parapatric
evodat_parapatric<-subset(evodat,evodat$visual_official=="para")
e5 <- evodat_parapatric$trait
t5<-evodat_parapatric$p_distance_100
l5<-evodat_parapatric$mass_avg
fit_parapatric <- model.test.sisters(e5,t5,l5,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t5,e5,xlim=c(0,10),ylim=c(0,1),main="parapatric")
print(fit_parapatric) #View model parameters

## sympatric
evodat_sympatric<-subset(evodat,evodat$visual_official=="sym")
e6 <- evodat_sympatric$trait
t6<-evodat_sympatric$p_distance_100
l6<-evodat_sympatric$mass_avg
fit_sympatric <- model.test.sisters(e6,t6,l6,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t6,e6,xlim=c(0,10),ylim=c(0,1),main="sympatric")
print(fit_sympatric) #View model parameters

## calculate AIC for patry
logLike_BM_patry<-fit_allo[1,1]+fit_parapatric[1,1]+fit_sympatric[1,1]
AIC_BM_patry<-2*1-2*logLike_BM_patry
print(AIC_BM_patry)

logLike_OU_patry<-fit_allo[1,2]+fit_parapatric[1,2]+fit_sympatric[1,2]
AIC_OU_patry<-2*2-2*logLike_OU_patry
print(AIC_OU_patry)


############## RUN MODEL WITH SEPARATE RATES FOR ALLO vs PARA/SYM (i.e. no gene flow, at least some gene flow)

## allopatric
evodat_no<-subset(evodat,evodat$visual_official_3=="no")
e7 <- evodat_no$trait
t7<-evodat_no$p_distance_100
l7<-evodat_no$mass_avg
fit_no <- model.test.sisters(e7,t7,l7,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t7,e7,xlim=c(0,10),ylim=c(0,1),main="allo")
print(fit_no) #View model parameters

## not allopatric
evodat_flow<-subset(evodat,evodat$visual_official_3=="flow")
e8 <- evodat_flow$trait
t8<-evodat_flow$p_distance_100
l8<-evodat_flow$mass_avg
fit_flow <- model.test.sisters(e8,t8,l8,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t8,e8,xlim=c(0,10),ylim=c(0,1),main="parapatric")
print(fit_flow) #View model parameters

## calculate AIC for patry
logLike_BM_patry2<-fit_no[1,1]+fit_flow[1,1]
AIC_BM_patry2<-2*1-2*logLike_BM_patry2
print(AIC_BM_patry2)

logLike_OU_patry2<-fit_no[1,2]+fit_flow[1,2]
AIC_OU_patry2<-2*2-2*logLike_OU_patry2
print(AIC_OU_patry2)


############## RUN MODEL WITH SEPARATE RATES FOR PARALLELS AND PERPENDICULARS AND INCLUDING PATRY AS CONT VARIABLE
models = c("BM_null", "BM_linear", "OU_null", "OU_linear","OU_linear_beta") ##there's a note in Lawson and Weir about how changes in alpha are not reliable or something, why he has linear_beta model

## make patry a continuous variable, yes this could be more compact ...
#note that the program will not take patry as an ordinal variable
evodat$visual_official_cont<-as.character(evodat$visual_official)
evodat$visual_official_cont[evodat$visual_official_cont == "allo"] <- 1
evodat$visual_official_cont[evodat$visual_official_cont == "para"] <- 2
evodat$visual_official_cont[evodat$visual_official_cont == "sym"] <- 3
evodat$visual_official_cont<-as.numeric(evodat$visual_official_cont)

## parallels with patry invoked as first linear variable (so instead of the filler of mass)
evodat_parallel_patry<-subset(evodat,evodat$migration_category=="parallel")
e9 <- evodat_parallel_patry$trait
t9<-evodat_parallel_patry$p_distance_100
#l2<-evodat_parallel$mass_avg 
g9<-evodat_parallel_patry$visual_official_cont
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters

#ci for parallels (HALEY - CIs for both para and perp start at 0.00001, do you know if it go negative or is this as low as it goes, suggesting we don't have much power to estimate this parameter)
intercept_beta_par<-as.numeric(fit_par_patry[5,4])
slope_beta_par<-as.numeric(fit_par_patry[6,4])
intercept_alpha_par<-as.numeric(fit_par_patry[11,4])
slope_alpha_par<-as.numeric(fit_par_patry[12,4])
parameters_par=c(intercept_beta_par, slope_beta_par, intercept_alpha_par, slope_alpha_par)
set.seed(seed = 3)
parR <- bootstrap.test(e9,t9,g9,model="OU_linear", parameters_par, meserr1=0, meserr2=0, breakpoint = "NULL", N = c(1000), starting=NULL)
parR$summary

## perpendiculars
evodat_perp_patry<-subset(evodat,evodat$migration_category=="perpendicular")
e10 <- evodat_perp_patry$trait
t10 <-evodat_perp_patry$p_distance_100
#l3<-evodat_perp$mass_avg
g10 <-evodat_perp_patry$visual_official_cont
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters

#ci for perpendiculars
intercept_beta_perp<-as.numeric(fit_perp_patry[5,4])
slope_beta_perp<-as.numeric(fit_perp_patry[6,4])
intercept_alpha_perp<-as.numeric(fit_perp_patry[11,4])
slope_alpha_perp<-as.numeric(fit_perp_patry[12,4])
parameters_per=c(intercept_beta_perp, slope_beta_perp, intercept_alpha_perp, slope_alpha_perp)
set.seed(seed = 3)
perpR <- bootstrap.test(e10,t10,g10,model="OU_linear", parameters_perp, meserr1=0, meserr2=0, breakpoint = "NULL", N = c(1000), starting=NULL)
perpR$summary

## calculate AIC for perp/par with patry
logLike_BM<-fit_par_patry[1,2]+fit_perp_patry[1,2]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)

logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)


############## PLOT MODEL WITH HIGHEST AIC
evodat$visual_official_2<-as.factor(evodat$visual_official_2) #so we can see which pairs are allo etc
palette(value=c("black","black","black","red","red","red")) #so perp and para are diff colours
pch_lookup <- c(parallel_allo = 2, parallel_sym = 16, parallel_para = 1, perpendicular_allo = 2, perpendicular_sym = 16, perpendicular_para = 1) #so allo etc are different symbols

##plotting avg_2 using values from OU_null
e1<-evodat$avg_all10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.0404328
par_a_OU<-1.1825905
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.006061431
perp_a_OU<-0.183192865
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="overall phenotypic divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

##plotting avg_2 using values from OU_linear (HALEY - lines seem low relative to data points)
par_b_OU<-0.03849
par_a_OU<-1.422
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.00425
perp_a_OU<-0.4111053
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
#ETpar<-expectation.time(intercept_beta_par,intercept_alpha_par,time.span=c(0,10))
#ETperp<-expectation.time(intercept_beta_perp,intercept_alpha_perp,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="overall phenotypic divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

##plotting avg_2 using values from OU_linear_beta (HALEY - lines seem even lower relative to data points)
par_b_OU<-0.0324
par_a_OU<-1.259
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.001451747
perp_a_OU<-0.211810875
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="overall phenotypic divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

## trying to plot gradient using values from OU_linear, HALEY - looks better if quantile=FALSE but not 100% sure if this is right, think it might be though, think the quantiles are for when you have two vars PLUS a gradient)
ETpargrad <- expectation.gradient(gradient.span = c(0, 3), model = c("OU_linear"),
                                  values = FALSE, parameters_par, time=c(3),quantile=FALSE)
ETperpgrad <- expectation.gradient(gradient.span = c(0, 3), model = c("OU_linear"),
                                   values = FALSE, parameters_perp, time=c(3),quantile=FALSE)

#################
#################
#################

##################################################FIGS

#template

e1<-evodat$#insert trait here
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-#insert parameter
par_a_OU<-#insert parameter
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-#insert parameter
perp_a_OU<-#insert parameter
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_song_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="average song divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()





##########################older figures with parameters filled in
#####avg_song
e1<-evodat$avg_song10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.02074476
par_a_OU<-0.42367374
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.008467075
perp_a_OU<-0.690203719
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_song_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="average song divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()

#####avg_morph
e1<-evodat$avg_morph10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.1445323
par_a_OU<-6.4260147
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.005943889
perp_a_OU<-0.017960849
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_morph_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="Average Morphological divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,1),yaxt="n")
axis(side=2,at=c(0,0.5,1),labels=c("0.0","0.5","1.0"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()

#####avg_colour
e1<-evodat$avg_colour10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.03730289
par_a_OU<-0.57014268
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.009246617
perp_a_OU<-0.239878507
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_colour_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="Average Colour divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.75),yaxt="n")
axis(side=2,at=c(0,0.5),labels=c("0.0","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()

#####avg_overall
e1<-evodat$avg_all10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.0404328
par_a_OU<-1.1825905
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.006061431
perp_a_OU<-0.183192865
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
#pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_overall_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="Average divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
#dev.off()


###KDEL MUCKING ABOUT
#########avg_song
e1<-evodat$avg_song10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.02074476
par_a_OU<-0.42367374
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.008467075
perp_a_OU<-0.690203719
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="song divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

#####avg_colour
e1<-evodat$avg_colour10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.03730289
par_a_OU<-0.57014268
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.009246617
perp_a_OU<-0.239878507
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="colour divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.75),yaxt="n")
axis(side=2,at=c(0,0.5),labels=c("0.0","0.5"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

#####avg_morph
e1<-evodat$avg_morph10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.1445323
par_a_OU<-6.4260147
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.005943889
perp_a_OU<-0.017960849
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))

plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="morphological divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,1),yaxt="n")
axis(side=2,at=c(0,0.5,1),labels=c("0.0","0.5","1.0"))
par(new = TRUE)
lines(ETpar,col="black")
par(new = TRUE)
lines(ETperp,col="red")

###########plotting for figure s2
evodat$visual_official_2<-as.factor(evodat$visual_official_2) #so we can see which pairs are allo etc
palette(value=c("black","black","black","red","red","red")) #so perp and para are diff colours
pch_lookup <- c(parallel_allo = 2, parallel_sym = 16, parallel_para = 1, perpendicular_allo = 2, perpendicular_sym = 16, perpendicular_para = 1) #so allo etc are different symbols

#####avg_song
e1<-evodat$avg_song10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.02074476
par_a_OU<-0.42367374
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.008467075
perp_a_OU<-0.690203719
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("avg_song_div_patry.pdf",width=6,height=4)
plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="average song divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,col="red")
dev.off()

#####avg_morph
e1<-evodat$avg_morph10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.1445323
par_a_OU<-6.4260147
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.005943889
perp_a_OU<-0.017960849
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("avg_morph_div.pdf",width=6,height=4)
plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="Average Morphological divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,1),yaxt="n")
axis(side=2,at=c(0,0.5,1),labels=c("0.0","0.5","1.0"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()

#####avg_colour
e1<-evodat$avg_colour10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.03730289
par_a_OU<-0.57014268
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.009246617
perp_a_OU<-0.239878507
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("avg_colour_div.pdf",width=6,height=4)
plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="Average Colour divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.75),yaxt="n")
axis(side=2,at=c(0,0.5),labels=c("0.0","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()

#####avg_overall
e1<-evodat$avg_all10
t1<-evodat$p_distance_100 
l1<-evodat$mass_avg
par_b_OU<-0.0404328
par_a_OU<-1.1825905
ETpar<-expectation.time(par_b_OU,par_a_OU,time.span=c(0,10))
perp_b_OU<-0.006061431
perp_a_OU<-0.183192865
ETperp<-expectation.time(perp_b_OU,perp_a_OU,time.span=c(0,10))
pdf("avg_overall_div.pdf",width=6,height=4)
plot(t1,e1,col=evodat$visual_official_2,pch=pch_lookup[as.numeric(evodat$visual_official_2)],xlab="time since divergence",ylab="Average divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()
