evodat<-read.csv(file.choose(), stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA","") ) #pair_hedges_avg_feb3
library(EvoRAG) #use EvoRAG 2

models<-c("BM_null","OU_null") # set models
#testing
#transforming original variables
evodat["max_div10"]<-evodat$max/10
evodat["tot_div100"]<-evodat$total/100
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["max_song10"]<-evodat$max_song/10
evodat["tot_song10"]<-evodat$total_song/10
evodat["max_colour10"]<-evodat$max_colour/10
evodat["tot_colour10"]<-evodat$total_colour/10
evodat["max_morph10"]<-evodat$max_morph/10
evodat["tot_morph10"]<-evodat$total_morph/10

#transforming avg variables
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10


##### set trait for all analyses
evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed

############## ALL TOGETHER (no separate rates)
e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs

plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")

par(new = TRUE) # to plot multiple plots on top of one another
print(fit_all) # I've just been pasting output variables manually into the spreadsheet instead of exporting all thisdata (so inelegant...)


############## PARALLELS vs PERPENDICULARS

#parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
t2<-evodat_parallel$p_distance_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters

#for parallel confidence intervals
parR<-bootstrap.test(e2,t2,l2, model="OU_null", parameters=c(0.0404328,1.1825905),meserr1=0, meserr2=0,breakpoint = "NULL", N = c(1000), starting=NULL) #need to fill in correct parameters
parR$summary


#perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$p_distance_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters

#for perp confidence intervals
perpR<-bootstrap.test(e3,t3,l3, model="OU_null", parameters=c(0.009246617,0.239878507),meserr1=0, meserr2=0,breakpoint = "NULL", N = c(1000), starting=NULL) #need to fill in correct parameters
perpR$summary

#to calculate AIC for perp/par######################
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)

logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)


#####################################PATRY
#allopatric
evodat_allo<-subset(evodat,evodat$visual_official=="allo")
e4 <- evodat_allo$trait
t4<-evodat_allo$p_distance_100
l4<-evodat_allo$mass_avg
fit_allo <- model.test.sisters(e4,t4,l4,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t4,e4,xlim=c(0,10),ylim=c(0,1),main="allo")
print(fit_allo) #View model parameters

#parapatric
evodat_parapatric<-subset(evodat,evodat$visual_official=="para")
e5 <- evodat_parapatric$trait
t5<-evodat_parapatric$p_distance_100
l5<-evodat_parapatric$mass_avg
fit_parapatric <- model.test.sisters(e5,t5,l5,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t4,e4,xlim=c(0,10),ylim=c(0,1),main="parapatric")
print(fit_parapatric) #View model parameters

#sympatric
evodat_sympatric<-subset(evodat,evodat$visual_official=="sym")
e6 <- evodat_parapatric$trait
t6<-evodat_parapatric$p_distance_100
l6<-evodat_parapatric$mass_avg
fit_sympatric <- model.test.sisters(e6,t6,l6,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
plot(t4,e4,xlim=c(0,10),ylim=c(0,1),main="sympatric")
print(fit_sympatric) #View model parameters

#to calculate AIC for patry###################### need to double-check these formulas
logLike_BM_patry<-fit_allo[1,1]+fit_parapatric[1,1]+fit_sympatric[1,1]
AIC_BM_patry<-2*1-2*logLike_BM_patry
print(AIC_BM_patry)

logLike_OU_patry<-fit_allo[1,1]+fit_parapatric[1,1]+fit_sympatric[1,1]
AIC_OU_patry<-2*2-2*logLike_OU_patry
print(AIC_OU_patry)


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
pdf("c:/Users/Haley/Documents/Comparative Extravaganza/Evolutionary change/averages 2 Feb 2015/figures/avg_overall_div.pdf",width=6,height=4)
plot(t1,e1,pch=c(16,1)[as.numeric(factor(evodat$migration_category))],xlab="time since divergence",ylab="Average divergence",cex.lab=1.5,cex.axis=1.2,ylim=c(0,0.5),yaxt="n")
axis(side=2,at=c(0,0.25,0.5),labels=c("0.0","0.25","0.5"))
par(new = TRUE)
lines(ETpar)
par(new = TRUE)
lines(ETperp,lty=2)
dev.off()
