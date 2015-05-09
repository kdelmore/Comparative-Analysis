## load libraries and deal with data

library(lme4) 
library(MuMIn)
library(nlme)

setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
#comp <- read.csv("../working files/pair_hedges_jun13.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))
#comp <- read.csv("../working files/pair_hedges_sep17.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))
comp <- read.csv("../working files/pair_hedges_avg_feb3.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

comp$overlap_proportion_log <- log(1+comp$overlap_proportion) ## log the skewed variables
comp$breed_lat_avg_log <- log(comp$breed_lat_avg)
comp$distance_avg_log <- log(comp$distance_avg)
comp$p_distance_log <- log(comp$p_distance)
comp$mass_avg_log <- log(comp$mass_avg)
#comp$max_log <- log(comp$max)
#comp$total_log <- log(comp$total)
comp$avg_log <- log(comp$avg_2)

comp$fam<-as.factor(comp$family) ## convert characters to factors
comp$sp1_genus<-as.factor(comp$sp1_genus)
comp$migration_category<-as.factor(comp$migration_category)

covar <- c( ## assign variables to a string for coming up with different combos below
  
  "migration_category",   #1
  "overlap_proportion_log",   #2
  "breed_lat_avg_log",        #3
  "distance_avg_log" ,        #4
  "p_distance_log",  #5
  "mass_avg_log"  #6
)

## construct all possible combinations of variables for models and paste formula

n <- length(covar)
id <- unlist( ## gives list with the numbers from the vector above and the models they go into
  lapply(1:n,
         function(i)combn(1:n,i,simplify=F)
  ),recursive=F)

#total
form <- sapply(id,function(i)
  paste("total_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_total <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#max
form <- sapply(id,function(i)
  paste("max_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_max <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#avg
form <- sapply(id,function(i)
  paste("avg_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_avg <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#avg w/o family/genus nested
form <- sapply(id,function(i)
  paste("avg_log ~",paste(covar[i],collapse="+"), sep="") 
)

fm_avg <- lapply(form,function(i) try(lm(as.formula(i),data=comp))) 

## standard multi-model code, will need to be modified if we remove pdist as random variable (ie. just run lm)
# first part organizes, second part tells it what to put in the output file
# we're interested in the parameter estimates and delta AIC, cut at 2 or 7 for AIC and see if parameter estimates overlap 0
# if more than one model will also use weights to average models in excel

glm.lst <- function(modlst)
{
  
  estList <- lapply(modlst, function(x) fixef(x))
  
  seList <- lapply(modlst, function(x) {
    se <- tryCatch(sqrt(diag(vcov(x))),
                   error=function(e) simpleError("Hessian is singular."))
    if(identical(class(se)[1], "simpleError")) {
      cat(se$message, fill=TRUE)
      se <- rep(NA, length(function(x) fixef(x)))
    }
    names(se) <- names(fixef(x))
    
    
    return(se)
  })
  
  eNames <- sort(unique(unlist(sapply(estList, names))))
  seNames <- paste("SE_", eNames, sep="")
  eseNames <- character(l <- length(c(eNames, seNames)))
  eseNames[seq(1, l, by=2)] <- eNames
  eseNames[seq(2, l, by=2)] <- seNames
  cNames <- c("formula", eseNames)
  out <- data.frame(matrix(NA, ncol=length(cNames), nrow=length(modlst)))
  colnames(out) <- cNames
  #out$model <- names(fits)
  out$formula <- sapply(modlst, function(x) {
    f <- as.character(formula(x))
    f <- paste(f[2], "~", f[3])
    f
  })
  for(i in 1:length(eNames)) {
    out[,eNames[i]] <- sapply(estList, function(x) x[eNames[i]])
    out[,seNames[i]] <- sapply(seList, function(x) x[eNames[i]])
  }
  out$negLogLike <- sapply(modlst, function(x) logLik(x)[1])
  out$n <- sapply(modlst, function(x) length(x@resp$y))
  out$R2m <- sapply(modlst, function(x) r.squaredGLMM(x)[1])
  out$R2c <- sapply(modlst, function(x) r.squaredGLMM(x)[2])
  out$converged <- sapply(modlst, function(x) ifelse(length(x@optinfo$conv$lme4$code)==0,0,x@optinfo$conv$lme4$code))
  out$convmessg <- sapply(modlst, function(x) ifelse(length(x@optinfo$conv$lme4$messages)==0,0,x@optinfo$conv$lme4$messages))
  #out$DF <- sapply(modlst, function(x) x$df.residual)
  out$AICc <- sapply(modlst, function(x) AICc(x))
  out$deltaAIC <- out$AICc - min(out$AICc)
  out$AICwt <- exp(-out$delta / 2)
  out$AICwt <- out$AICwt / sum(out$AICwt)
  out <- out[order(out$AICc),]
  out$cumltvWt <- cumsum(out$AICwt)
  return(out)
}

#modlst_total <- glm.lst(fm_total) ## all models
#write.csv(modlst_total,file ="Comparative_analysis_total.csv",row.names=F)

#modlst_max <- glm.lst(fm_max)
#write.csv(modlst_max,file ="Comparative_analysis_max.csv",row.names=F)

modlst_avg <- glm.lst(fm_avg) ## all models
write.csv(modlst_avg,file ="Comparative_analysis_avg.csv",row.names=F)

modlst_avg <- glm.lst(fm_avg) ## all models
write.csv(modlst_avg,file ="Comparative_analysis_avg_norandom.csv",row.names=F)

### aic 2

#min.AIC <- min(sapply(fm_total,AICc)) ## grab models with AIC less than 2
#min.modlst_total<-fm_total[sapply(fm_total,AICc)<(min.AIC+2)]

#min.AIC <- min(sapply(fm_max,AICc))
#min.modlst_max<-fm_max[sapply(fm_max,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_avg,AICc))
min.modlst_avg<-fm_avg[sapply(fm_avg,AICc)<(min.AIC+2)]

#min.modlst_total.avg<-model.avg(min.modlst_total) ## do model averaging
#min.modlst_max.avg<-model.avg(min.modlst_max)
min.modlst_avg.avg<-model.avg(min.modlst_avg)

#output.modlst_total<-data.frame(min.modlst_total.avg$term.names,min.modlst_total.avg$avg.model)
#output.modlst_max<-data.frame(min.modlst_max.avg$term.names,min.modlst_max.avg$avg.model)
output.modlst_avg<-data.frame(min.modlst_avg.avg$term.names,min.modlst_avg.avg$avg.model)

#write.csv(output.modlst_max,file="output.modlst_max_aic2.csv")
#write.csv(output.modlst_total,file="output.modlst_total_aic2.csv")
write.csv(output.modlst_avg,file="output.modlst_avg_aic2.csv")

### run each of the models within aic 2 for total model to get means models (estimates of parallel and perp)
# saved in C:\Users\Kira Delmore\Dropbox\Haley and Kira's Comparative Analysis Extravaganza\Analysis\hedges\all traits as output.modlst_total_aic2_mean_models

a<-lmer(total_log ~ migration_category-1 + p_distance_log + (1 | family/sp1_genus),data=comp)
b<-lmer(total_log ~ overlap_proportion_log + p_distance_log + (1 | family/sp1_genus),data=comp)
c<-lmer(total_log ~ overlap_proportion_log + (1 | family/sp1_genus),data=comp)
d<-lmer(total_log ~ migration_category-1 + p_distance_log + mass_avg_log + (1 | family/sp1_genus),data=comp)
e<-lmer(total_log ~ migration_category-1 + breed_lat_avg_log + p_distance_log + (1 | family/sp1_genus),data=comp)
f<-lmer(total_log ~ migration_category-1 + overlap_proportion_log + p_distance_log + (1 | family/sp1_genus),data=comp)
g<-lmer(total_log ~ overlap_proportion_log + breed_lat_avg_log + p_distance_log + (1 | family/sp1_genus),data=comp)
h<-lmer(total_log ~ overlap_proportion_log + mass_avg_log + (1 | family/sp1_genus),data=comp)
j<-lmer(total_log ~ overlap_proportion_log + breed_lat_avg_log + (1 | family/sp1_genus),data=comp)

summary(a)
summary(b)
summary(c)
summary(d)
summary(e)
summary(f)
summary(g)
summary(h)
summary(j)

############ MAKING FIGURES

pdf("../Write Up//Hedges Results/Figure 1.pdf",height=6,width=4)
boxplot(split(comp$total_log,comp$migration_category),varwidth=TRUE,ylab="total trait divergence (log)",cex.lab=1.5,cex.axis=1.2)
boxplot(split(comp$max_log,comp$migration_category),varwidth=TRUE,ylab="maximum trait divergence (log)",cex.lab=1.5,cex.axis=1.2)
dev.off()

> by(comp$residuals_avg2_pdist,comp$migration_category,sd)
comp$migration_category: parallel
[1] 0.5506428
------------------------------------------------------------------------------------------ 
  comp$migration_category: perpendicular
[1] 0.5277391
> by(comp$residuals_avg2_pdist,comp$migration_category,mean)
comp$migration_category: parallel
[1] 0.265395
------------------------------------------------------------------------------------------ 
comp$migration_category: perpendicular
[1] -0.1680835

> 0.5506428/(19^0.5)
[1] 0.1263261
> 0.5277391/(30^0.5)
[1] 0.09635154

pdf("../Write Up//Hedges Results/Figure 1 avg and resid.pdf",height=6,width=4)

stripchart(comp$residuals_avg2_pdist~comp$migration_category,cex.lab=1.5,cex.axis=1.2,pch=16,method="jitter",vertical=TRUE,xlim=c(0.5,2.8),ylab="average trait divergence")
points(c(1.1,2.1)+0.3,tapply(comp$residuals_avg2_pdist,comp$migration_category,mean),pch=16)
lines(c(1.1,1.1)+0.3,c((0.2654+0.13),(0.2654-0.13)))
arrows(1.4,(0.2654+0.13),1.4,(0.2654-0.13),angle=90,code=3,length=0.05)
lines(c(2.1,2.1)+0.3,c((-0.168+0.096),(-0.168-0.096)))
arrows(2.4,(-0.168+0.096),2.4,(-0.168-0.096),angle=90,code=3,length=0.05)

> by(comp$max_log,comp$migration_category,sd)
comp$migration_category: parallel
[1] 0.6254976
--------------------------------------------------------------------------------- 
  comp$migration_category: perpendicular
[1] 0.8301626
> by(comp$max_log,comp$migration_category,mean)
comp$migration_category: parallel
[1] 1.794908
--------------------------------------------------------------------------------- 
  comp$migration_category: perpendicular
[1] 1.416044

stripchart(comp$max_log~comp$migration_category,cex.lab=1.5,cex.axis=1.2,pch=16,method="jitter",vertical=TRUE,xlim=c(0.5,2.8),ylab="total trait divergence (log)")
points(c(1.1,2.1)+0.3,tapply(comp$max_log,comp$migration_category,mean),pch=16)
lines(c(1.1,1.1)+0.3,c(1.16941,2.420406))
arrows(1.4,1.16941,1.4,2.420406,angle=90,code=3,length=0.05)
lines(c(2.1,2.1)+0.3,c(0.5858814,2.246207))
arrows(2.4,0.5858814,2.4,2.246207,angle=90,code=3,length=0.05)

dev.off()


####MUMIN
Global_Personal.Use<-lm(avg_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log,data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$avg.model # parameter estimates and SEs from average model

Global_Personal.Use.random<-lmer(avg_log ~ (1| family/sp1_genus)+migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log,data=comp,na.action = na.pass)
All_models_Personal.Use.random<-dredge(Global_Personal.Use.random, extra = "R^2") # all possible combinations of fixed effects
Sub_All_models_Personal.Use.random<-subset(All_models_Personal.Use.random, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use.random<-model.avg(All_models_Personal.Use.random, subset=delta<2) # model averaging
Avg.Personal.Use.random$avg.model # parameter estimates and SEs from average model
