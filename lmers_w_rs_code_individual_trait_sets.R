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

# comp$max_morph_log <- log(comp$max_morph)
# comp$total_morph_log <- log(comp$total_morph)
comp$avg_morph_log <- log(comp$avg_morph)
# comp$max_song_log <- log(comp$max_song)
# comp$total_song_log <- log(comp$total_song)
comp$avg_song_log <- log(comp$avg_song)
# comp$max_colour_log <- log(comp$max_colour)
# comp$total_colour_log <- log(comp$total_colour)
comp$avg_colour_log <- log(comp$avg_colour)

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

#total_morph
form <- sapply(id,function(i)
  paste("total_morph_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_total_morph <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#max_morph
form <- sapply(id,function(i)
  paste("max_morph_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_max_morph <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#avg_morph
form <- sapply(id,function(i)
  paste("avg_morph_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_avg_morph <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#total_song
form <- sapply(id,function(i)
  paste("total_song_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_total_song <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#max_song
form <- sapply(id,function(i)
  paste("max_song_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_max_song <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#avg_song
form <- sapply(id,function(i)
  paste("avg_song_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_avg_song <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#total_plum
form <- sapply(id,function(i)
  paste("total_colour_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_total_colour <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#max_plum
form <- sapply(id,function(i)
  paste("max_colour_log ~",paste(covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_max_colour <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 

#avg_plum
form <- sapply(id,function(i)
  paste("avg_colour_log ~ ",paste (covar[i],collapse="+"), "+ (1| family/sp1_genus)", sep="") 
)

fm_avg_colour <- lapply(form,function(i) try(lmer(as.formula(i),data=comp))) 


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

modlst_total <- glm.lst(fm_total_morph) ## all models
write.csv(modlst_total,file ="Comparative_analysis_total_morph.csv",row.names=F)

modlst_max <- glm.lst(fm_max_morph)
write.csv(modlst_max,file ="Comparative_analysis_max_morph.csv",row.names=F)

modlst_avg <- glm.lst(fm_avg_morph)
write.csv(modlst_avg,file ="Comparative_analysis_avg_morph.csv",row.names=F)

modlst_total <- glm.lst(fm_total_song) ## all models
write.csv(modlst_total,file ="Comparative_analysis_total_song.csv",row.names=F)

modlst_max <- glm.lst(fm_max_song)
write.csv(modlst_max,file ="Comparative_analysis_max_song.csv",row.names=F)

modlst_avg <- glm.lst(fm_avg_song)
write.csv(modlst_avg,file ="Comparative_analysis_avg_song.csv",row.names=F)

modlst_total <- glm.lst(fm_total_colour) ## all models
write.csv(modlst_total,file ="Comparative_analysis_total_colour.csv",row.names=F)

modlst_max <- glm.lst(fm_max_colour)
write.csv(modlst_max,file ="Comparative_analysis_max_colour.csv",row.names=F)

modlst_avg <- glm.lst(fm_avg_colour)
write.csv(modlst_avg,file ="Comparative_analysis_avg_colour.csv",row.names=F)

### aic 2

min.AIC <- min(sapply(fm_total_morph,AICc)) ## grab models with AIC less than 2
min.modlst_total_morph<-fm_total_morph[sapply(fm_total_morph,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_max_morph,AICc))
min.modlst_max_morph<-fm_max_morph[sapply(fm_max_morph,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_avg_morph,AICc))
min.modlst_avg_morph<-fm_avg_morph[sapply(fm_avg_morph,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_total_song,AICc)) ## grab models with AIC less than 2
min.modlst_total_song<-fm_total_song[sapply(fm_total_song,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_max_song,AICc))
min.modlst_max_song<-fm_max_song[sapply(fm_max_song,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_avg_song,AICc))
min.modlst_avg_song<-fm_avg_song[sapply(fm_avg_song,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_total_colour,AICc)) ## grab models with AIC less than 2
min.modlst_total_colour<-fm_total_colour[sapply(fm_total_colour,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_max_colour,AICc))
min.modlst_max_colour<-fm_max_colour[sapply(fm_max_colour,AICc)<(min.AIC+2)]

min.AIC <- min(sapply(fm_avg_colour,AICc))
min.modlst_avg_colour<-fm_avg_colour[sapply(fm_avg_colour,AICc)<(min.AIC+2)]



min.modlst_total_morph.avg<-model.avg(min.modlst_total_morph) ## do model averaging
min.modlst_max_morph.avg<-model.avg(min.modlst_max_morph)
min.modlst_avg_morph.avg<-model.avg(min.modlst_avg_morph)

min.modlst_total_song.avg<-model.avg(min.modlst_total_song) ## do model averaging
min.modlst_max_song.avg<-model.avg(min.modlst_max_song)
min.modlst_avg_song.avg<-model.avg(min.modlst_avg_song)

min.modlst_total_colour.avg<-model.avg(min.modlst_total_colour) ## do model averaging
min.modlst_max_colour.avg<-model.avg(min.modlst_max_colour)
min.modlst_avg_colour.avg<-model.avg(min.modlst_avg_colour)



output.modlst_total_morph<-data.frame(min.modlst_total_morph.avg$term.names,min.modlst_total_morph.avg$avg.model)
output.modlst_max_morph<-data.frame(min.modlst_max_morph.avg$term.names,min.modlst_max_morph.avg$avg.model)
output.modlst_avg_morph<-data.frame(min.modlst_avg_morph.avg$term.names,min.modlst_avg_morph.avg$avg.model)

output.modlst_total_song<-data.frame(min.modlst_total_song.avg$term.names,min.modlst_total_song.avg$avg.model)
output.modlst_max_song<-data.frame(min.modlst_max_song.avg$term.names,min.modlst_max_song.avg$avg.model)
output.modlst_avg_song<-data.frame(min.modlst_avg_song.avg$term.names,min.modlst_avg_song.avg$avg.model)

output.modlst_total_colour<-data.frame(min.modlst_total_colour.avg$term.names,min.modlst_total_colour.avg$avg.model)
output.modlst_max_colour<-data.frame(min.modlst_max_colour.avg$term.names,min.modlst_max_colour.avg$avg.model)
output.modlst_avg_colour<-data.frame(min.modlst_avg_colour.avg$term.names,min.modlst_avg_colour.avg$avg.model)



write.csv(output.modlst_max_morph,file="output.modlst_max_morph_aic2.csv")
write.csv(output.modlst_total_morph,file="output.modlst_total_morph_aic2.csv")
write.csv(output.modlst_avg_morph,file="output.modlst_avg_morph_aic2.csv")

write.csv(output.modlst_max_song,file="output.modlst_max_song_aic2.csv")
write.csv(output.modlst_total_song,file="output.modlst_total_song_aic2.csv")
write.csv(output.modlst_avg_song,file="output.modlst_avg_song_aic2.csv")

write.csv(output.modlst_max_colour,file="output.modlst_max_colour_aic2.csv")
write.csv(output.modlst_total_colour,file="output.modlst_total_colour_aic2.csv")
write.csv(output.modlst_avg_colour,file="output.modlst_avg_colour_aic2.csv")


### running the models without model averaging, use lme so you can get pvalues and try out the -1 option for calculating estimates

a <- lme (total_morph_log ~ migration_category + p_distance_log + overlap_proportion_log + mass_avg_log + breed_lat_avg_log + distance_avg_log, random = ~1|family/sp1_genus, data=comp)
b <- lme (total_song_log ~ migration_category + p_distance_log + overlap_proportion_log + mass_avg_log + breed_lat_avg_log + distance_avg_log, random = ~1|family/sp1_genus, data=comp)
c <- lme (total_colour_log ~ migration_category + p_distance_log + overlap_proportion_log + mass_avg_log + breed_lat_avg_log + distance_avg_log, random = ~1|family/sp1_genus, data=comp)

> anova(a)
numDF denDF  F-value p-value
(Intercept)                1    18 3.619490  0.0732
migration_category         1    18 0.376366  0.5472
p_distance_log             1    18 8.150594  0.0105
overlap_proportion_log     1    18 0.927873  0.3482
mass_avg_log               1    18 6.095732  0.0238
breed_lat_avg_log          1    18 4.355099  0.0514
distance_avg_log           1    18 1.261237  0.2762
> anova(b)
numDF denDF  F-value p-value
(Intercept)                1    18 58.67998  <.0001
migration_category         1    18  3.39693  0.0818
p_distance_log             1    18  6.96960  0.0166
overlap_proportion_log     1    18  2.55834  0.1271
mass_avg_log               1    18  2.62832  0.1224
breed_lat_avg_log          1    18  2.46078  0.1341
distance_avg_log           1    18  0.02071  0.8872
> anova(c)
numDF denDF   F-value p-value
(Intercept)                1    18 108.06585  <.0001
migration_category         1    18   5.23305  0.0345
p_distance_log             1    18   9.59899  0.0062
overlap_proportion_log     1    18   0.47262  0.5005
mass_avg_log               1    18   0.40860  0.5307
breed_lat_avg_log          1    18   0.00080  0.9777
distance_avg_log           1    18   3.10438  0.0951