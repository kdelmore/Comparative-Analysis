# checking_into_geography
kira_delmore  
`r Sys.Date()`  

Get things ready

```r
setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
library(EvoRAG)
library(car)
evodat<-read.csv("../working files/pair_hedges_avg_jul24.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

## transform avg variables
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10

## set trait for analyses
evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10
```

FULL DATASET AND _NULL MODELS

a) Run model allowing no separate rates

```r
models = c("BM_null", "OU_null")
e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs
#plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)
```

```
##                            BM_null      OU_null
## logLik                18.491938644  23.71689259
## n                      1.000000000   2.00000000
## AIC                  -34.983877287 -43.43378518
## AICc                 -34.898770904 -43.17291561
## b1                     0.006134345   0.01752217
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.61845964
## a1_slope                        NA           NA
## a2                              NA           NA
## a2_slope                        NA           NA
## Quadratic_c                     NA           NA
## Quadratic_b                     NA           NA
## Quadratic_a                     NA           NA
## nlm_termination_code   1.000000000   1.00000000
```

b) Run model allowing separate rates

```r
## parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
t2<-evodat_parallel$p_distance_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters
```

```
##                           BM_null    OU_null
## logLik                3.135422112  6.7129518
## n                     1.000000000  2.0000000
## AIC                  -4.270844224 -9.4259036
## AICc                 -4.035550107 -8.6759036
## b1                    0.009749965  0.0404328
## b1_slope                       NA         NA
## breakpoint                     NA         NA
## b2                             NA         NA
## b2_slope                       NA         NA
## quadratic_term                 NA         NA
## a1                             NA  1.1825905
## a1_slope                       NA         NA
## a2                             NA         NA
## a2_slope                       NA         NA
## Quadratic_c                    NA         NA
## Quadratic_b                    NA         NA
## Quadratic_a                    NA         NA
## nlm_termination_code  1.000000000  1.0000000
```

```r
## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$p_distance_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null       OU_null
## logLik                17.963671013  18.919184256
## n                      1.000000000   2.000000000
## AIC                  -33.927342026 -33.838368513
## AICc                 -33.784484883 -33.393924068
## b1                     0.003844453   0.006061431
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.183192865
## a1_slope                        NA            NA
## a2                              NA            NA
## a2_slope                        NA            NA
## Quadratic_c                     NA            NA
## Quadratic_b                     NA            NA
## Quadratic_a                     NA            NA
## nlm_termination_code   1.000000000   1.000000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -40.19819
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -47.26427
```

LIMIT DATASET TO 36 NON-SYM PAIRS AND RUN _NULL MODELS


```r
evodat=subset(evodat,evodat$visual_official_jason!="sym")
models = c("BM_null", "OU_null")
```

a) Run model allowing no separate rates

```r
e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs
#plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)
```

```
##                            BM_null     OU_null
## logLik                17.942066875  26.1063276
## n                      1.000000000   2.0000000
## AIC                  -33.884133749 -48.2126552
## AICc                 -33.766486691 -47.8490188
## b1                     0.006298839   0.0566004
## b1_slope                        NA          NA
## breakpoint                      NA          NA
## b2                              NA          NA
## b2_slope                        NA          NA
## quadratic_term                  NA          NA
## a1                              NA   3.9760407
## a1_slope                        NA          NA
## a2                              NA          NA
## a2_slope                        NA          NA
## Quadratic_c                     NA          NA
## Quadratic_b                     NA          NA
## Quadratic_a                     NA          NA
## nlm_termination_code   1.000000000   1.0000000
```

b) Run model allowing separate rates

```r
## parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
t2<-evodat_parallel$p_distance_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters
```

```
##                          BM_null     OU_null
## logLik                3.22849769  5.41150249
## n                     1.00000000  2.00000000
## AIC                  -4.45699537 -6.82300498
## AICc                 -3.79032870 -4.42300498
## b1                    0.01537471  0.07334116
## b1_slope                      NA          NA
## breakpoint                    NA          NA
## b2                            NA          NA
## b2_slope                      NA          NA
## quadratic_term                NA          NA
## a1                            NA  4.32869282
## a1_slope                      NA          NA
## a2                            NA          NA
## a2_slope                      NA          NA
## Quadratic_c                   NA          NA
## Quadratic_b                   NA          NA
## Quadratic_a                   NA          NA
## nlm_termination_code  1.00000000  1.00000000
```

```r
## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$p_distance_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null     OU_null
## logLik                18.570910397  21.1513273
## n                      1.000000000   2.0000000
## AIC                  -35.141820794 -38.3026546
## AICc                 -34.987974640 -37.8226546
## b1                     0.003705733   0.0101522
## b1_slope                        NA          NA
## breakpoint                      NA          NA
## b2                              NA          NA
## b2_slope                        NA          NA
## quadratic_term                  NA          NA
## a1                              NA   0.6308815
## a1_slope                        NA          NA
## a2                              NA          NA
## a2_slope                        NA          NA
## Quadratic_c                     NA          NA
## Quadratic_b                     NA          NA
## Quadratic_a                     NA          NA
## nlm_termination_code   1.000000000   1.0000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -41.59882
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -49.12566
```

LIMIT DATASET TO 36 NON-SYM PAIRS AND RUN _LINEAR* MODELS


```r
evodat$migration_category<-recode(evodat$migration_category,"'perpendicular'=0;'parallel'=1")
models = c("BM_null", "BM_linear", "OU_null", "OU_linear","OU_linear_beta")
```

a) Run model allowing no separate rates

```r
e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs
#plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)
```

```
##                            BM_null     BM_linear     OU_null     OU_linear
## logLik                17.942066875  1.986398e+01  26.1063276  27.386944864
## n                      1.000000000  2.000000e+00   2.0000000   4.000000000
## AIC                  -33.884133749 -3.572796e+01 -48.2126552 -46.773889728
## AICc                 -33.766486691 -3.536432e+01 -47.8490188 -45.483567147
## b1                     0.006298839  2.595573e-03   0.0566004   0.010093417
## b1_slope                        NA  1.669326e-04          NA   0.001322327
## breakpoint                      NA            NA          NA            NA
## b2                              NA            NA          NA            NA
## b2_slope                        NA            NA          NA            NA
## quadratic_term                  NA            NA          NA            NA
## a1                              NA            NA   3.9760407   2.190319642
## a1_slope                        NA            NA          NA   0.013289628
## a2                              NA            NA          NA            NA
## a2_slope                        NA            NA          NA            NA
## Quadratic_c                     NA            NA          NA            NA
## Quadratic_b                     NA            NA          NA            NA
## Quadratic_a                     NA            NA          NA            NA
## nlm_termination_code   1.000000000  1.000000e+00   1.0000000   1.000000000
##                      OU_linear_beta
## logLik                  27.37717028
## n                        3.00000000
## AIC                    -48.75434056
## AICc                   -48.00434056
## b1                       0.01218008
## b1_slope                 0.00115506
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       2.40298547
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     1.00000000
```

b) Run model allowing separate rates

```r
e9 <- evodat$trait
t9<-evodat$p_distance_100
g9<-evodat$migration_category
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear     OU_null    OU_linear
## logLik                17.942066875  21.799408082  26.1063276  26.56282977
## n                      1.000000000   2.000000000   2.0000000   4.00000000
## AIC                  -33.884133749 -39.598816164 -48.2126552 -45.12565955
## AICc                 -33.766486691 -39.235179800 -47.8490188 -43.83533697
## b1                     0.006298839   0.003705738   0.0566004   0.01015237
## b1_slope                        NA   0.011668753          NA   0.06318326
## breakpoint                      NA            NA          NA           NA
## b2                              NA            NA          NA           NA
## b2_slope                        NA            NA          NA           NA
## quadratic_term                  NA            NA          NA           NA
## a1                              NA            NA   3.9760407   0.63089649
## a1_slope                        NA            NA          NA   3.69741569
## a2                              NA            NA          NA           NA
## a2_slope                        NA            NA          NA           NA
## Quadratic_c                     NA            NA          NA           NA
## Quadratic_b                     NA            NA          NA           NA
## Quadratic_a                     NA            NA          NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.0000000   1.00000000
##                      OU_linear_beta
## logLik                  26.24521447
## n                        3.00000000
## AIC                    -46.49042894
## AICc                   -45.74042894
## b1                       0.03158472
## b1_slope                 0.01275381
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       2.31498011
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     1.00000000
```

GO BACK TO FULL DATASET AND USE OVERLAP PROPORTION TO EXAMINE GEOGRAPHIES INFLUENCE


```r
setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
evodat<-read.csv("../working files/pair_hedges_avg_jul24.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

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
```

a) Run model allowing no separate rates (same as first set of models but with full dataset)

```r
models = c("BM_null", "OU_null")

e1<-evodat$trait
t1<-evodat$p_distance_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs
#plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)
```

```
##                            BM_null      OU_null
## logLik                18.491938644  23.71689259
## n                      1.000000000   2.00000000
## AIC                  -34.983877287 -43.43378518
## AICc                 -34.898770904 -43.17291561
## b1                     0.006134345   0.01752217
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.61845964
## a1_slope                        NA           NA
## a2                              NA           NA
## a2_slope                        NA           NA
## Quadratic_c                     NA           NA
## Quadratic_b                     NA           NA
## Quadratic_a                     NA           NA
## nlm_termination_code   1.000000000   1.00000000
```

b) Run model allowing separate rates (again, same as first set of models but with full dataset)

```r
## parallels
evodat_parallel<-subset(evodat,evodat$migration_category=="parallel")
e2 <- evodat_parallel$trait
t2<-evodat_parallel$p_distance_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters
```

```
##                           BM_null    OU_null
## logLik                3.135422112  6.7129518
## n                     1.000000000  2.0000000
## AIC                  -4.270844224 -9.4259036
## AICc                 -4.035550107 -8.6759036
## b1                    0.009749965  0.0404328
## b1_slope                       NA         NA
## breakpoint                     NA         NA
## b2                             NA         NA
## b2_slope                       NA         NA
## quadratic_term                 NA         NA
## a1                             NA  1.1825905
## a1_slope                       NA         NA
## a2                             NA         NA
## a2_slope                       NA         NA
## Quadratic_c                    NA         NA
## Quadratic_b                    NA         NA
## Quadratic_a                    NA         NA
## nlm_termination_code  1.000000000  1.0000000
```

```r
## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$p_distance_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null       OU_null
## logLik                17.963671013  18.919184256
## n                      1.000000000   2.000000000
## AIC                  -33.927342026 -33.838368513
## AICc                 -33.784484883 -33.393924068
## b1                     0.003844453   0.006061431
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.183192865
## a1_slope                        NA            NA
## a2                              NA            NA
## a2_slope                        NA            NA
## Quadratic_c                     NA            NA
## Quadratic_b                     NA            NA
## Quadratic_a                     NA            NA
## nlm_termination_code   1.000000000   1.000000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -40.19819
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -47.26427
```

c) Run model with overlap proportion as continuous variable

```r
models = c("BM_null", "BM_linear", "OU_null", "OU_linear","OU_linear_beta")
e9 <- evodat$trait
t9<-evodat$p_distance_100
g9<-evodat$overlap_proportion
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null    OU_linear
## logLik                18.491938644  18.603637482  23.71689259  27.12072781
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -34.983877287 -33.207274964 -43.43378518 -46.24145561
## AICc                 -34.898770904 -32.946405398 -43.17291561 -45.33236470
## b1                     0.006134345   0.006507582   0.01752217   0.02970956
## b1_slope                        NA  -0.002217379           NA   0.95543623
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.61845964   2.55591080
## a1_slope                        NA            NA           NA  18.37173985
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                  26.49334735
## n                        3.00000000
## AIC                    -46.98669471
## AICc                   -46.45336137
## b1                       0.04900735
## b1_slope                 0.28737025
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       3.84613980
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     1.00000000
```

d) Run model with separate rates for migration category AND including overlap proportion

```r
## parallels
evodat_parallel_patry<-subset(evodat,evodat$migration_category=="parallel")
e9 <- evodat_parallel_patry$trait
t9<-evodat_parallel_patry$p_distance_100
g9<-evodat_parallel_patry$overlap_proportion
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                           BM_null   BM_linear    OU_null   OU_linear
## logLik                3.135422112  4.43478227  6.7129518  6.83470288
## n                     1.000000000  2.00000000  2.0000000  4.00000000
## AIC                  -4.270844224 -4.86956455 -9.4259036 -5.66940575
## AICc                 -4.035550107 -4.11956455 -8.6759036 -2.81226289
## b1                    0.009749965  0.01349296  0.0404328  0.02285949
## b1_slope                       NA -0.01160964         NA  1.71084606
## breakpoint                     NA          NA         NA          NA
## b2                             NA          NA         NA          NA
## b2_slope                       NA          NA         NA          NA
## quadratic_term                 NA          NA         NA          NA
## a1                             NA          NA  1.1825905  0.79014304
## a1_slope                       NA          NA         NA 50.81315127
## a2                             NA          NA         NA          NA
## a2_slope                       NA          NA         NA          NA
## Quadratic_c                    NA          NA         NA          NA
## Quadratic_b                    NA          NA         NA          NA
## Quadratic_a                    NA          NA         NA          NA
## nlm_termination_code  1.000000000  1.00000000  1.0000000  1.00000000
##                      OU_linear_beta
## logLik                  6.715631304
## n                       3.000000000
## AIC                    -7.431262608
## AICc                   -5.831262608
## b1                      0.042314247
## b1_slope                0.004445286
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      1.300700892
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    1.000000000
```

```r
# ci for parallels
# intercept_beta_par<-as.numeric(fit_par_patry[5,4])
# slope_beta_par<-as.numeric(fit_par_patry[6,4])
# intercept_alpha_par<-as.numeric(fit_par_patry[11,4])
# slope_alpha_par<-as.numeric(fit_par_patry[12,4])
# parameters_par=c(intercept_beta_par, slope_beta_par, intercept_alpha_par, slope_alpha_par)
# set.seed(seed = 3)
# parR <- bootstrap.test(e9,t9,g9,model="OU_linear", parameters_par, meserr1=0, meserr2=0, breakpoint = "NULL", N = c(1000), starting=NULL)
# parR$summary

## perpendiculars
evodat_perp_patry<-subset(evodat,evodat$migration_category=="perpendicular")
e10 <- evodat_perp_patry$trait
t10 <-evodat_perp_patry$p_distance_100
g10 <-evodat_perp_patry$overlap_proportion
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear       OU_null
## logLik                17.963671013  18.081605160  18.919184256
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -33.927342026 -32.163210320 -33.838368513
## AICc                 -33.784484883 -31.718765876 -33.393924068
## b1                     0.003844453   0.003664990   0.006061431
## b1_slope                        NA   0.003689401            NA
## breakpoint                      NA            NA            NA
## b2                              NA            NA            NA
## b2_slope                        NA            NA            NA
## quadratic_term                  NA            NA            NA
## a1                              NA            NA   0.183192865
## a1_slope                        NA            NA            NA
## a2                              NA            NA            NA
## a2_slope                        NA            NA            NA
## Quadratic_c                     NA            NA            NA
## Quadratic_b                     NA            NA            NA
## Quadratic_a                     NA            NA            NA
## nlm_termination_code   1.000000000   1.000000000   1.000000000
##                         OU_linear OU_linear_beta
## logLik                21.60788612    21.60613586
## n                      4.00000000     3.00000000
## AIC                  -35.21577223   -37.21227173
## AICc                 -33.61577223   -36.28919480
## b1                     0.02270884     0.02394112
## b1_slope               0.40517413     0.43624641
## breakpoint                     NA             NA
## b2                             NA             NA
## b2_slope                       NA             NA
## quadratic_term                 NA             NA
## a1                     2.22184685     2.35355651
## a1_slope              -0.10000008             NA
## a2                             NA             NA
## a2_slope                       NA             NA
## Quadratic_c                    NA             NA
## Quadratic_b                    NA             NA
## Quadratic_a                    NA             NA
## nlm_termination_code   1.00000000     1.00000000
```

```r
# ci for perpendiculars
# intercept_beta_perp<-as.numeric(fit_perp_patry[5,4])
# slope_beta_perp<-as.numeric(fit_perp_patry[6,4])
# intercept_alpha_perp<-as.numeric(fit_perp_patry[11,4])
# slope_alpha_perp<-as.numeric(fit_perp_patry[12,4])
# parameters_perp=c(intercept_beta_perp, slope_beta_perp, intercept_alpha_perp, slope_alpha_perp)
# set.seed(seed = 3)
# perpR <- bootstrap.test(e10,t10,g10,model="OU_linear", parameters_perp, meserr1=0, meserr2=0, breakpoint = "NULL", N = c(1000), starting=NULL)
# perpR$summary

## calculate AIC for perp/par with patry and using _linear models
logLike_BM<-fit_par_patry[1,2]+fit_perp_patry[1,2]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -43.03277
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -52.88518
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -52.64353
```

