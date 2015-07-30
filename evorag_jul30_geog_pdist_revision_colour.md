# evorag_jul30_geog_pdist_revision
kira_delmore  
`r Sys.Date()`  

Get things ready

```r
setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
library(EvoRAG)
library(car)
evodat<-read.csv("../working files/pair_hedges_avg_jul30.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

## transform avg variables
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10

## set trait for analyses
#evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10
```

FULL DATASET AND USE OVERLAP PROPORTION TO EXAMINE GEOGRAPHIES INFLUENCE

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
## logLik                 9.979526259  14.08798799
## n                      1.000000000   2.00000000
## AIC                  -17.959052517 -24.17597599
## AICc                 -17.873946134 -23.91510642
## b1                     0.008682842   0.01991003
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.42679730
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
##                          BM_null    OU_null
## logLik               -0.31193100 1.93442469
## n                     1.00000000 2.00000000
## AIC                   2.62386201 0.13115061
## AICc                  2.85915612 0.88115061
## b1                    0.01401521 0.03730289
## b1_slope                      NA         NA
## breakpoint                    NA         NA
## b2                            NA         NA
## b2_slope                      NA         NA
## quadratic_term                NA         NA
## a1                            NA 0.57014268
## a1_slope                      NA         NA
## a2                            NA         NA
## a2_slope                      NA         NA
## Quadratic_c                   NA         NA
## Quadratic_b                   NA         NA
## Quadratic_a                   NA         NA
## nlm_termination_code  1.00000000 1.00000000
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
## logLik                13.131483720  14.499359091
## n                      1.000000000   2.000000000
## AIC                  -24.262967439 -24.998718181
## AICc                 -24.120110296 -24.554273737
## b1                     0.005305676   0.009246617
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.239878507
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
## [1] -23.63911
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -28.86757
```

c) Run model with overlap proportion as continuous variable

```r
models = c("BM_null", "BM_linear", "OU_null", "OU_linear","OU_linear_beta")
e9 <- evodat$trait
t9<-evodat$p_distance_100
g9<-evodat$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null    OU_linear
## logLik                 9.979526259  10.137055660  14.08798799  17.36991928
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -17.959052517 -16.274111320 -24.17597599 -26.73983856
## AICc                 -17.873946134 -16.013241755 -23.91510642 -25.83074765
## b1                     0.008682842   0.007898427   0.01991003   0.03709434
## b1_slope                        NA   0.004312602           NA   0.18854299
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.42679730   1.99255035
## a1_slope                        NA            NA           NA  -0.10009999
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                  17.36752080
## n                        3.00000000
## AIC                    -28.73504159
## AICc                   -28.20170826
## b1                       0.03728446
## b1_slope                 0.19778439
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       2.01292165
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
g9<-evodat_parallel_patry$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                          BM_null    BM_linear    OU_null  OU_linear
## logLik               -0.31193100 -0.086995733 1.93442469 3.16352363
## n                     1.00000000  2.000000000 2.00000000 4.00000000
## AIC                   2.62386201  4.173991467 0.13115061 1.67295273
## AICc                  2.85915612  4.923991467 0.88115061 4.53009559
## b1                    0.01401521  0.017425325 0.03730289 0.05783331
## b1_slope                      NA -0.009482838         NA 0.63637181
## breakpoint                    NA           NA         NA         NA
## b2                            NA           NA         NA         NA
## b2_slope                      NA           NA         NA         NA
## quadratic_term                NA           NA         NA         NA
## a1                            NA           NA 0.57014268 4.27899244
## a1_slope                      NA           NA         NA 2.04660272
## a2                            NA           NA         NA         NA
## a2_slope                      NA           NA         NA         NA
## Quadratic_c                   NA           NA         NA         NA
## Quadratic_b                   NA           NA         NA         NA
## Quadratic_a                   NA           NA         NA         NA
## nlm_termination_code  1.00000000  1.000000000 1.00000000 1.00000000
##                      OU_linear_beta
## logLik                   3.14580208
## n                        3.00000000
## AIC                     -0.29160417
## AICc                     1.30839583
## b1                       0.06308294
## b1_slope                 0.48502733
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       4.35700735
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     1.00000000
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
g10 <-evodat_perp_patry$overlap_proportion_hz_info
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear       OU_null
## logLik                13.131483720  1.313354e+01  14.499359091
## n                      1.000000000  2.000000e+00   2.000000000
## AIC                  -24.262967439 -2.226708e+01 -24.998718181
## AICc                 -24.120110296 -2.182263e+01 -24.554273737
## b1                     0.005305676  5.245635e-03   0.009246617
## b1_slope                        NA  8.617601e-04            NA
## breakpoint                      NA            NA            NA
## b2                              NA            NA            NA
## b2_slope                        NA            NA            NA
## quadratic_term                  NA            NA            NA
## a1                              NA            NA   0.239878507
## a1_slope                        NA            NA            NA
## a2                              NA            NA            NA
## a2_slope                        NA            NA            NA
## Quadratic_c                     NA            NA            NA
## Quadratic_b                     NA            NA            NA
## Quadratic_a                     NA            NA            NA
## nlm_termination_code   1.000000000  1.000000e+00   1.000000000
##                          OU_linear OU_linear_beta
## logLik                15.557631017   14.970034695
## n                      4.000000000    3.000000000
## AIC                  -23.115262034  -23.940069390
## AICc                 -21.515262034  -23.016992467
## b1                     0.003110823    0.009075579
## b1_slope               0.306253840    0.034026675
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.039264923    0.346202564
## a1_slope               9.672367011             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
## [1] -24.09309
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -33.44231
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -32.23167
```

RERUN MODELS WITH DIFFERENT PDIST

Get things ready

```r
evodat["speed_coi_100"]<-evodat$speed_coi*100
evodat=subset(evodat,evodat$mtdna_type!="no")
```

a) Run model allowing no separate rates (same as first set of models but with full dataset)

```r
models = c("BM_null", "OU_null")

e1<-evodat$trait
t1<-evodat$speed_coi_100
l1<-evodat$mass_avg #dummy
fit_all<-model.test.sisters(e1,t1,l1,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL) #model for all pairs
#plot(t1,e1,xlim=c(0,10),ylim=c(0,1),main="all")
print(fit_all)
```

```
##                            BM_null      OU_null
## logLik                 8.968321114  12.32346190
## n                      1.000000000   2.00000000
## AIC                  -15.936642228 -20.64692381
## AICc                 -15.845733138 -20.36785404
## b1                     0.009062583   0.01879389
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.36141352
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
t2<-evodat_parallel$speed_coi_100
l2<-evodat_parallel$mass_avg 
fit_par <- model.test.sisters(e2,t2,l2,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t2,e2,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par) #View model parameters
```

```
##                          BM_null    OU_null
## logLik               -0.30218862 1.91774533
## n                     1.00000000 2.00000000
## AIC                   2.60437724 0.16450934
## AICc                  2.83967136 0.91450934
## b1                    0.01394561 0.03689662
## b1_slope                      NA         NA
## breakpoint                    NA         NA
## b2                            NA         NA
## b2_slope                      NA         NA
## quadratic_term                NA         NA
## a1                            NA 0.56314018
## a1_slope                      NA         NA
## a2                            NA         NA
## a2_slope                      NA         NA
## Quadratic_c                   NA         NA
## Quadratic_b                   NA         NA
## Quadratic_a                   NA         NA
## nlm_termination_code  1.00000000 1.00000000
```

```r
## perpendiculars
evodat_perp<-subset(evodat,evodat$migration_category=="perpendicular")
e3 <- evodat_perp$trait
t3<-evodat_perp$speed_coi_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null      OU_null
## logLik                11.611193974  12.46397285
## n                      1.000000000   2.00000000
## AIC                  -21.222387948 -20.92794570
## AICc                 -21.062387948 -20.42794570
## b1                     0.005626381   0.00866642
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.17936502
## a1_slope                        NA           NA
## a2                              NA           NA
## a2_slope                        NA           NA
## Quadratic_c                     NA           NA
## Quadratic_b                     NA           NA
## Quadratic_a                     NA           NA
## nlm_termination_code   1.000000000   1.00000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -20.61801
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -24.76344
```

c) Run model with overlap proportion as continuous variable

```r
models = c("BM_null", "BM_linear", "OU_null", "OU_linear","OU_linear_beta")
e9 <- evodat$trait
t9<-evodat$speed_coi_100
g9<-evodat$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null    OU_linear
## logLik                 8.968321114   9.100963248  12.32346190  15.00818629
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -15.936642228 -14.201926496 -20.64692381 -22.01637258
## AICc                 -15.845733138 -13.922856729 -20.36785404 -21.04076283
## b1                     0.009062583   0.008273102   0.01879389   0.03035425
## b1_slope                        NA   0.004245922           NA   0.17519515
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.36141352   1.51644958
## a1_slope                        NA            NA           NA   0.24457060
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                  15.00706477
## n                        3.00000000
## AIC                    -24.01412955
## AICc                   -23.44270098
## b1                       0.03064883
## b1_slope                 0.15506157
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       1.50715889
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
t9<-evodat_parallel_patry$speed_coi_100
g9<-evodat_parallel_patry$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                          BM_null    BM_linear    OU_null  OU_linear
## logLik               -0.30218862 -0.086106737 1.91774533 3.15364917
## n                     1.00000000  2.000000000 2.00000000 4.00000000
## AIC                   2.60437724  4.172213474 0.16450934 1.69270167
## AICc                  2.83967136  4.922213474 0.91450934 4.54984452
## b1                    0.01394561  0.017282369 0.03689662 0.05871296
## b1_slope                      NA -0.009275045         NA 0.65200910
## breakpoint                    NA           NA         NA         NA
## b2                            NA           NA         NA         NA
## b2_slope                      NA           NA         NA         NA
## quadratic_term                NA           NA         NA         NA
## a1                            NA           NA 0.56314018 4.38229136
## a1_slope                      NA           NA         NA 2.08655580
## a2                            NA           NA         NA         NA
## a2_slope                      NA           NA         NA         NA
## Quadratic_c                   NA           NA         NA         NA
## Quadratic_b                   NA           NA         NA         NA
## Quadratic_a                   NA           NA         NA         NA
## nlm_termination_code  1.00000000  2.000000000 1.00000000 1.00000000
##                      OU_linear_beta
## logLik                   3.13578229
## n                        3.00000000
## AIC                     -0.27156457
## AICc                     1.32843543
## b1                       0.06419686
## b1_slope                 0.50014769
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       4.47937760
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     1.00000000
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
t10 <-evodat_perp_patry$speed_coi_100
g10 <-evodat_perp_patry$overlap_proportion_hz_info
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null
## logLik                11.611193974  11.642334244  12.46397285
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -21.222387948 -19.284668487 -20.92794570
## AICc                 -21.062387948 -18.784668487 -20.42794570
## b1                     0.005626381   0.005283309   0.00866642
## b1_slope                        NA   0.005502639           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.17936502
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                13.574225481   12.965944735
## n                      4.000000000    3.000000000
## AIC                  -19.148450962  -19.931889471
## AICc                 -17.330269144  -18.888411210
## b1                     0.003040887    0.007914132
## b1_slope               0.220860427    0.042594263
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.009127105    0.266357741
## a1_slope               5.672052176             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
## [1] -21.11246
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -29.45575
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -28.20345
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
##                            BM_null     BM_linear      OU_null
## logLik                 8.876729393  9.223940e+00  12.34387369
## n                      1.000000000  2.000000e+00   2.00000000
## AIC                  -15.753458787 -1.444788e+01 -20.68774738
## AICc                 -15.662549696 -1.416881e+01 -20.40867761
## b1                     0.009218373  7.447550e-03   0.01935826
## b1_slope                        NA  8.908550e-05           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.37411672
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000  2.000000e+00   1.00000000
##                          OU_linear OU_linear_beta
## logLik                1.268042e+01   1.246487e+01
## n                     4.000000e+00   3.000000e+00
## AIC                  -1.736084e+01  -1.892975e+01
## AICc                 -1.638523e+01  -1.835832e+01
## b1                    1.081817e-02   1.630653e-02
## b1_slope              4.764792e-04   1.239968e-04
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    1.026569e-01   3.536772e-01
## a1_slope              1.522100e-02             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code  1.000000e+00   1.000000e+00
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
##                            BM_null     BM_linear      OU_null
## logLik                 8.876729393  11.052547120  12.34387369
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -15.753458787 -18.105094240 -20.68774738
## AICc                 -15.662549696 -17.826024473 -20.40867761
## b1                     0.009218373   0.005842883   0.01935826
## b1_slope                        NA   0.008171853           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.37411672
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                14.276124696    14.00016782
## n                      4.000000000     3.00000000
## AIC                  -20.552249392   -22.00033564
## AICc                 -19.576639636   -21.42890707
## b1                     0.009268498     0.01196485
## b1_slope               0.028034380     0.01377534
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.198376011     0.32077946
## a1_slope               0.371766053             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000     1.00000000
```

LIMIT DATASET TO 36 NON-SYM PAIRS AND RUN _NULL MODELS


```r
setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
evodat<-read.csv("../working files/pair_hedges_avg_jul30.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

## transform avg variables
evodat["p_distance_100"]<-evodat$p_distance*100
evodat["avg_song10"]<-evodat$avg_song/10
evodat["avg_morph10"]<-evodat$avg_morph/10
evodat["avg_colour10"]<-evodat$avg_colour/10
evodat["avg_all10"]<-evodat$avg_2/10

## set trait for analyses
#evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10

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
##                            BM_null      OU_null
## logLik                12.899687138  18.96195986
## n                      1.000000000   2.00000000
## AIC                  -23.799374276 -33.92391972
## AICc                 -23.681727218 -33.56028336
## b1                     0.008335286   0.05099368
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   2.31416997
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
##                          BM_null     OU_null
## logLik                2.48227602  5.06065304
## n                     1.00000000  2.00000000
## AIC                  -2.96455205 -6.12130608
## AICc                 -2.29788538 -3.72130608
## b1                    0.01852794  0.08115564
## b1_slope                      NA          NA
## breakpoint                    NA          NA
## b2                            NA          NA
## b2_slope                      NA          NA
## quadratic_term                NA          NA
## a1                            NA  4.39756224
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
##                          BM_null      OU_null
## logLik                13.2399034  14.68752901
## n                      1.0000000   2.00000000
## AIC                  -24.4798067 -25.37505803
## AICc                 -24.3259606 -24.89505803
## b1                     0.0054231   0.01011076
## b1_slope                      NA           NA
## breakpoint                    NA           NA
## b2                            NA           NA
## b2_slope                      NA           NA
## quadratic_term                NA           NA
## a1                            NA   0.31020791
## a1_slope                      NA           NA
## a2                            NA           NA
## a2_slope                      NA           NA
## Quadratic_c                   NA           NA
## Quadratic_b                   NA           NA
## Quadratic_a                   NA           NA
## nlm_termination_code   1.0000000   1.00000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -29.44436
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -35.49636
```

