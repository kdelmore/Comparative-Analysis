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
evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
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
## logLik                22.203860169  26.98934384
## n                      1.000000000   2.00000000
## AIC                  -42.407720338 -49.97868768
## AICc                 -42.322613955 -49.71781812
## b1                     0.005271928   0.01266311
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.47552463
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
##                           BM_null     OU_null
## logLik                3.721180439  5.52282866
## n                     1.000000000  2.00000000
## AIC                  -5.442360879 -7.04565731
## AICc                 -5.207066761 -6.29565731
## b1                    0.009166952  0.02074476
## b1_slope                       NA          NA
## breakpoint                     NA          NA
## b2                             NA          NA
## b2_slope                       NA          NA
## quadratic_term                 NA          NA
## a1                             NA  0.42367374
## a1_slope                       NA          NA
## a2                             NA          NA
## a2_slope                       NA          NA
## Quadratic_c                    NA          NA
## Quadratic_b                    NA          NA
## Quadratic_a                    NA          NA
## nlm_termination_code  1.000000000  1.00000000
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
##                           BM_null       OU_null
## logLik                22.69165321  26.206246782
## n                      1.00000000   2.000000000
## AIC                  -43.38330641 -48.412493563
## AICc                 -43.24044927 -47.968049119
## b1                     0.00280508   0.008467075
## b1_slope                       NA            NA
## breakpoint                     NA            NA
## b2                             NA            NA
## b2_slope                       NA            NA
## quadratic_term                 NA            NA
## a1                             NA   0.690203719
## a1_slope                       NA            NA
## a2                             NA            NA
## a2_slope                       NA            NA
## Quadratic_c                    NA            NA
## Quadratic_b                    NA            NA
## Quadratic_a                    NA            NA
## nlm_termination_code   1.00000000   1.000000000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -50.82567
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -59.45815
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
## logLik                22.203860169  23.041787144  26.98934384  27.20999083
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -42.407720338 -42.083574288 -49.97868768 -46.41998166
## AICc                 -42.322613955 -41.822704723 -49.71781812 -45.51089075
## b1                     0.005271928   0.006107067   0.01266311   0.01246298
## b1_slope                        NA  -0.004591961           NA   0.12731548
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.47552463   0.72616739
## a1_slope                        NA            NA           NA   3.35135084
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                 27.029338376
## n                       3.000000000
## AIC                   -48.058676751
## AICc                  -47.525343418
## b1                      0.012848995
## b1_slope                0.005106618
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      0.533214078
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    1.000000000
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
##                           BM_null   BM_linear     OU_null   OU_linear
## logLik                3.721180439  5.94362472  5.52282866  6.65857831
## n                     1.000000000  2.00000000  2.00000000  4.00000000
## AIC                  -5.442360879 -7.88724945 -7.04565731 -5.31715663
## AICc                 -5.207066761 -7.13724945 -6.29565731 -2.46001377
## b1                    0.009166952  0.01285265  0.02074476  0.01861160
## b1_slope                       NA -0.01240673          NA -0.01084908
## breakpoint                     NA          NA          NA          NA
## b2                             NA          NA          NA          NA
## b2_slope                       NA          NA          NA          NA
## quadratic_term                 NA          NA          NA          NA
## a1                             NA          NA  0.42367374  0.09286587
## a1_slope                       NA          NA          NA  0.46347769
## a2                             NA          NA          NA          NA
## a2_slope                       NA          NA          NA          NA
## Quadratic_c                    NA          NA          NA          NA
## Quadratic_b                    NA          NA          NA          NA
## Quadratic_a                    NA          NA          NA          NA
## nlm_termination_code  1.000000000  1.00000000  1.00000000  1.00000000
##                      OU_linear_beta
## logLik                   6.30339629
## n                        3.00000000
## AIC                     -6.60679257
## AICc                    -5.00679257
## b1                       0.01549721
## b1_slope                -0.01010000
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       0.19896226
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
##                           BM_null     BM_linear       OU_null
## logLik                22.69165321  22.704909450  26.206246782
## n                      1.00000000   2.000000000   2.000000000
## AIC                  -43.38330641 -41.409818899 -48.412493563
## AICc                 -43.24044927 -40.965374455 -47.968049119
## b1                     0.00280508   0.002935161   0.008467075
## b1_slope                       NA  -0.001774540            NA
## breakpoint                     NA            NA            NA
## b2                             NA            NA            NA
## b2_slope                       NA            NA            NA
## quadratic_term                 NA            NA            NA
## a1                             NA            NA   0.690203719
## a1_slope                       NA            NA            NA
## a2                             NA            NA            NA
## a2_slope                       NA            NA            NA
## Quadratic_c                    NA            NA            NA
## Quadratic_b                    NA            NA            NA
## Quadratic_a                    NA            NA            NA
## nlm_termination_code   1.00000000   2.000000000   1.000000000
##                          OU_linear OU_linear_beta
## logLik                27.774347603   27.328177918
## n                      4.000000000    3.000000000
## AIC                  -47.548695207  -48.656355836
## AICc                 -45.948695207  -47.733278913
## b1                     0.004160796    0.007311597
## b1_slope               0.244598251    0.095192452
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.742383444    1.134453001
## a1_slope              10.798798216             NA
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
## [1] -55.29707
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -64.86585
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -63.26315
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
## logLik                20.828398394  25.20173757
## n                      1.000000000   2.00000000
## AIC                  -39.656796788 -46.40347513
## AICc                 -39.565887697 -46.12440536
## b1                     0.005411348   0.01258302
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.46116747
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
##                         BM_null    OU_null
## logLik                3.7300142  5.5081098
## n                     1.0000000  2.0000000
## AIC                  -5.4600284 -7.0162195
## AICc                 -5.2247343 -6.2662195
## b1                    0.0091223  0.0205523
## b1_slope                     NA         NA
## breakpoint                   NA         NA
## b2                           NA         NA
## b2_slope                     NA         NA
## quadratic_term               NA         NA
## a1                           NA  0.4191659
## a1_slope                     NA         NA
## a2                           NA         NA
## a2_slope                     NA         NA
## Quadratic_c                  NA         NA
## Quadratic_b                  NA         NA
## Quadratic_a                  NA         NA
## nlm_termination_code  1.0000000  1.0000000
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
##                            BM_null       OU_null
## logLik                21.032416604  24.577082528
## n                      1.000000000   2.000000000
## AIC                  -40.064833207 -45.154165056
## AICc                 -39.904833207 -44.654165056
## b1                     0.002799939   0.009067215
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.818020601
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
## [1] -47.52486
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -56.17038
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
## logLik                20.828398394  21.721730142  25.20173757  25.29707886
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -39.656796788 -39.443460283 -46.40347513 -42.59415773
## AICc                 -39.565887697 -39.164390516 -46.12440536 -41.61854797
## b1                     0.005411348   0.006310702   0.01258302   0.01200978
## b1_slope                        NA  -0.004841264           NA   0.11561962
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.46116747   0.65222354
## a1_slope                        NA            NA           NA   3.13980128
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                 25.211455228
## n                       3.000000000
## AIC                   -44.422910456
## AICc                  -43.851481885
## b1                      0.012715276
## b1_slope                0.002217033
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      0.489413436
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    1.000000000
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
##                         BM_null   BM_linear    OU_null   OU_linear
## logLik                3.7300142  5.93968377  5.5081098  6.64471245
## n                     1.0000000  2.00000000  2.0000000  4.00000000
## AIC                  -5.4600284 -7.87936754 -7.0162195 -5.28942490
## AICc                 -5.2247343 -7.12936754 -6.2662195 -2.43228204
## b1                    0.0091223  0.01279425  0.0205523  0.01840694
## b1_slope                     NA -0.01233978         NA -0.01066848
## breakpoint                   NA          NA         NA          NA
## b2                           NA          NA         NA          NA
## b2_slope                     NA          NA         NA          NA
## quadratic_term               NA          NA         NA          NA
## a1                           NA          NA  0.4191659  0.08886828
## a1_slope                     NA          NA         NA  0.46486277
## a2                           NA          NA         NA          NA
## a2_slope                     NA          NA         NA          NA
## Quadratic_c                  NA          NA         NA          NA
## Quadratic_b                  NA          NA         NA          NA
## Quadratic_a                  NA          NA         NA          NA
## nlm_termination_code  1.0000000  2.00000000  1.0000000  1.00000000
##                      OU_linear_beta
## logLik                   6.29529153
## n                        3.00000000
## AIC                     -6.59058306
## AICc                    -4.99058306
## b1                       0.01538429
## b1_slope                -0.01010000
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       0.19575148
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
##                            BM_null     BM_linear       OU_null
## logLik                21.032416604  21.098896965  24.577082528
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -40.064833207 -38.197793930 -45.154165056
## AICc                 -39.904833207 -37.697793930 -44.654165056
## b1                     0.002799939   0.002986989   0.009067215
## b1_slope                        NA  -0.002711684            NA
## breakpoint                      NA            NA            NA
## b2                              NA            NA            NA
## b2_slope                        NA            NA            NA
## quadratic_term                  NA            NA            NA
## a1                              NA            NA   0.818020601
## a1_slope                        NA            NA            NA
## a2                              NA            NA            NA
## a2_slope                        NA            NA            NA
## Quadratic_c                     NA            NA            NA
## Quadratic_b                     NA            NA            NA
## Quadratic_a                     NA            NA            NA
## nlm_termination_code   1.000000000   2.000000000   1.000000000
##                          OU_linear OU_linear_beta
## logLik                25.859759908   25.199169230
## n                      4.000000000    3.000000000
## AIC                  -43.719519815  -44.398338461
## AICc                 -41.901337997  -43.354860200
## b1                     0.003749991    0.006965855
## b1_slope               0.291143606    0.091572615
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.693339179    1.089755059
## a1_slope              15.823811783             NA
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
## [1] -52.07716
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -61.00894
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -58.98892
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
## logLik                20.778867179  2.149967e+01  25.24855002
## n                      1.000000000  2.000000e+00   2.00000000
## AIC                  -39.557734359 -3.899934e+01 -46.49710005
## AICc                 -39.466825268 -3.872027e+01 -46.21803028
## b1                     0.005494315  3.524887e-03   0.01284287
## b1_slope                        NA  1.010774e-04           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.47080135
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000  1.000000e+00   1.00000000
##                          OU_linear OU_linear_beta
## logLik                2.620429e+01   2.539858e+01
## n                     4.000000e+00   3.000000e+00
## AIC                  -4.440858e+01  -4.479717e+01
## AICc                 -4.343297e+01  -4.422574e+01
## b1                    2.258854e-03   9.777167e-03
## b1_slope              5.672894e-04   1.164179e-04
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    1.000103e-06   4.249841e-01
## a1_slope              2.449361e-02             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code  2.000000e+00   1.000000e+00
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
## logLik                20.778867179  24.496584347  25.24855002
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -39.557734359 -44.993168695 -46.49710005
## AICc                 -39.466825268 -44.714098927 -46.21803028
## b1                     0.005494315   0.002910154   0.01284287
## b1_slope                        NA   0.006252788           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.47080135
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                29.997896727    29.94550453
## n                      4.000000000     3.00000000
## AIC                  -51.995793455   -53.89100906
## AICc                 -51.020183699   -53.31958049
## b1                     0.007649919     0.00743286
## b1_slope               0.017160986     0.01992723
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.644712431     0.61889981
## a1_slope              -0.100099998             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000     2.00000000
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
evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
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
## logLik                21.269514203  27.35558920
## n                      1.000000000   2.00000000
## AIC                  -40.539028407 -50.71117840
## AICc                 -40.421381348 -50.34754204
## b1                     0.005235734   0.01705468
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   1.11726054
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
## logLik                3.79753910  4.30233028
## n                     1.00000000  2.00000000
## AIC                  -5.59507821 -4.60466057
## AICc                 -4.92841154 -2.20466057
## b1                    0.01333595  0.02285771
## b1_slope                      NA          NA
## breakpoint                    NA          NA
## b2                            NA          NA
## b2_slope                      NA          NA
## quadratic_term                NA          NA
## a1                            NA  0.57124087
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
##                            BM_null       OU_null
## logLik                21.900428422  25.269400196
## n                      1.000000000   2.000000000
## AIC                  -41.800856843 -46.538800392
## AICc                 -41.647010689 -46.058800392
## b1                     0.002921386   0.009322695
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.826085820
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
## [1] -49.39594
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -55.14346
```

