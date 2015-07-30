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
evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
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
g9<-evodat$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null   OU_linear
## logLik                18.491938644  18.649660891  23.71689259  26.0126523
## n                      1.000000000   2.000000000   2.00000000   4.0000000
## AIC                  -34.983877287 -33.299321781 -43.43378518 -44.0253046
## AICc                 -34.898770904 -33.038452216 -43.17291561 -43.1162137
## b1                     0.006134345   0.006605677   0.01752217   0.0326312
## b1_slope                        NA  -0.002581214           NA   0.3299469
## breakpoint                      NA            NA           NA          NA
## b2                              NA            NA           NA          NA
## b2_slope                        NA            NA           NA          NA
## quadratic_term                  NA            NA           NA          NA
## a1                              NA            NA   0.61845964   2.6076273
## a1_slope                        NA            NA           NA   4.2013542
## a2                              NA            NA           NA          NA
## a2_slope                        NA            NA           NA          NA
## Quadratic_c                     NA            NA           NA          NA
## Quadratic_b                     NA            NA           NA          NA
## Quadratic_a                     NA            NA           NA          NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000   1.0000000
##                      OU_linear_beta
## logLik                  25.86492889
## n                        3.00000000
## AIC                    -45.72985778
## AICc                   -45.19652445
## b1                       0.03901859
## b1_slope                 0.17458562
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       2.89818853
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code     2.00000000
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
##                           BM_null   BM_linear    OU_null   OU_linear
## logLik                3.135422112  4.43173578  6.7129518  6.75444820
## n                     1.000000000  2.00000000  2.0000000  4.00000000
## AIC                  -4.270844224 -4.86347156 -9.4259036 -5.50889639
## AICc                 -4.035550107 -4.11347156 -8.6759036 -2.65175353
## b1                    0.009749965  0.01351171  0.0404328  0.01742742
## b1_slope                       NA -0.01163025         NA  1.20846276
## breakpoint                     NA          NA         NA          NA
## b2                             NA          NA         NA          NA
## b2_slope                       NA          NA         NA          NA
## quadratic_term                 NA          NA         NA          NA
## a1                             NA          NA  1.1825905  0.57288635
## a1_slope                       NA          NA         NA 35.94682128
## a2                             NA          NA         NA          NA
## a2_slope                       NA          NA         NA          NA
## Quadratic_c                    NA          NA         NA          NA
## Quadratic_b                    NA          NA         NA          NA
## Quadratic_a                    NA          NA         NA          NA
## nlm_termination_code  1.000000000  1.00000000  1.0000000  1.00000000
##                      OU_linear_beta
## logLik                  6.715252446
## n                       3.000000000
## AIC                    -7.430504893
## AICc                   -5.830504893
## b1                      0.042173245
## b1_slope                0.004070607
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      1.291403056
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
g10 <-evodat_perp_patry$overlap_proportion_hz_info
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear       OU_null
## logLik                17.963671013  18.123135369  18.919184256
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -33.927342026 -32.246270738 -33.838368513
## AICc                 -33.784484883 -31.801826294 -33.393924068
## b1                     0.003844453   0.003489983   0.006061431
## b1_slope                        NA   0.004955765            NA
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
## nlm_termination_code   1.000000000   2.000000000   1.000000000
##                          OU_linear OU_linear_beta
## logLik                20.803469006   20.779708906
## n                      4.000000000    3.000000000
## AIC                  -33.606938013  -35.559417812
## AICc                 -32.006938013  -34.636340889
## b1                     0.007965075    0.008083926
## b1_slope               0.070996826    0.077572300
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.657247001    0.676933949
## a1_slope              -0.100099972             NA
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
## [1] -43.10974
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -51.11583
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -50.98992
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
## logLik                17.083308165  21.50209242
## n                      1.000000000   2.00000000
## AIC                  -32.166616330 -39.00418485
## AICc                 -32.075707239 -38.72511508
## b1                     0.006368274   0.01610987
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.52198339
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
##                           BM_null     OU_null
## logLik                3.169205795  6.69871738
## n                     1.000000000  2.00000000
## AIC                  -4.338411590 -9.39743476
## AICc                 -4.103117473 -8.64743476
## b1                    0.009677024  0.03968025
## b1_slope                       NA          NA
## breakpoint                     NA          NA
## b2                             NA          NA
## b2_slope                       NA          NA
## quadratic_term                 NA          NA
## a1                             NA  1.15929030
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
t3<-evodat_perp$speed_coi_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null      OU_null
## logLik                16.083038233  16.69516608
## n                      1.000000000   2.00000000
## AIC                  -30.166076466 -29.39033217
## AICc                 -30.006076466 -28.89033217
## b1                     0.004039894   0.00579031
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.14199033
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
## [1] -36.50449
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -42.78777
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
## logLik                17.083308165  17.261153402  21.50209242  23.46520006
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -32.166616330 -30.522306804 -39.00418485 -38.93040011
## AICc                 -32.075707239 -30.243237036 -38.72511508 -37.95479036
## b1                     0.006368274   0.006913594   0.01610987   0.02849367
## b1_slope                        NA  -0.002902299           NA   0.33996329
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.52198339   2.19259144
## a1_slope                        NA            NA           NA   4.81567497
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                  23.25625060
## n                        3.00000000
## AIC                    -40.51250120
## AICc                   -39.94107263
## b1                       0.03512405
## b1_slope                 0.15066856
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       2.45260812
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
##                           BM_null   BM_linear     OU_null   OU_linear
## logLik                3.169205795  4.44640377  6.69871738  6.74158476
## n                     1.000000000  2.00000000  2.00000000  4.00000000
## AIC                  -4.338411590 -4.89280754 -9.39743476 -5.48316952
## AICc                 -4.103117473 -4.14280754 -8.64743476 -2.62602666
## b1                    0.009677024  0.01340596  0.03968025  0.01695905
## b1_slope                       NA -0.01150453          NA  1.19324408
## breakpoint                     NA          NA          NA          NA
## b2                             NA          NA          NA          NA
## b2_slope                       NA          NA          NA          NA
## quadratic_term                 NA          NA          NA          NA
## a1                             NA          NA  1.15929030  0.55387980
## a1_slope                       NA          NA          NA 35.51621660
## a2                             NA          NA          NA          NA
## a2_slope                       NA          NA          NA          NA
## Quadratic_c                    NA          NA          NA          NA
## Quadratic_b                    NA          NA          NA          NA
## Quadratic_a                    NA          NA          NA          NA
## nlm_termination_code  1.000000000  1.00000000  1.00000000  1.00000000
##                      OU_linear_beta
## logLik                  6.700891168
## n                       3.000000000
## AIC                    -7.401782335
## AICc                   -5.801782335
## b1                      0.041204516
## b1_slope                0.004087859
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      1.262467049
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    2.000000000
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
## logLik                16.083038233  16.304963576  16.69516608
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -30.166076466 -28.609927152 -29.39033217
## AICc                 -30.006076466 -28.109927152 -28.89033217
## b1                     0.004039894   0.003553306   0.00579031
## b1_slope                        NA   0.007424485           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.14199033
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                18.396000534   18.374698572
## n                      4.000000000    3.000000000
## AIC                  -28.792001068  -30.749397145
## AICc                 -26.973819249  -29.705918884
## b1                     0.006855817    0.006842032
## b1_slope               0.067765174    0.073336452
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.513337925    0.518070388
## a1_slope              -0.100099997             NA
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
## [1] -39.50273
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -46.27517
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -46.15118
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
## logLik                16.992946585  1.839867e+01  21.53855512
## n                      1.000000000  2.000000e+00   2.00000000
## AIC                  -31.985893169 -3.279733e+01 -39.07711024
## AICc                 -31.894984078 -3.251826e+01 -38.79804047
## b1                     0.006477401  3.704316e-03   0.01663957
## b1_slope                        NA  1.355225e-04           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.54129927
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000  2.000000e+00   1.00000000
##                          OU_linear OU_linear_beta
## logLik                2.247167e+01   2.232561e+01
## n                     4.000000e+00   3.000000e+00
## AIC                  -3.694335e+01  -3.865121e+01
## AICc                 -3.596774e+01  -3.807978e+01
## b1                    4.319032e-03   8.710881e-03
## b1_slope              6.162271e-04   2.887814e-04
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    1.822982e-01   4.354755e-01
## a1_slope              1.637295e-02             NA
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
## logLik                16.992946585  19.038902697  21.53855512
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -31.985893169 -34.077805394 -39.07711024
## AICc                 -31.894984078 -33.798735627 -38.79804047
## b1                     0.006477401   0.004174501   0.01663957
## b1_slope                        NA   0.005575368           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.54129927
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                23.316586844   22.467331262
## n                      4.000000000    3.000000000
## AIC                  -38.633173688  -38.934662524
## AICc                 -37.657563932  -38.363233952
## b1                     0.006131171    0.010405616
## b1_slope               0.034301665    0.008299899
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.156197797    0.411590877
## a1_slope               1.026393696             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
##                            BM_null      OU_null
## logLik                16.490583765  23.31514055
## n                      1.000000000   2.00000000
## AIC                  -30.981167531 -42.63028110
## AICc                 -30.852135273 -42.23028110
## b1                     0.006791993   0.05011969
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   3.34192786
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
##                      BM_null OU_null
## logLik                 0e+00   0.000
## n                      1e+00   2.000
## AIC                    2e+00   4.000
## AICc                   0e+00   0.000
## b1                     1e-05   0.010
## b1_slope                  NA      NA
## breakpoint                NA      NA
## b2                        NA      NA
## b2_slope                  NA      NA
## quadratic_term            NA      NA
## a1                        NA   0.001
## a1_slope                  NA      NA
## a2                        NA      NA
## a2_slope                  NA      NA
## Quadratic_c               NA      NA
## Quadratic_b               NA      NA
## Quadratic_a               NA      NA
## nlm_termination_code   1e+00   1.000
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
##                      BM_null OU_null
## logLik                 0e+00   0.000
## n                      1e+00   2.000
## AIC                    2e+00   4.000
## AICc                   0e+00   0.000
## b1                     1e-05   0.010
## b1_slope                  NA      NA
## breakpoint                NA      NA
## b2                        NA      NA
## b2_slope                  NA      NA
## quadratic_term            NA      NA
## a1                        NA   0.001
## a1_slope                  NA      NA
## a2                        NA      NA
## a2_slope                  NA      NA
## Quadratic_c               NA      NA
## Quadratic_b               NA      NA
## Quadratic_a               NA      NA
## nlm_termination_code   1e+00   1.000
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] 2
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] 4
```

