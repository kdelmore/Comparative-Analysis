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
#evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
evodat["trait"]<-evodat$avg_colour10
#evodat["trait"]<-evodat$avg_morph10
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
## logLik                12.899687138  1.382191e+01  18.96195986
## n                      1.000000000  2.000000e+00   2.00000000
## AIC                  -23.799374276 -2.364383e+01 -33.92391972
## AICc                 -23.681727218 -2.328019e+01 -33.56028336
## b1                     0.008335286  5.082625e-03   0.05099368
## b1_slope                        NA  1.492677e-04           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   2.31416997
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000  1.000000e+00   1.00000000
##                          OU_linear OU_linear_beta
## logLik                20.465566023   20.453488857
## n                      4.000000000    3.000000000
## AIC                  -32.931132045  -34.906977714
## AICc                 -31.640809465  -34.156977714
## b1                     0.007781481    0.013106961
## b1_slope               0.002450663    0.001891497
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     2.093388069    2.301830184
## a1_slope               0.020586028             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
##                            BM_null    BM_linear      OU_null    OU_linear
## logLik                12.899687138  15.72217930  18.96195986  19.74818205
## n                      1.000000000   2.00000000   2.00000000   4.00000000
## AIC                  -23.799374276 -27.44435859 -33.92391972 -31.49636410
## AICc                 -23.681727218 -27.08072223 -33.56028336 -30.20604152
## b1                     0.008335286   0.00542320   0.05099368   0.01011078
## b1_slope                        NA   0.01310074           NA   0.07104463
## breakpoint                      NA           NA           NA           NA
## b2                              NA           NA           NA           NA
## b2_slope                        NA           NA           NA           NA
## quadratic_term                  NA           NA           NA           NA
## a1                              NA           NA   2.31416997   0.31020910
## a1_slope                        NA           NA           NA   4.08733613
## a2                              NA           NA           NA           NA
## a2_slope                        NA           NA           NA           NA
## Quadratic_c                     NA           NA           NA           NA
## Quadratic_b                     NA           NA           NA           NA
## Quadratic_a                     NA           NA           NA           NA
## nlm_termination_code   1.000000000   1.00000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                 18.969881772
## n                       3.000000000
## AIC                   -31.939763545
## AICc                  -31.189763545
## b1                      0.058815920
## b1_slope               -0.004885431
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      2.649464175
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    1.000000000
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
#evodat["trait"]<-evodat$avg_all10 #re-set this for each variable being analyzed
#evodat["trait"]<-evodat$avg_song10
evodat["trait"]<-evodat$avg_colour10
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
g9<-evodat$overlap_proportion
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null
## logLik                 9.979526259  10.164861534  14.08798799
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -17.959052517 -16.329723068 -24.17597599
## AICc                 -17.873946134 -16.068853503 -23.91510642
## b1                     0.008682842   0.007920962   0.01991003
## b1_slope                        NA   0.004575729           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.42679730
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                2.011415e+01    18.31245196
## n                     4.000000e+00     3.00000000
## AIC                  -3.222830e+01   -30.62490393
## AICc                 -3.131921e+01   -30.09157059
## b1                    5.851967e-02     0.05453201
## b1_slope              2.185279e+07     0.40367241
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    5.843906e+00     3.25484148
## a1_slope              3.884799e+08             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code  1.000000e+00     1.00000000
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
##                          BM_null   BM_linear    OU_null  OU_linear
## logLik               -0.31193100 -0.08675360 1.93442469 3.20591635
## n                     1.00000000  2.00000000 2.00000000 4.00000000
## AIC                   2.62386201  4.17350720 0.13115061 1.58816731
## AICc                  2.85915612  4.92350720 0.88115061 4.44531016
## b1                    0.01401521  0.01740918 0.03730289 0.06022637
## b1_slope                      NA -0.00946067         NA 0.75546004
## breakpoint                    NA          NA         NA         NA
## b2                            NA          NA         NA         NA
## b2_slope                      NA          NA         NA         NA
## quadratic_term                NA          NA         NA         NA
## a1                            NA          NA 0.57014268 4.64102983
## a1_slope                      NA          NA         NA 2.97073165
## a2                            NA          NA         NA         NA
## a2_slope                      NA          NA         NA         NA
## Quadratic_c                   NA          NA         NA         NA
## Quadratic_b                   NA          NA         NA         NA
## Quadratic_a                   NA          NA         NA         NA
## nlm_termination_code  1.00000000  1.00000000 1.00000000 1.00000000
##                      OU_linear_beta
## logLik                   3.17766369
## n                        3.00000000
## AIC                     -0.35532738
## AICc                     1.24467262
## b1                       0.06655896
## b1_slope                 0.53442272
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       4.72301837
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
g10 <-evodat_perp_patry$overlap_proportion
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear       OU_null
## logLik                13.131483720  13.138913273  14.499359091
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -24.262967439 -22.277826547 -24.998718181
## AICc                 -24.120110296 -21.833382102 -24.554273737
## b1                     0.005305676   0.005358099   0.009246617
## b1_slope                        NA  -0.001175305            NA
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
## nlm_termination_code   1.000000000   2.000000000   1.000000000
##                          OU_linear OU_linear_beta
## logLik                1.806771e+01    15.37597864
## n                     4.000000e+00     3.00000000
## AIC                  -2.813541e+01   -24.75195728
## AICc                 -2.653541e+01   -23.82888036
## b1                    1.909047e-01     0.03861165
## b1_slope              1.151883e+08     0.52582314
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    1.987028e+01     2.37561192
## a1_slope              2.316308e+09             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code  1.000000e+00     1.00000000
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
## [1] -24.10432
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -38.54725
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -33.10728
```

