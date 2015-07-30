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
#evodat["trait"]<-evodat$avg_colour10
evodat["trait"]<-evodat$avg_morph10
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
## logLik                12.934141633  22.3469207
## n                      1.000000000   2.0000000
## AIC                  -23.868283266 -40.6938413
## AICc                 -23.750636207 -40.3302050
## b1                     0.008319346   0.8662344
## b1_slope                        NA          NA
## breakpoint                      NA          NA
## b2                              NA          NA
## b2_slope                        NA          NA
## quadratic_term                  NA          NA
## a1                              NA  51.2017313
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
##                          BM_null   OU_null
## logLik                1.98397640  5.026157
## n                     1.00000000  2.000000
## AIC                  -1.96795280 -6.052315
## AICc                 -1.30128614 -3.652315
## b1                    0.02098598  1.022275
## b1_slope                      NA        NA
## breakpoint                    NA        NA
## b2                            NA        NA
## b2_slope                      NA        NA
## quadratic_term                NA        NA
## a1                            NA 61.340932
## a1_slope                      NA        NA
## a2                            NA        NA
## a2_slope                      NA        NA
## Quadratic_c                   NA        NA
## Quadratic_b                   NA        NA
## Quadratic_a                   NA        NA
## nlm_termination_code  1.00000000  1.000000
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
##                            BM_null      OU_null
## logLik                15.242455523  17.84428119
## n                      1.000000000   2.00000000
## AIC                  -28.484911047 -31.68856238
## AICc                 -28.331064893 -31.20856238
## b1                     0.004700308   0.01283746
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.62957916
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
## [1] -32.45286
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -41.74088
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
## logLik                12.934141633  15.866259849  22.3469207  23.004400504
## n                      1.000000000   2.000000000   2.0000000   4.000000000
## AIC                  -23.868283266 -27.732519697 -40.6938413 -38.008801008
## AICc                 -23.750636207 -27.368883334 -40.3302050 -36.718478427
## b1                     0.008319346   0.002687721   0.8662344   0.008743369
## b1_slope                        NA   0.000242093          NA   0.001451469
## breakpoint                      NA            NA          NA            NA
## b2                              NA            NA          NA            NA
## b2_slope                        NA            NA          NA            NA
## quadratic_term                  NA            NA          NA            NA
## a1                              NA            NA  51.2017313   1.006078458
## a1_slope                        NA            NA          NA   0.048555404
## a2                              NA            NA          NA            NA
## a2_slope                        NA            NA          NA            NA
## Quadratic_c                     NA            NA          NA            NA
## Quadratic_b                     NA            NA          NA            NA
## Quadratic_a                     NA            NA          NA            NA
## nlm_termination_code   1.000000000   2.000000000   1.0000000   1.000000000
##                      OU_linear_beta
## logLik                 22.844919650
## n                       3.000000000
## AIC                   -39.689839300
## AICc                  -38.939839300
## b1                      0.024819347
## b1_slope                0.001151601
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      2.638440526
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code    1.000000000
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
## logLik                12.934141633  17.226431850  22.3469207  22.87043892
## n                      1.000000000   2.000000000   2.0000000   4.00000000
## AIC                  -23.868283266 -30.452863700 -40.6938413 -37.74087785
## AICc                 -23.750636207 -30.089227336 -40.3302050 -36.45055527
## b1                     0.008319346   0.004700367   0.8662344   0.01283747
## b1_slope                        NA   0.016281613          NA   1.03080301
## breakpoint                      NA            NA          NA           NA
## b2                              NA            NA          NA           NA
## b2_slope                        NA            NA          NA           NA
## quadratic_term                  NA            NA          NA           NA
## a1                              NA            NA  51.2017313   0.62957992
## a1_slope                        NA            NA          NA  61.99336763
## a2                              NA            NA          NA           NA
## a2_slope                        NA            NA          NA           NA
## Quadratic_c                     NA            NA          NA           NA
## Quadratic_b                     NA            NA          NA           NA
## Quadratic_a                     NA            NA          NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.0000000   1.00000000
##                      OU_linear_beta
## logLik                 22.344387436
## n                       3.000000000
## AIC                   -38.688774871
## AICc                  -37.938774871
## b1                      0.327147729
## b1_slope               -0.004967709
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                     19.252019904
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
#evodat["trait"]<-evodat$avg_colour10
evodat["trait"]<-evodat$avg_morph10
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
## logLik                12.674010752  16.93935105
## n                      1.000000000   2.00000000
## AIC                  -23.348021504 -29.87870210
## AICc                 -23.262915121 -29.61783254
## b1                     0.007778549   0.02132601
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.55532483
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
##                          BM_null     OU_null
## logLik                1.81709758   9.3430399
## n                     1.00000000   2.0000000
## AIC                  -1.63419515 -14.6860798
## AICc                 -1.39890103 -13.9360798
## b1                    0.01120135   0.1445323
## b1_slope                      NA          NA
## breakpoint                    NA          NA
## b2                            NA          NA
## b2_slope                      NA          NA
## quadratic_term                NA          NA
## a1                            NA   6.4260147
## a1_slope                      NA          NA
## a2                            NA          NA
## a2_slope                      NA          NA
## Quadratic_c                   NA          NA
## Quadratic_b                   NA          NA
## Quadratic_a                   NA          NA
## nlm_termination_code  1.00000000   1.0000000
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
## logLik                12.292816504  12.311903595
## n                      1.000000000   2.000000000
## AIC                  -22.585633008 -20.623807190
## AICc                 -22.442775865 -20.179362745
## b1                     0.005610773   0.005943889
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA   0.017960849
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
## [1] -26.21983
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -39.30989
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
## logLik                12.674010752  12.914605594  16.93935105  22.09705339
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -23.348021504 -21.829211188 -29.87870210 -36.19410679
## AICc                 -23.262915121 -21.568341623 -29.61783254 -35.28501588
## b1                     0.007778549   0.008436416   0.02132601   0.05981612
## b1_slope                        NA  -0.003921381           NA   1.93186258
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.55532483   4.82710083
## a1_slope                        NA            NA           NA  23.27959345
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                   21.5319969
## n                         3.0000000
## AIC                     -37.0639939
## AICc                    -36.5306606
## b1                        0.2018179
## b1_slope                  1.7936238
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       15.0545484
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code      1.0000000
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
##                          BM_null   BM_linear     OU_null    OU_linear
## logLik                1.81709758  3.80545257   9.3430399   9.50022307
## n                     1.00000000  2.00000000   2.0000000   4.00000000
## AIC                  -1.63419515 -3.61090515 -14.6860798 -11.00044613
## AICc                 -1.39890103 -2.86090515 -13.9360798  -8.14330327
## b1                    0.01120135  0.01485540   0.1445323   0.03533784
## b1_slope                      NA -0.01325518          NA   9.46860324
## breakpoint                    NA          NA          NA           NA
## b2                            NA          NA          NA           NA
## b2_slope                      NA          NA          NA           NA
## quadratic_term                NA          NA          NA           NA
## a1                            NA          NA   6.4260147   1.54568413
## a1_slope                      NA          NA          NA 416.08907818
## a2                            NA          NA          NA           NA
## a2_slope                      NA          NA          NA           NA
## Quadratic_c                   NA          NA          NA           NA
## Quadratic_b                   NA          NA          NA           NA
## Quadratic_a                   NA          NA          NA           NA
## nlm_termination_code  1.00000000  1.00000000   1.0000000   1.00000000
##                      OU_linear_beta
## logLik                   9.34876775
## n                        3.00000000
## AIC                    -12.69753550
## AICc                   -11.09753550
## b1                       0.13609080
## b1_slope                -0.01740527
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       5.73737053
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
## logLik                12.292816504  13.533103576  12.311903595
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -22.585633008 -23.066207152 -20.623807190
## AICc                 -22.442775865 -22.621762708 -20.179362745
## b1                     0.005610773   0.004522114   0.005943889
## b1_slope                        NA   0.019692115            NA
## breakpoint                      NA            NA            NA
## b2                              NA            NA            NA
## b2_slope                        NA            NA            NA
## quadratic_term                  NA            NA            NA
## a1                              NA            NA   0.017960849
## a1_slope                        NA            NA            NA
## a2                              NA            NA            NA
## a2_slope                        NA            NA            NA
## Quadratic_c                     NA            NA            NA
## Quadratic_b                     NA            NA            NA
## Quadratic_a                     NA            NA            NA
## nlm_termination_code   1.000000000   1.000000000   1.000000000
##                         OU_linear OU_linear_beta
## logLik                17.57023208    17.55946871
## n                      4.00000000     3.00000000
## AIC                  -27.14046416   -29.11893743
## AICc                 -25.54046416   -28.19586051
## b1                     0.02574689     0.02727497
## b1_slope               0.87125829     0.94017729
## breakpoint                     NA             NA
## b2                             NA             NA
## b2_slope                       NA             NA
## quadratic_term                 NA             NA
## a1                     2.20950456     2.35170893
## a1_slope              -0.10010000             NA
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
## [1] -32.67711
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -50.14091
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -49.81647
```

