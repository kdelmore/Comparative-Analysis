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
evodat["trait"]<-evodat$avg_song10
#evodat["trait"]<-evodat$avg_colour10
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
## logLik                21.269514203  22.961176494  27.35558920
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -40.539028407 -41.922352987 -50.71117840
## AICc                 -40.421381348 -41.558716624 -50.34754204
## b1                     0.005235734   0.001761784   0.01705468
## b1_slope                        NA   0.000166196           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   1.11726054
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                3.008736e+01   30.060462361
## n                     4.000000e+00    3.000000000
## AIC                  -5.217472e+01  -54.120924722
## AICc                 -5.088440e+01  -53.370924722
## b1                    3.010897e-03    0.001306088
## b1_slope              8.752838e-04    0.001059220
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    1.658199e+00    1.548376234
## a1_slope             -1.010000e-02             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code  1.000000e+00    1.000000000
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
## logLik                21.269514203  25.697967523  27.35558920
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -40.539028407 -47.395935045 -50.71117840
## AICc                 -40.421381348 -47.032298682 -50.34754204
## b1                     0.005235734   0.002921394   0.01705468
## b1_slope                        NA   0.010414086           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   1.11726054
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                29.564154026   29.551002917
## n                      4.000000000    3.000000000
## AIC                  -51.128308052  -53.102005835
## AICc                 -49.837985471  -52.352005835
## b1                     0.008634332    0.008413058
## b1_slope               0.015804303    0.017552584
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.749688933    0.723338637
## a1_slope              -0.100099997             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
evodat["trait"]<-evodat$avg_song10
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
g9<-evodat$overlap_proportion
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null    OU_linear
## logLik                22.203860169  23.008641487  26.98934384  27.47035878
## n                      1.000000000   2.000000000   2.00000000   4.00000000
## AIC                  -42.407720338 -42.017282973 -49.97868768 -46.94071757
## AICc                 -42.322613955 -41.756413408 -49.71781812 -46.03162666
## b1                     0.005271928   0.006020135   0.01266311   0.01373252
## b1_slope                        NA  -0.004459859           NA   0.32481960
## breakpoint                      NA            NA           NA           NA
## b2                              NA            NA           NA           NA
## b2_slope                        NA            NA           NA           NA
## quadratic_term                  NA            NA           NA           NA
## a1                              NA            NA   0.47552463   0.79227687
## a1_slope                        NA            NA           NA   9.56548081
## a2                              NA            NA           NA           NA
## a2_slope                        NA            NA           NA           NA
## Quadratic_c                     NA            NA           NA           NA
## Quadratic_b                     NA            NA           NA           NA
## Quadratic_a                     NA            NA           NA           NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.00000000
##                      OU_linear_beta
## logLik                 27.035362151
## n                       3.000000000
## AIC                   -48.070724301
## AICc                  -47.537390968
## b1                      0.012948536
## b1_slope                0.005395806
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                      0.536630319
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
g9<-evodat_parallel_patry$overlap_proportion
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                           BM_null   BM_linear     OU_null   OU_linear
## logLik                3.721180439  5.94298737  5.52282866  6.65667626
## n                     1.000000000  2.00000000  2.00000000  4.00000000
## AIC                  -5.442360879 -7.88597474 -7.04565731 -5.31335252
## AICc                 -5.207066761 -7.13597474 -6.29565731 -2.45620966
## b1                    0.009166952  0.01283979  0.02074476  0.01856456
## b1_slope                       NA -0.01239191          NA -0.01068431
## breakpoint                     NA          NA          NA          NA
## b2                             NA          NA          NA          NA
## b2_slope                       NA          NA          NA          NA
## quadratic_term                 NA          NA          NA          NA
## a1                             NA          NA  0.42367374  0.09073558
## a1_slope                       NA          NA          NA  0.47205023
## a2                             NA          NA          NA          NA
## a2_slope                       NA          NA          NA          NA
## Quadratic_c                    NA          NA          NA          NA
## Quadratic_b                    NA          NA          NA          NA
## Quadratic_a                    NA          NA          NA          NA
## nlm_termination_code  1.000000000  1.00000000  1.00000000  1.00000000
##                      OU_linear_beta
## logLik                    6.3022944
## n                         3.0000000
## AIC                      -6.6045889
## AICc                     -5.0045889
## b1                        0.0154846
## b1_slope                 -0.0101000
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                        0.1986047
## a1_slope                         NA
## a2                               NA
## a2_slope                         NA
## Quadratic_c                      NA
## Quadratic_b                      NA
## Quadratic_a                      NA
## nlm_termination_code      1.0000000
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
##                           BM_null     BM_linear       OU_null
## logLik                22.69165321  23.003396755  26.206246782
## n                      1.00000000   2.000000000   2.000000000
## AIC                  -43.38330641 -42.006793511 -48.412493563
## AICc                 -43.24044927 -41.562349066 -47.968049119
## b1                     0.00280508   0.002936023   0.008467075
## b1_slope                       NA  -0.003278569            NA
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
## nlm_termination_code   1.00000000   1.000000000   1.000000000
##                          OU_linear OU_linear_beta
## logLik                26.320060536   26.297914090
## n                      4.000000000    3.000000000
## AIC                  -44.640121073  -46.595828181
## AICc                 -43.040121073  -45.672751258
## b1                     0.009555487    0.009555235
## b1_slope               0.085081663    0.013306910
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.894407587    0.862332292
## a1_slope               4.220495469             NA
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
## [1] -55.89277
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -61.95347
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -61.20042
```

