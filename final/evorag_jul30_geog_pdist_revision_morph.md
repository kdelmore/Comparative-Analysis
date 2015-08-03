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
#evodat["trait"]<-evodat$avg_colour10
evodat["trait"]<-evodat$avg_morph10
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
g9<-evodat$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                            BM_null     BM_linear      OU_null   OU_linear
## logLik                12.674010752  13.044583480  16.93935105  20.2218526
## n                      1.000000000   2.000000000   2.00000000   4.0000000
## AIC                  -23.348021504 -22.089166959 -29.87870210 -32.4437053
## AICc                 -23.262915121 -21.828297394 -29.61783254 -31.5346144
## b1                     0.007778549   0.008596266   0.02132601   0.7151628
## b1_slope                        NA  -0.004556975           NA   5.3670073
## breakpoint                      NA            NA           NA          NA
## b2                              NA            NA           NA          NA
## b2_slope                        NA            NA           NA          NA
## quadratic_term                  NA            NA           NA          NA
## a1                              NA            NA   0.55532483  48.8246687
## a1_slope                        NA            NA           NA  20.1275392
## a2                              NA            NA           NA          NA
## a2_slope                        NA            NA           NA          NA
## Quadratic_c                     NA            NA           NA          NA
## Quadratic_b                     NA            NA           NA          NA
## Quadratic_a                     NA            NA           NA          NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.0000000
##                      OU_linear_beta
## logLik                   20.1910268
## n                         3.0000000
## AIC                     -34.3820536
## AICc                    -33.8487203
## b1                        0.1701720
## b1_slope                  0.9827701
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       11.2931392
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
g9<-evodat_parallel_patry$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                          BM_null   BM_linear     OU_null     OU_linear
## logLik                1.81709758  3.79809764   9.3430399  9.615837e+00
## n                     1.00000000  2.00000000   2.0000000  4.000000e+00
## AIC                  -1.63419515 -3.59619527 -14.6860798 -1.123167e+01
## AICc                 -1.39890103 -2.84619527 -13.9360798 -8.374531e+00
## b1                    0.01120135  0.01488159   0.1445323  4.703912e-03
## b1_slope                      NA -0.01328120          NA  3.300083e+00
## breakpoint                    NA          NA          NA            NA
## b2                            NA          NA          NA            NA
## b2_slope                      NA          NA          NA            NA
## quadratic_term                NA          NA          NA            NA
## a1                            NA          NA   6.4260147  1.006121e-06
## a1_slope                      NA          NA          NA  1.448354e+02
## a2                            NA          NA          NA            NA
## a2_slope                      NA          NA          NA            NA
## Quadratic_c                   NA          NA          NA            NA
## Quadratic_b                   NA          NA          NA            NA
## Quadratic_a                   NA          NA          NA            NA
## nlm_termination_code  1.00000000  2.00000000   1.0000000  1.000000e+00
##                      OU_linear_beta
## logLik                    9.3493582
## n                         3.0000000
## AIC                     -12.6987163
## AICc                    -11.0987163
## b1                        0.1358725
## b1_slope                 -0.0181450
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                        5.7139927
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
g10 <-evodat_perp_patry$overlap_proportion_hz_info
fit_perp_patry <- model.test.sisters(e10,t10,g10,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t10,e10,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp_patry) #View model parameters
```

```
##                            BM_null     BM_linear       OU_null
## logLik                12.292816504  12.998809238  12.311903595
## n                      1.000000000   2.000000000   2.000000000
## AIC                  -22.585633008 -21.997618476 -20.623807190
## AICc                 -22.442775865 -21.553174031 -20.179362745
## b1                     0.005610773   0.004711806   0.005943889
## b1_slope                        NA   0.010524828            NA
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
## nlm_termination_code   1.000000000   2.000000000   1.000000000
##                         OU_linear OU_linear_beta
## logLik                14.84309659    14.74453970
## n                      4.00000000     3.00000000
## AIC                  -21.68619319   -23.48907939
## AICc                 -20.08619319   -22.56600247
## b1                     0.01020582     0.01221565
## b1_slope               0.11070409     0.15657782
## breakpoint                     NA             NA
## b2                             NA             NA
## b2_slope                       NA             NA
## quadratic_term                 NA             NA
## a1                     0.58035594     0.76358166
## a1_slope              -0.10009997             NA
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
## [1] -31.59381
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -44.91787
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -44.1878
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
## logLik                11.517677698  14.98132302
## n                      1.000000000   2.00000000
## AIC                  -21.035355395 -25.96264605
## AICc                 -20.944446304 -25.68357628
## b1                     0.008111741   0.01840354
## b1_slope                        NA           NA
## breakpoint                      NA           NA
## b2                              NA           NA
## b2_slope                        NA           NA
## quadratic_term                  NA           NA
## a1                              NA   0.41903738
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
##                          BM_null     OU_null
## logLik                1.87525561   9.3363520
## n                     1.00000000   2.0000000
## AIC                  -1.75051121 -14.6727041
## AICc                 -1.51521709 -13.9227041
## b1                    0.01108907   0.1528627
## b1_slope                      NA          NA
## breakpoint                    NA          NA
## b2                            NA          NA
## b2_slope                      NA          NA
## quadratic_term                NA          NA
## a1                            NA   6.8172372
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
t3<-evodat_perp$speed_coi_100
l3<-evodat_perp$mass_avg
fit_perp <- model.test.sisters(e3,t3,l3,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t3,e3,xlim=c(0,10),ylim=c(0,1),main="divide")
print(fit_perp) #View model parameters
```

```
##                            BM_null       OU_null
## logLik                10.705974139  1.070597e+01
## n                      1.000000000  2.000000e+00
## AIC                  -19.411948277 -1.741195e+01
## AICc                 -19.251948277 -1.691195e+01
## b1                     0.006016585  6.016190e-03
## b1_slope                        NA            NA
## breakpoint                      NA            NA
## b2                              NA            NA
## b2_slope                        NA            NA
## quadratic_term                  NA            NA
## a1                              NA  1.105969e-06
## a1_slope                        NA            NA
## a2                              NA            NA
## a2_slope                        NA            NA
## Quadratic_c                     NA            NA
## Quadratic_b                     NA            NA
## Quadratic_a                     NA            NA
## nlm_termination_code   1.000000000  2.000000e+00
```

```r
## calculate AIC for perp/par
logLike_BM<-fit_par[1,1]+fit_perp[1,1]
AIC_BM<-2*1-2*logLike_BM
print(AIC_BM)
```

```
## [1] -23.16246
```

```r
logLike_OU<-fit_par[1,2]+fit_perp[1,2]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -36.08465
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
##                            BM_null     BM_linear      OU_null   OU_linear
## logLik                11.517677698  11.911380845  14.98132302  17.7660578
## n                      1.000000000   2.000000000   2.00000000   4.0000000
## AIC                  -21.035355395 -19.822761690 -25.96264605 -27.5321156
## AICc                 -20.944446304 -19.543691922 -25.68357628 -26.5565059
## b1                     0.008111741   0.009042660   0.01840354   0.7012834
## b1_slope                        NA  -0.005024034           NA   6.1061289
## breakpoint                      NA            NA           NA          NA
## b2                              NA            NA           NA          NA
## b2_slope                        NA            NA           NA          NA
## quadratic_term                  NA            NA           NA          NA
## a1                              NA            NA   0.41903738  46.3430819
## a1_slope                        NA            NA           NA  32.0689495
## a2                              NA            NA           NA          NA
## a2_slope                        NA            NA           NA          NA
## Quadratic_c                     NA            NA           NA          NA
## Quadratic_b                     NA            NA           NA          NA
## Quadratic_a                     NA            NA           NA          NA
## nlm_termination_code   1.000000000   2.000000000   1.00000000   1.0000000
##                      OU_linear_beta
## logLik                   17.7012658
## n                         3.0000000
## AIC                     -29.4025317
## AICc                    -28.8311031
## b1                        0.1605178
## b1_slope                  0.9316062
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       10.1556782
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
t9<-evodat_parallel_patry$speed_coi_100
g9<-evodat_parallel_patry$overlap_proportion_hz_info
fit_par_patry <- model.test.sisters(e9,t9,g9,GRAD2=NULL,meserr1=0,meserr2=0,models,starting=NULL,Beta_starting=NULL,Alpha_starting=NULL)
#plot(t9,e9,xlim=c(0,10),ylim=c(0,1),main="no divide")
print(fit_par_patry) #View model parameters
```

```
##                          BM_null   BM_linear     OU_null     OU_linear
## logLik                1.87525561  3.83364350   9.3363520  9.607534e+00
## n                     1.00000000  2.00000000   2.0000000  4.000000e+00
## AIC                  -1.75051121 -3.66728700 -14.6727041 -1.121507e+01
## AICc                 -1.51521709 -2.91728700 -13.9227041 -8.357925e+00
## b1                    0.01108907  0.01473674   0.1528627  4.700108e-03
## b1_slope                      NA -0.01312099          NA  3.229132e+00
## breakpoint                    NA          NA          NA            NA
## b2                            NA          NA          NA            NA
## b2_slope                      NA          NA          NA            NA
## quadratic_term                NA          NA          NA            NA
## a1                            NA          NA   6.8172372  1.300963e-06
## a1_slope                      NA          NA          NA  1.418180e+02
## a2                            NA          NA          NA            NA
## a2_slope                      NA          NA          NA            NA
## Quadratic_c                   NA          NA          NA            NA
## Quadratic_b                   NA          NA          NA            NA
## Quadratic_a                   NA          NA          NA            NA
## nlm_termination_code  1.00000000  1.00000000   1.0000000  1.000000e+00
##                      OU_linear_beta
## logLik                   9.34151244
## n                        3.00000000
## AIC                    -12.68302487
## AICc                   -11.08302487
## b1                       0.14205852
## b1_slope                -0.01739632
## breakpoint                       NA
## b2                               NA
## b2_slope                         NA
## quadratic_term                   NA
## a1                       6.01896695
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
## logLik                10.705974139  11.505946242  1.070597e+01
## n                      1.000000000   2.000000000  2.000000e+00
## AIC                  -19.411948277 -19.011892484 -1.741195e+01
## AICc                 -19.251948277 -18.511892484 -1.691195e+01
## b1                     0.006016585   0.004927097  6.016190e-03
## b1_slope                        NA   0.013494011            NA
## breakpoint                      NA            NA            NA
## b2                              NA            NA            NA
## b2_slope                        NA            NA            NA
## quadratic_term                  NA            NA            NA
## a1                              NA            NA  1.105969e-06
## a1_slope                        NA            NA            NA
## a2                              NA            NA            NA
## a2_slope                        NA            NA            NA
## Quadratic_c                     NA            NA            NA
## Quadratic_b                     NA            NA            NA
## Quadratic_a                     NA            NA            NA
## nlm_termination_code   1.000000000   1.000000000  2.000000e+00
##                          OU_linear OU_linear_beta
## logLik                12.878832120   12.750579413
## n                      4.000000000    3.000000000
## AIC                  -17.757664241  -19.501158827
## AICc                 -15.939482423  -18.457680566
## b1                     0.008597366    0.009748347
## b1_slope               0.086918408    0.121604189
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.396030464    0.503656235
## a1_slope              -0.100100000             NA
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
## [1] -28.67918
```

```r
logLike_OU<-fit_par_patry[1,4]+fit_perp_patry[1,4]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -40.97273
```

```r
## calculate AIC for perp/par with patry and using _linear_beta models
logLike_OU<-fit_par_patry[1,5]+fit_perp_patry[1,5]
AIC_OU<-2*2-2*logLike_OU
print(AIC_OU)
```

```
## [1] -40.18418
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
## logLik                11.464131116  1.485697e+01  15.02962912
## n                      1.000000000  2.000000e+00   2.00000000
## AIC                  -20.928262232 -2.571394e+01 -26.05925823
## AICc                 -20.837353141 -2.543487e+01 -25.78018846
## b1                     0.008237548  2.919240e-03   0.01908006
## b1_slope                        NA  2.457382e-04           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.43820904
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000  1.000000e+00   1.00000000
##                          OU_linear OU_linear_beta
## logLik                1.884623e+01   1.843180e+01
## n                     4.000000e+00   3.000000e+00
## AIC                  -2.969245e+01  -3.086360e+01
## AICc                 -2.871684e+01  -3.029217e+01
## b1                    7.052118e-03   4.884194e-03
## b1_slope              4.630297e-04   6.983064e-04
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                    6.081894e-01   4.415386e-01
## a1_slope             -1.117982e-02             NA
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
## logLik                11.464131116  12.485675447  15.02962912
## n                      1.000000000   2.000000000   2.00000000
## AIC                  -20.928262232 -20.971350893 -26.05925823
## AICc                 -20.837353141 -20.692281126 -25.78018846
## b1                     0.008237548   0.006151958   0.01908006
## b1_slope                        NA   0.005049170           NA
## breakpoint                      NA            NA           NA
## b2                              NA            NA           NA
## b2_slope                        NA            NA           NA
## quadratic_term                  NA            NA           NA
## a1                              NA            NA   0.43820904
## a1_slope                        NA            NA           NA
## a2                              NA            NA           NA
## a2_slope                        NA            NA           NA
## Quadratic_c                     NA            NA           NA
## Quadratic_b                     NA            NA           NA
## Quadratic_a                     NA            NA           NA
## nlm_termination_code   1.000000000   1.000000000   1.00000000
##                          OU_linear OU_linear_beta
## logLik                20.012035792   15.069457352
## n                      4.000000000    3.000000000
## AIC                  -32.024071585  -24.138914703
## AICc                 -31.048461829  -23.567486132
## b1                     0.006205505    0.021412591
## b1_slope               0.138327591   -0.002493817
## breakpoint                      NA             NA
## b2                              NA             NA
## b2_slope                        NA             NA
## quadratic_term                  NA             NA
## a1                     0.002721359    0.487582295
## a1_slope               6.423332328             NA
## a2                              NA             NA
## a2_slope                        NA             NA
## Quadratic_c                     NA             NA
## Quadratic_b                     NA             NA
## Quadratic_a                     NA             NA
## nlm_termination_code   1.000000000    1.000000000
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
#evodat["trait"]<-evodat$avg_colour10
evodat["trait"]<-evodat$avg_morph10

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

