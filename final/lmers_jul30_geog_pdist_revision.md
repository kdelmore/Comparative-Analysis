# lmers_jul30_geog_pdist_revision
kira_delmore  
`r Sys.Date()`  

Load libraries and deal with data


```r
library(lme4) 
```

```
## Loading required package: Matrix
## Loading required package: Rcpp
```

```r
library(MuMIn)
library(nlme)
```

```
## 
## Attaching package: 'nlme'
## 
## The following object is masked from 'package:lme4':
## 
##     lmList
```

```r
library(visreg)

setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Analysis")
comp <- read.csv("../working files/pair_hedges_avg_jul30.csv",stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))

comp$overlap_proportion_log <- log(1+comp$overlap_proportion_hz_info) ## log the skewed variables
comp$breed_lat_avg_log <- log(comp$breed_lat_avg)
comp$distance_avg_log <- log(comp$distance_avg)
comp$p_distance_log <- log(comp$p_distance)
comp$p_distance_log <- log(comp$p_distance)
comp$mass_avg_log <- log(comp$mass_avg)
comp$avg_log <- log(comp$avg_2)
comp$avg_morph_log <- log(comp$avg_morph)
comp$avg_song_log <- log(comp$avg_song)
comp$avg_colour_log <- log(comp$avg_colour)
comp$speed_coi_log<-log(comp$speed_coi)

comp$fam<-as.factor(comp$family) ## convert characters to factors
comp$sp1_genus<-as.factor(comp$sp1_genus)
comp$migration_category<-as.factor(comp$migration_category)
```

All traits together


```r
Global_Personal.Use<-lmer(avg_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                   Estimate Std. Error Adjusted SE
## (Intercept)                      1.7325460 1.26969677  1.28645654
## migration_categoryperpendicular -0.4373307 0.10624373  0.10930561
## p_distance_log                   0.2228987 0.04707131  0.04845963
## mass_avg_log                     0.2326687 0.09811735  0.10104473
## breed_lat_avg_log               -0.5102102 0.35246092  0.36296851
##                                    Lower CI   Upper CI
## (Intercept)                     -0.78886246  4.2539545
## migration_categoryperpendicular -0.65156575 -0.2230956
## p_distance_log                   0.12791961  0.3178779
## mass_avg_log                     0.03462469  0.4307127
## breed_lat_avg_log               -1.22161537  0.2011950
```

Each set of traits separately


```r
#### colour

Global_Personal.Use<-lmer(avg_colour_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                   Estimate Std. Error Adjusted SE
## (Intercept)                      1.1818458 1.69086004  1.71790065
## migration_categoryperpendicular -0.4466621 0.17970930  0.18484300
## p_distance_log                   0.2430674 0.08492907  0.08713091
## overlap_proportion_log           1.0977076 0.61319399  0.62508188
## mass_avg_log                     0.3012581 0.16988486  0.17481217
## breed_lat_avg_log               -0.5225293 0.61111630  0.62903179
##                                    Lower CI    Upper CI
## (Intercept)                     -2.18517762  4.54886917
## migration_categoryperpendicular -0.80894769 -0.08437644
## p_distance_log                   0.07229393  0.41384083
## overlap_proportion_log          -0.12743039  2.32284555
## mass_avg_log                    -0.04136746  0.64388366
## breed_lat_avg_log               -1.75540896  0.71035037
```

```r
#### song

Global_Personal.Use<-lmer(avg_song_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                     Estimate Std. Error Adjusted SE
## (Intercept)                     -0.005681832 1.59309685   1.6163633
## migration_categoryperpendicular -0.581389895 0.16714608   0.1717834
## p_distance_log                   0.185715113 0.07270479   0.0747764
## distance_avg_log                 0.299275619 0.16623948   0.1710510
## overlap_proportion_log          -0.477959886 0.54570636   0.5618879
## breed_lat_avg_log                0.433130995 0.52806432   0.5435082
##                                    Lower CI   Upper CI
## (Intercept)                     -3.17369563  3.1623320
## migration_categoryperpendicular -0.91807922 -0.2447006
## p_distance_log                   0.03915606  0.3322742
## distance_avg_log                -0.03597824  0.6345295
## overlap_proportion_log          -1.57923990  0.6233201
## breed_lat_avg_log               -0.63212552  1.4983875
```

```r
#### morph

Global_Personal.Use<-lmer(avg_morph_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+p_distance_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                          Estimate Std. Error Adjusted SE    Lower CI
## (Intercept)             5.2806503 2.20556918  2.26458966  0.84213612
## breed_lat_avg_log      -1.2756320 0.56182106  0.57766123 -2.40782716
## overlap_proportion_log  0.7136964 0.46768060  0.47971939 -0.22653632
## p_distance_log          0.1811957 0.07569129  0.07782483  0.02866183
##                          Upper CI
## (Intercept)             9.7191645
## breed_lat_avg_log      -0.1434367
## overlap_proportion_log  1.6539291
## p_distance_log          0.3337295
```

CORRECTING FOR DIFFERENCES IN TIME OF DIVERGENCE ESTIMATED FROM CYTB VS COI


```r
comp<-subset(comp,comp$mtdna_type!="no")
```

All traits together


```r
Global_Personal.Use<-lmer(avg_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+speed_coi_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                   Estimate Std. Error Adjusted SE
## (Intercept)                      1.1356278 0.38077529  0.38613584
## migration_categoryperpendicular -0.3851620 0.10270164  0.10593160
## speed_coi_log                    0.2251081 0.04548365  0.04691389
## mass_avg_log                     0.2091629 0.09705304  0.10015914
##                                    Lower CI   Upper CI
## (Intercept)                      0.37881543  1.8924401
## migration_categoryperpendicular -0.59278413 -0.1775399
## speed_coi_log                    0.13315857  0.3170576
## mass_avg_log                     0.01285459  0.4054712
```

Each set of traits separately


```r
#### colour

Global_Personal.Use<-lmer(avg_colour_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+speed_coi_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                   Estimate Std. Error Adjusted SE
## (Intercept)                      1.2530694 0.33750982  0.34578829
## speed_coi_log                    0.2666304 0.07270109  0.07486804
## migration_categoryperpendicular -0.3165279 0.15413384  0.15893971
## overlap_proportion_log           0.5580700 0.43710389  0.45073273
##                                   Lower CI     Upper CI
## (Intercept)                      0.5753368  1.930801976
## speed_coi_log                    0.1198918  0.413369110
## migration_categoryperpendicular -0.6280440 -0.005011767
## overlap_proportion_log          -0.3253499  1.441489935
```

```r
#### song

Global_Personal.Use<-lmer(avg_song_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+speed_coi_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                                    Estimate Std. Error Adjusted SE
## (Intercept)                     -0.08385862 1.61618353  1.63684737
## migration_categoryperpendicular -0.58947225 0.17220184  0.17725958
## speed_coi_log                    0.18266889 0.07686569  0.07914048
## distance_avg_log                 0.33354501 0.18001733  0.18563025
## breed_lat_avg_log                0.29501955 0.55935139  0.57679189
## overlap_proportion_log          -0.62255823 0.54287473  0.56024896
##                                    Lower CI   Upper CI
## (Intercept)                     -3.29202052  3.1243033
## migration_categoryperpendicular -0.93689463 -0.2420499
## speed_coi_log                    0.02755640  0.3377814
## distance_avg_log                -0.03028359  0.6973736
## breed_lat_avg_log               -0.83547179  1.4255109
## overlap_proportion_log          -1.72062602  0.4755096
```

```r
#### morph

Global_Personal.Use<-lmer(avg_morph_log ~migration_category+overlap_proportion_log+breed_lat_avg_log+distance_avg_log+speed_coi_log+mass_avg_log + (1 | family/sp1_genus),data=comp,na.action = na.pass)
All_models_Personal.Use<-dredge(Global_Personal.Use, extra = "R^2") # all possible combinations of fixed effects
```

```
## Warning in dredge(Global_Personal.Use, extra = "R^2"): comparing models
## fitted by REML
```

```
## Fixed term is "(Intercept)"
```

```r
Sub_All_models_Personal.Use<-subset(All_models_Personal.Use, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.Personal.Use<-model.avg(All_models_Personal.Use, subset=delta<2) # model averaging
Avg.Personal.Use$coefTable # parameter estimates and SEs from average model
```

```
##                          Estimate Std. Error Adjusted SE    Lower CI
## (Intercept)             4.1888550 2.67888769  2.72789823 -1.15772729
## breed_lat_avg_log      -1.1363649 0.58538710  0.60334145 -2.31889243
## speed_coi_log           0.1997395 0.07859419  0.08099424  0.04099368
## overlap_proportion_log  0.7614477 0.49456415  0.50740224 -0.23304237
##                          Upper CI
## (Intercept)            9.53543729
## breed_lat_avg_log      0.04616259
## speed_coi_log          0.35848526
## overlap_proportion_log 1.75593787
```
