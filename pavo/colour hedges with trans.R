
library(effsize)
#spmerged is the file with all of the song data for each individual along with their pair numbers, etc.
setwd("C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Cohen and Hedge")
kiraspmerged<-read.csv("individual_colour_data.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","") )

## trans colour vars with arcsine cuz they're proportions

instanceconvert <- colnames(kiraspmerged[5:32])

for (i in instanceconvert)
{
  kiraspmerged[[i]] <- asin(kiraspmerged[[i]])
}

##############Hedge's G

list2<-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50) #all pairs
#list2<-c(1,3,4,5,6,7,8,9,11,13,14,15,16,17,18,19,20,21,22,24,27,28,29,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45,46,47,48,49,50) #normal pairs
#list2<-c(10,12,23,25,26) #just wing pairs
#list2<-c(39) #AMREs

list2 <- as.data.frame(list2, stringsAsFactors = FALSE)
#cohen's d columns
list2["crown_u"]<-NA
list2["crown_s"]<-NA
list2["crown_m"]<-NA
list2["crown_l"]<-NA
list2["back_u"]<-NA
list2["back_s"]<-NA
list2["back_m"]<-NA
list2["back_l"]<-NA
list2["rump_u"]<-NA
list2["rump_s"]<-NA
list2["rump_m"]<-NA
list2["rump_l"]<-NA
list2["tail_u"]<-NA
list2["tail_s"]<-NA
list2["tail_m"]<-NA
list2["tail_l"]<-NA
list2["throat_u"]<-NA
list2["throat_s"]<-NA
list2["throat_m"]<-NA
list2["throat_l"]<-NA
list2["chest_u"]<-NA
list2["chest_s"]<-NA
list2["chest_m"]<-NA
list2["chest_l"]<-NA
list2["wing_u"]<-NA
list2["wing_s"]<-NA
list2["wing_m"]<-NA
list2["wing_l"]<-NA

#cohen's d loop

for(i in seq(from=1, to=49,by=1))
{
  sub<-subset(kiraspmerged,kiraspmerged$Pair==list2$list2[i])
  
  a<-cohen.d(sub$crown_u,sub$X,hedges.correction=TRUE) #X is the column which says species A or B for the pair
  b<-cohen.d(sub$crown_s,sub$X,hedges.correction=TRUE)
  c<-cohen.d(sub$crown_m,sub$X,hedges.correction=TRUE)
  d<-cohen.d(sub$crown_l,sub$X,hedges.correction=TRUE)
  e<-cohen.d(sub$back_u,sub$X,hedges.correction=TRUE)
  f<-cohen.d(sub$back_s,sub$X,hedges.correction=TRUE)
  g<-cohen.d(sub$back_m,sub$X,hedges.correction=TRUE)
  h<-cohen.d(sub$back_l,sub$X,hedges.correction=TRUE)
  ii<-cohen.d(sub$rump_u,sub$X,hedges.correction=TRUE) #changed from i to ii - leaving as i causes problems with loop???
  j<-cohen.d(sub$rump_s,sub$X,hedges.correction=TRUE)
  k<-cohen.d(sub$rump_m,sub$X,hedges.correction=TRUE)
  l<-cohen.d(sub$rump_l,sub$X,hedges.correction=TRUE)
  m<-cohen.d(sub$tail_u,sub$X,hedges.correction=TRUE)
  n<-cohen.d(sub$tail_s,sub$X,hedges.correction=TRUE)
  o<-cohen.d(sub$tail_m,sub$X,hedges.correction=TRUE)
  p<-cohen.d(sub$tail_l,sub$X,hedges.correction=TRUE)
  q<-cohen.d(sub$throat_u,sub$X,hedges.correction=TRUE)
  r<-cohen.d(sub$throat_s,sub$X,hedges.correction=TRUE)
  s<-cohen.d(sub$throat_m,sub$X,hedges.correction=TRUE)
  t<-cohen.d(sub$throat_l,sub$X,hedges.correction=TRUE)
  u<-cohen.d(sub$chest_u,sub$X,hedges.correction=TRUE)
  v<-cohen.d(sub$chest_s,sub$X,hedges.correction=TRUE)
  w<-cohen.d(sub$chest_m,sub$X,hedges.correction=TRUE)
  x<-cohen.d(sub$chest_l,sub$X,hedges.correction=TRUE)
  y<-cohen.d(sub$wing_u,sub$X,hedges.correction=TRUE)
  z<-cohen.d(sub$wing_s,sub$X,hedges.correction=TRUE)
  aa<-cohen.d(sub$wing_m,sub$X,hedges.correction=TRUE)
  bb<-cohen.d(sub$wing_l,sub$X,hedges.correction=TRUE)
  
  list2$crown_u[i]<-a$estimate
  list2$crown_s[i]<-b$estimate
  list2$crown_m[i]<-c$estimate
  list2$crown_l[i]<-d$estimate
  list2$back_u[i]<-e$estimate
  list2$back_s[i]<-f$estimate
  list2$back_m[i]<-g$estimate
  list2$back_l[i]<-h$estimate
  list2$rump_u[i]<-ii$estimate
  list2$rump_s[i]<-j$estimate
  list2$rump_m[i]<-k$estimate
  list2$rump_l[i]<-l$estimate
  list2$tail_u[i]<-m$estimate
  list2$tail_s[i]<-n$estimate
  list2$tail_m[i]<-o$estimate
  list2$tail_l[i]<-p$estimate
  list2$throat_u[i]<-q$estimate
  list2$throat_s[i]<-r$estimate
  list2$throat_m[i]<-s$estimate
  list2$throat_l[i]<-t$estimate
  list2$chest_u[i]<-u$estimate
  list2$chest_s[i]<-v$estimate
  list2$chest_m[i]<-w$estimate
  list2$chest_l[i]<-x$estimate
  list2$wing_u[i]<-y$estimate
  list2$wing_s[i]<-z$estimate
  list2$wing_m[i]<-aa$estimate
  list2$wing_l[i]<-bb$estimate
  
}

###############merge files

library("plyr")
#list<-rename(list, c("list"="Pair")) #rename species column back to species
list2<-rename(list2,c("list2"="Pair"))

#GDcolour<-merge(list,list2,by="Pair",suffixes=c("_cohens","_hedges"))

write.csv(list2, "C:/Users/Kira Delmore/Dropbox/Haley and Kira's Comparative Analysis Extravaganza/Cohen and Hedge/GD_Colour.csv")

##################