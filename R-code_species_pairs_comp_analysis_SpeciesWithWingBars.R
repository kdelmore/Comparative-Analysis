###calculating species colouration in tetrahedral space and calculating Eucledian distance

##NOTE- Ammended file for the 5 species with two wing regions. Before running, create two subdirectories
#Normal - which has all regions but wing, and "WINGONLY" which has both wing regions.

rm (list = ls ( ) )

#setwd("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/PineWarber_YellowThroatedWarbler")
#set the working directory to the one you'd currently like
library(pavo)

###bring in all files in the working directory 
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)

###Alternative would be to call each species separately and then merge them, like so:
#specs.Pine<-getspec(where ='./Pine Warblers', ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)
#specs.Yellow<-getspec(where ='./Yellow Throated Warblers', ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)
#specs.new<-merge(specs.Pine,specs.Yellow)

#quick check to see what the data look like
explorespec(specs[,1:400],by=5,lwd=2)

###This line averages the reflecta over a certain number. Since here we have 5 measures per region,
###We set 'by = 5'. The output will now trim the column names down so that it's (for example for back) Species_Individual_FMNH_Individual.1
mspecs<-aggspec(specs,by=5,FUN=mean)
#for reference:  _(blank) = crown (specs 1-5), .1 = back (specs 6-10), .2 = rump (secs 11-15)
# .3 = tail (specs 16-20), .4 = throat (specs 21-25), .5 = chest (specs 26-30), .6 = wing (specs 31-35)

#Now want want to get one average measure (all individuals) for each species (for each region)
#First thing is to create a species name/region vector (could only figure out how to do it one by one)

spp<-gsub(".\\d+.[A-z]+_\\d+\\.1$",'.zzback',names(mspecs),perl=T) #got it
spp2<-gsub(".\\d+.[A-z]+_\\d+\\.2$",'.zzrump',spp,perl=T) 
spp3<-gsub(".\\d+.[A-z]+_\\d+\\.3$",'.zztail',spp2,perl=T) 
spp4<-gsub(".\\d+.[A-z]+_\\d+\\.4$",'.zzthroat',spp3,perl=T)
spp5<-gsub(".\\d+.[A-z]+_\\d+\\.5$",'.zzchest',spp4,perl=T)
spp6<-gsub(".\\d+.[A-z]+_\\d+",'.zzcrown',spp5,perl=T)

table(spp6)

#now to average by species (and region). Instead of averaging by a number, we average by the species/region vector we just
#made. MAKE SURE YOU USE THE LAST ONE (spp7)
sppspec<-aggspec(mspecs,by=spp6,FUN=mean)
round(sppspec[1:5,],2) #just to make sure it worked out
explorespec(sppspec,by=6,lwd=3)

#explore the raw data to see if a smoothing term should be applied
plotsmooth(sppspec,minsmooth=0.05,maxsmooth=0.5,curves=4,ask=F)
#apply a span of 0.2 to smooth out noise
spec.sm<-procspec(sppspec,opt='smooth',span=0.2)
explorespec(spec.sm,by=6,lwd=3)



#calculate photon catches at each photoreceptor, here using the average avian UV visual system 
vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)

#calculate position in tetra-hedral colour space (see Stoddard & Prum 2008 for details)
tcs.all<-tcs(vm.spec.sm)

#Euclidean distances of the relative cone stimulation data
dist(tcs.all)

#Euclidean distance of the relative cone stimulation data (by region)
#Euc.crown<-dist(subset(vm.spec.sm,"crown")[,1:4])  #alternate way of doing it, stick with below for clarity of which variables involved

Euc.crown<-dist(subset(tcs.all,"zzcrown")[,c('u','s','m','l')])
Euc.back<-dist(subset(tcs.all,"zzback")[,c('u','s','m','l')])
Euc.rump<-dist(subset(tcs.all,"zzrump")[,c('u','s','m','l')])
Euc.tail<-dist(subset(tcs.all,"zztail")[,c('u','s','m','l')])
Euc.throat<-dist(subset(tcs.all,"zzthroat")[,c('u','s','m','l')])
Euc.chest<-dist(subset(tcs.all,"zzchest")[,c('u','s','m','l')])

#string all of the values together 
out <- list(Euc.crown,Euc.back,Euc.rump,Euc.tail,Euc.throat,Euc.chest)
#convert from a list to a vector, add the mean
out2 <- unlist(out)
Eucledian.distance<-c(out2,(mean(out2)))

#To export as csv, first add the row names for each region, then get the workding directory and name the output after that
export<-as.data.frame(Eucledian.distance, row.names = c("crown","back","rump","tail","throat","chest","Average"), optional = FALSE)
export
one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/26_Bobolink_Yellow-headedBlackbird/","26_Bobolink_Yellow-headedBlackbird",one) 
write.csv(export,file = paste(two,"distances.csv",sep="_"))

#_____________________________________________________________________________
#Close it down, re-open R, load pavo, Select Wing-only as the working directory

library(pavo)
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)

explorespec(specs[,1:120],by=3,lwd=2)

mspecs<-aggspec(specs,by=3,FUN=mean)

spp<-gsub(".\\d+.[A-z]+_\\d+\\.1$",'.zzwingcolour',names(mspecs),perl=T)
spp2<-gsub(".\\d+.[A-z]+_\\d+",'.zzwingblack',spp,perl=T)

table(spp2)

sppspec<-aggspec(mspecs,by=spp2,FUN=mean)
round(sppspec[1:5,],2) 
explorespec(sppspec,by=6,lwd=3)

spec.sm<-procspec(sppspec,opt='smooth',span=0.2)
explorespec(spec.sm,by=6,lwd=3)

vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)
tcs.all<-tcs(vm.spec.sm)

Euc.black<-dist(subset(tcs.all,"zzwingblack")[,c('u','s','m','l')])
Euc.colour<-dist(subset(tcs.all,"zzwingcolour")[,c('u','s','m','l')])

out <- list(Euc.black,Euc.colour)

out2 <- unlist(out)
Eucledian.distance<-c(out2,(mean(out2)))
export<-as.data.frame(Eucledian.distance, row.names = c("Wing_Black","Wing_Colour","Average"), optional = FALSE)
export
one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/26_Bobolink_Yellow-headedBlackbird/","26_Bobolink_Yellow-headedBlackbird",one) 

write.csv(export,file = paste(two,"distances.csv",sep="_"))
