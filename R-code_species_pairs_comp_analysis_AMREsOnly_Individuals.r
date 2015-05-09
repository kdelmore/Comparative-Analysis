#############Kira. Use this code for the "NORMAL" files

rm (list = ls ( ) )

#setwd("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/PineWarber_YellowThroatedWarbler")
#set the working directory to the one you'd currently like
library(pavo)

###bring in all files in the working directory 
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)

#
###This line averages the reflecta over a certain number. Since here we have 5 measures per region,
###We set 'by = 5'. The output will now trim the column names down so that it's (for example for back) Species_Individual_FMNH_Individual.1
mspecs<-aggspec(specs,by=5,FUN=mean)
#for reference:  _(blank) = crown (specs 1-5), .1 = back (specs 6-10), .2 = rump (secs 11-15)
# .3 = tail (specs 16-20), .4 = throat (specs 21-25), .5 = chest (specs 26-30), .6 = wing (specs 31-35)

#

#
#apply a span of 0.2 to smooth out noise
spec.sm<-procspec(mspecs,opt='smooth',span=0.2)
explorespec(spec.sm,by=6,lwd=3)



#calculate photon catches at each photoreceptor, here using the average avian UV visual system 
vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)

#calculate position in tetra-hedral colour space (see Stoddard & Prum 2008 for details)
tcs.all<-tcs(vm.spec.sm)


one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/39_American-redstart_HoodedWarbler/","39_American-redstart_HoodedWarbler",one) 
write.csv(tcs.all,file = paste(two,"Individual_values.csv",sep="_"))

#_____________________________________________________________________________
#Close it down, re-open R, load pavo, Select Wing-Tail-only as the working directory

library(pavo)
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)


mspecs<-aggspec(specs,by=3,FUN=mean)



spec.sm<-procspec(mspecs,opt='smooth',span=0.2)
explorespec(spec.sm,by=4,lwd=3)

vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)
tcs.all<-tcs(vm.spec.sm)


one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/39_American-redstart_HoodedWarbler/","39_American-redstart_HoodedWarbler",one) 

write.csv(tcs.all,file = paste(two,"Individual_values_WING-TAIL ONLY.csv",sep="_"))
