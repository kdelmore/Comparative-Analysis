###calculating species colouration in tetrahedral space and calculating Eucledian distance

##NOTE- Ammended file for the 5 species with two wing regions. Before running, create two subdirectories
#Normal - which has all regions but wing, and "WINGONLY" which has both wing regions.

rm (list = ls ( ) )

#setwd("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/PineWarber_YellowThroatedWarbler")
#set the working directory to the one you'd currently like
library(pavo)

###bring in all files in the working directory 
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)


###This line averages the reflecta over a certain number. Since here we have 5 measures per region,
###We set 'by = 5'. The output will now trim the column names down so that it's (for example for back) Species_Individual_FMNH_Individual.1
mspecs<-aggspec(specs,by=5,FUN=mean)


#apply a span of 0.2 to smooth out noise
spec.sm<-procspec(mspecs,opt='smooth',span=0.2)
explorespec(spec.sm,by=6,lwd=3)



#calculate photon catches at each photoreceptor, here using the average avian UV visual system 
vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)

#calculate position in tetra-hedral colour space (see Stoddard & Prum 2008 for details)
tcs.all<-tcs(vm.spec.sm)


one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/26_Bobolink_Yellow-headedBlackbird/","26_Bobolink_Yellow-headedBlackbird",one) 
write.csv(tcs.all,file = paste(two,"Individual_Values.csv",sep="_"))

#_____________________________________________________________________________
#Close it down, re-open R, load pavo, Select Wing-only as the working directory

library(pavo)
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)



mspecs<-aggspec(specs,by=3,FUN=mean)



spec.sm<-procspec(mspecs,opt='smooth',span=0.2)
explorespec(spec.sm,by=6,lwd=3)

vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)
tcs.all<-tcs(vm.spec.sm)



one<-getwd()
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/26_Bobolink_Yellow-headedBlackbird/","26_Bobolink_Yellow-headedBlackbird",one) 

write.csv(tcs.all,file = paste(two,"Individual_Values.csv",sep="_"))
