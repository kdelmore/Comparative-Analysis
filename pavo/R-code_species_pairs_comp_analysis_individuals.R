###calculating species colouration in tetrahedral space and calculating Eucledian distance

rm (list = ls ( ) )

#setwd("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/PineWarber_YellowThroatedWarbler")
#set the working directory to the one you'd currently like
library(pavo)

###bring in all files in the working directory 
specs<-getspec(where = getwd(), ext = "txt", lim = c(300,700), subdir = TRUE, subdir.names = TRUE)


###This line averages the reflecta over a certain number. Since here we have 5 measures per region,
###We set 'by = 5'. The output will now trim the column names down so that it's (for example for back) Species_Individual_FMNH_Individual.1
mspecs<-aggspec(specs,by=5,FUN=mean)
#for reference:  _(blank) = crown (specs 1-5), .1 = back (specs 6-10), .2 = rump (secs 11-15)
# .3 = tail (specs 16-20), .4 = throat (specs 21-25), .5 = chest (specs 26-30), .6 = wing (specs 31-35)



#explore the raw data to see if a smoothing term should be applied
#plotsmooth(mspecs,minsmooth=0.05,maxsmooth=0.5,curves=4,ask=F)
#apply a span of 0.2 to smooth out noise
spec.sm<-procspec(mspecs,opt='smooth',span=0.2)
explorespec(spec.sm,by=7,lwd=3)


#calculate photon catches at each photoreceptor, here using the average avian UV visual system 
vm.spec.sm <- vismodel(spec.sm, visual="avg.uv",relative=TRUE)

#calculate position in tetra-hedral colour space (see Stoddard & Prum 2008 for details)
tcs.all<-tcs(vm.spec.sm)


one<-getwd()
#Replace what is in quotations below with whatever the value is for "one" -character string
two<-sub("C:/Users/Ryan/Desktop/PhD/Side Projects/Kira - comparative analysis/Real Data Files/Species_Pairs/","",one) 
write.csv(tcs.all,file = paste(two,"Individual_values.csv",sep="_"))


