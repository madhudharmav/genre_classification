library(wavethresh)
library(wmtsa)
library(tuneR)
library(seewave)
library(pracma)
rm(list=ls())
setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/")

start.time1 <- Sys.time()
##get the signal
gdic<-c('blues','classical','country','disco','hiphop','jazz','metal','pop','reggae','rock')
dirD<-"C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/data/gztan/" 

freq_features<-matrix(0,1000,140)
for (i3 in 1:length(gdic)){
#for (i3 in 1:1){
  dirD_g<-paste(dirD,gdic[i3],'/',sep="")
  setwd(dirD_g)
  p<-list.files(pattern=glob2rx("*.wav"))
  setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/")
  for (i5 in 1:length(p)){
  #for (i5 in 1:1){
    i4<-((i3-1)*100)+i5
    fname<-paste(dirD_g,p[i5],sep="")
    print(paste("analyzing frequency features of ",p[i5],"....."))
    s <- readWave(fname)
    #s <- mono(s, which = c("both"))
    s <- downsample(s, 16348) #downsample to 16348 so crystals are divided into music octaves
    s<-s@left
    N1<-length(s)
    #plot(s,type='l')
    
    
    
    z<-wavDWT(s,n.levels=9)
    z<-z$data
    i1<-0
    for (i2 in 1:10){
      f<-16348/(2^i2)
      if(i2==10){f<-16348/2^9}
      freq_features[i4,i1+1]<-zcr(z[[i2]],f=f,wl=NULL,plot=FALSE) #zero crossing
      
      spec_z<-spec(z[[i2]],f=f,plot=FALSE)#spec or mean spec
      #par(mfrow=c(2,1))
      #plot(spec(z[[i2]],f=f,plot=FALSE), main="just spec",type='l')
      #plot(meanspec(z[[i2]],f=f,plot=FALSE), main="meanspec",type='l')


      specpeaks<-findpeaks(as.numeric(spec_z))
      specpeaks<-specpeaks[order(specpeaks[,1],decreasing=TRUE),]
      freq_features[i4,(i1+c(2:4))]<-specpeaks[1:3,1] #values of peaks
      freq_features[i4,(i1+c(5:7))]<-specpeaks[1:3,2] #position of peaks
      
      spec_z<-meanspec(z[[i2]],f=f,plot=FALSE)
      spec_f<-unlist(specprop(spec_z,f=f,plot=FALSE))      
      freq_features[i4,(i1+c(8:14))]<-spec_f[c(1,2,4,5,9,11,12)]      
      i1<-i1+14
    }
  }
}  
end.time1 <- Sys.time();time.taken1 <- end.time1 - start.time1;time.taken1

save(freq_features,file="freq_features_spec2peaks_3peaks_gztan_mywavelets.RData")    
    



