library(stringi)
library(wavethresh)
library(tuneR)
library(pracma)
rm(list=ls())
setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/")

start.time1 <- Sys.time()
##get the signal
gdic<-c('blues','classical','country','disco','hiphop','jazz','metal','pop','reggae','rock')
dirD<-"C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/data/gztan/" 

time_features<-matrix(0,1000,62)
for (i3 in 1:length(gdic)){
  dirD_g<-paste(dirD,gdic[i3],'/',sep="")
  setwd(dirD_g)
  p<-list.files(pattern=glob2rx("*.wav"))
  setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/")
  for (i5 in 1:length(p)){
    i4<-((i3-1)*100)+i5
    fname<-paste(dirD_g,p[i5],sep="")
    print(paste("analyzing",p[i5],"....."))
    s <- readWave(fname)
    #s <- mono(s, which = c("both"))
    #s <- downsample(s, 22050)
    s<-s@left
    N1<-2^19
    s<-s[1:N1]
    #plot(s,type='l')
    
    #start.time2 <- Sys.time()
    ##find wpt for timefeatures to resolution 9
    #z <- wavDWT(s, wavelet="s8", n.levels=8)
    z<-wp(s)
    z1<-accessD(z,12)#corresponding to level 8 which is the highest for sampling freq of 22050
    z1<-matrix(z1,nrow=256,byrow=TRUE)
    #end.time2 <- Sys.time();time.taken2 <- end.time2 - start.time2;time.taken2

    
    ##abs (filter,normalise?) of subbands
    z1<-abs(z1)
    #z1<-lapply(split(z1,row(z1)),function(ts) {ts*.01/1.99})
    N2<-dim(z1)[2]
    
    ##autocorrelation of subbands
    #z2<-acf(z1[1,],plot=FALSE,lag.max=N2/2)
    z2<-lapply(split(z1,row(z1)),function(ts) acf(ts,plot=FALSE,lag.max=300))
    #z2[[1]][0]
    #par(mfrow=c(2,1))
    #plot(z1[1,], main="1st subband",type='l')
    #plot(z2[[1]], main="autocor",type='l')
    
    ##peak detection in subbands 
    N3<-dim(z1)[1]
    z3<-NULL
    for (i1 in 1:N3){
      z3<-rbind(z3,findpeaks(as.numeric(z2[[i1]]$acf),minpeakheight=0))
    }
    #plot.new()
    #plot(z2[[1]]$acf,type='l')
    #lines(z3[,2],z3[,1],type='p',col='red')
    
    ##beathistogram hist max,mean energy
    bh<-NULL
    for (i2 in 1:300){
      bh[i2]<-sum(z3[z3[,2]==i2,1])*sum(z3[,2]==i2)
    }
    bh.fft<-fft(bh)
    bh.fft[61:240]<-0+0i
    bh<-abs(fft(bh.fft,inverse=TRUE))/length(bh.fft)
   
    bhpeaks<-findpeaks(bh)
    bhpeaks<-bhpeaks[order(bhpeaks[,1],decreasing=TRUE),]
    #plot(bh,type='l'); lines(bhpeaks[1:20,2],bhpeaks[1:20,1],type='p',col='red');
    time_features[i4,1:20]<-bhpeaks[1:20,1] #values of peaks
    time_features[i4,21:40]<-bhpeaks[1:20,2] #position of peaks
    time_features[i4,41:60]<-bhpeaks[1:20,4]-bhpeaks[1:20,3] #width of peaks
    time_features[i4,61]<-dim(bhpeaks)[1] #no. of  oeaks
    time_features[i4,62]<-mean(bh) #hist max,mean energy
  
  }
}
end.time1 <- Sys.time();time.taken1 <- end.time1 - start.time1;time.taken1

save(time_features,file="time_features_gztan_mywavelets.RData")


