library(randomForest)

setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/fivegenres")

load("feature_all_5genres.RData")#loads feature_all
gdic<-c('classical','country','hiphop','metal','pop')

feature_all<-data.frame(feature_all)
zv<-nearZeroVar(feature_all)
feature_all<-feature_all[,-zv]
nof<-dim(feature_all)[2]

feat_g<-NULL
for(j2 in c(1:5)){
  m1<-(j2-1)*100+1
  m2<-m1+79
  feat_g<- rbind(feat_g,feature_all[m1:m2,])
}

my_genre<-rep(gdic,1,each=80)
feat_g<-cbind(feat_g,my_genre)
names(feat_g)<-c(as.character(1:nof),"gb")
feat_g<-feat_g[sample(nrow(feat_g)),]

start.time <- Sys.time()
genre_rfm <- randomForest(feat_g[,c(1:nof)],feat_g[,nof+1],ntree=5000)
end.time <- Sys.time();time.taken <- end.time - start.time;time.taken

save(genre_rfm,file="rfmodel_5genres_mywavelets.RData")

rm(list=setdiff(ls(), c("feature_all","genre_rfm","gdic")))


feat_g<-NULL
for (j4 in 1:5){
  feat_g<-rbind(feat_g,feature_all[c((100*j4-19):(100*j4)),])
}
nof<-dim(feat_g)[2]
names(feat_g)<-c(as.character(1:nof))

testitemlength<-20
mac_genre<-predict(genre_rfm,feat_g)
my_genre<-rep(gdic,1,each=testitemlength)
perc<-array(0,5)
for (i1 in 1:5){
  
  i3<-(testitemlength*(i1-1)+1):(testitemlength*i1)
  perc[i1]<-100*sum(my_genre[i3]==mac_genre[i3])/length(my_genre[i3])
  
  
}

perc_total<-100*sum(my_genre==mac_genre)/length(my_genre)
print(perc_total)
print(perc) 
