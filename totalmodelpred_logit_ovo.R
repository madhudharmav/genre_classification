library(glmnet)
library(caret)
setwd("C:/Users/MadhusudhanaRao/Documents/DSR/dsrportfolio3/programs/mywavelet_based/fivegenres")

load("feature_all_5genres.RData")#loads feature_all
gdic<-c('classical','country','hiphop','metal','pop')

zv<-nearZeroVar(feature_all)
feat_g<-feature_all[,-zv]
nof<-dim(feat_g)[2]
feat_g<-cbind(feat_g,array(0,500,1))

genre_logit <- array(list(NULL), c(5,5))


start.time <- Sys.time()
for(j2 in c(1:5)){
  m1<-(j2-1)*100+1
  m2<-m1+79
  feat_g[m1:m2,nof+1]<- 0
  
  j3<-setdiff(1:5,j2)
  for(j1 in j3) {
    
    m3<-(j1-1)*100+1
    m4<-m3+79
    feat_g[m3:m4,nof+1]<- 1
    
    feat_2class<- rbind(feat_g[m1:m2,],feat_g[m3:m4,])
    feat_2class<-feat_2class[sample(nrow(feat_2class),nrow(feat_2class)),]
    genre_logit[[j2,j1]] <- cv.glmnet(feat_2class[,c(1:nof)],feat_2class[,nof+1],family="binomial",nfold=30)
    
    
  }
}
end.time <- Sys.time();time.taken <- end.time - start.time;time.taken
save(genre_logit,file="logitmodel_ovo_5genres_mywavelets.RData")


rm(list=setdiff(ls(), c("feature_all","genre_logit","gdic","zv")))
feat_g<-NULL
for (j4 in 1:5){
  feat_g<-rbind(feat_g,feature_all[c((100*j4-19):(100*j4)),])
}
feat_g<-feat_g[,-zv]
nof<-dim(feat_g)[2]

predlogitfit<-array(0,c(100,5,5))
start.time <- Sys.time()
for(j2 in c(1:5)){
  j3<-setdiff(1:5,j2)
  for(j1 in j3) {
    
    predlogitfit[,j2,j1]<-predict(genre_logit[[j2,j1]],newx=feat_g,type="response",s = "lambda.min")
    
  }
}

mg_1<-apply(predlogitfit,c(1,2),sum) 
mg_2<-apply(mg_1,1,which.min)
mac_genre<-gdic[mg_2]

testitemlength<-20
my_genre<-rep(gdic,1,each=testitemlength)

perc<-array(0,5)
for (i1 in 1:5){
  
  i3<-(testitemlength*(i1-1)+1):(testitemlength*i1)
  perc[i1]<-100*sum(my_genre[i3]==mac_genre[i3])/length(my_genre[i3])
  
  
}

perc_total<-100*sum(my_genre==mac_genre)/length(my_genre)
print(perc_total)
print(perc) 
