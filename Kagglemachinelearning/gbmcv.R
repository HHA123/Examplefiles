#gbm crossvalidation model for kaggle competion:Springleaf
library(gbm);
library(verification);
gbmcv <- function(data){
  set.seed(666)
  N <- 1000
  data <-data.frame(data)
  k = 5
  n = floor(dim(data)[1]/k)
  cv.err = rep(NA,k)
  set.seed(666)
  
  for(i in 1:k){
    target <- length(names(data))
    #choping up the training set in k subsets with the following indeces
    st = (i-1)*n+1#start of subset
    ed = i*n
    subset = st:ed #index for subset
    
    cvtrain <- data[-subset,]
    cvtest <- data[subset,]
    
    #mod <- randomForest(x = cvtrain[,-1934],y=factor(cvtrain[,1934]))
    mod <- gbm.fit(x=cvtrain[,-target],y=cvtrain[,target],n.trees=N,verbose=F,
                   shrinkage=0.005,interaction.depth=20,n.minobsinnode=5,distribution="bernoulli")
    pred <- predict(mod,newdata=cvtest[,-target],n.trees=N)
    
    cv.err[i] = roc.area(cvtest[,target],pred)$A
    print(paste(paste("AUC for subset ",i),cv.err[i],sep=" "))
    
    save(mod,file=paste("modgbm",i,".rda",sep=""))
    
  }
  print(paste("Average AUC ",mean(cv.err)))
}