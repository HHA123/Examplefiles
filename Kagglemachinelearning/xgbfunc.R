#xgboost crossvalidation model for kaggle competion:Springleaf
library(xgboost);
library(verification);
xgbfunc <- function(data2){
  set.seed(666)
  data2 <-data.frame(data2)
  k = 4
  #n = floor(dim(data)[1]/k)
  #cv.err = rep(NA,k)
  
  
    target <- length(names(data2))
    #choping up the training set in k subsets with the following indeces
    #st = (i-1)*n+1#start of subset
    #ed = i*n
    #subset = st:ed #index for subset
    
    subset <- sample(1:dim(data2)[1],0.3*dim(data2)[1])
    
    cvtest <- data2[subset,]
    y<- data2[subset,target]
    cvtest <- as.matrix(cvtest)
    mode(cvtest) <- "numeric"
    cvtest <- xgb.DMatrix(cvtest[,-target],label=cvtest[,target])
    
    data2 <- data2[-subset,]
    cvtrain <- as.matrix(data2)
    rm(data2)
    mode(cvtrain) <- "numeric"
    cvtrain <- xgb.DMatrix(cvtrain[,-target],label=cvtrain[,target])
    
    
    
    nround.cv = 150
    #best.cv <- xgb.cv(param=param,data=cvtrain,nfold=k,nrounds=nround.cv,prediction=T)
    #max.auc =which.max(best.cv$dt[,test.auc.mean])
    #max.auc = nround.cv
   for(i in 1:k){
     param <- list(objective='binary:logistic',max.depth=7,eta=0.01,eval_metric="auc",
                   subsample=1)
     max.auc = 650 -50*i
    
    best <- xgboost(param=param,data=cvtrain,nrounds=max.auc,verbose=0)
    
    pred <- predict(best,cvtest)
    #cv.err[i] = roc.area(cvtest[,target],pred)$A
    print(paste("model",i,"auc",roc.area(y,pred)$A,sep=""))
    #print(max.auc)
   }
    #return(best)
    #print(paste(paste("AUC for subset ",i),cv.err[i],sep=" "))
    
   # save(mod,file=paste("modgbm",i,".rda",sep=""))
    
  #}
  #print(paste("Average AUC ",mean(cv.err)))
}