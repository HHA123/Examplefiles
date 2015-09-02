#randomForest crossvalidation model for kaggle competion:Springleaf
library(randomForest);
library(verification);
ranfor <- function(data){
data <-data.frame(data)
sista <- dim(data)[2]
k = 5
n = floor(dim(data)[1]/k)
cv.err = rep(NA,k)
set.seed(666)
N <-500
for(i in 1:k){
#choping up the training set in k subsets with the following indeces
st = (i-1)*n+1#start of subset
ed = i*n
subset = st:ed #index for subset

cvtrain <- data[-subset,]
cvtest <- data[subset,]

#mod <- randomForest(x = cvtrain[,-1934],y=factor(cvtrain[,1934]))
mod <- randomForest(as.factor(cvtrain[["target"]])~.,data=cvtrain[-sista],type="classification",ntree=N)
pred <- predict(mod,newdata=cvtest[,-sista],type="prob")[,2]

cv.err[i] = roc.area(cvtest[,sista],pred)$A
print(paste(paste("AUC for subset ",i),cv.err[i],sep=" "))

save(mod,file=paste("modranfor",i,".rda",sep=""))

}
print(paste("Average AUC ",mean(cv.err)))
}
