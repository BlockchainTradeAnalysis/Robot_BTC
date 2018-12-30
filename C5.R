
data = read.csv("",header = T,sep=",")
closeprice = data$closeprice
lrow = nrow(data)

#compute class for 2 levels about long condition
#class must be numeric by stepwise regression
#Target R > 10% and MDD < -5%  Y=1 , else Y=0
class=rep(0,lrow)

for(i in 1:(lrow-1))
{
  data.fun=rep(closeprice[i],lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun

  com10=c()
  com5=c()
  #多單
  com5=which(data.ret<(-0.05))#MDD 空單 >0.05
  com10=which(data.ret>=0.1)#Target R 空單 <=-0.1
  
  if((length(com5)!=0)&length(com10)!=0)
  {
    if(com10[1]<com5[1])
    {
      class[i]=1 
    }
  }
  
  if((length(com5)==0)&length(com10)!=0)
  {
    class[i]=1
  }  
}
#we can not confirm some data$class recently,so delete 250days
class=class[1:(lrow-250)]#delete 250days recently
data=data[1:(lrow-250),3:ncol(data)]
data.total=data.frame(class,data)

library(MASS)

fit1=lm(class~.,data=data.total)
stepresult=stepAIC(fit1,direction="both")

needdata.frame(class,data[,1],data[,3])
names(needdata.tsmc)=c("class","v1","v3")
needdata=needdata.tsmc

#prepare testing data
data1=read.csv("",header = T,sep=",")
closeprice = data1$closeprice
lrow = nrow(data1)

class=rep(0,lrow)

for(i in 1:(lrow-1))
{
  data.fun=rep(closeprice[i],lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun
  
  com10=c()
  com5=c()
  #多單
  com5=which(data.ret<(-0.05))#MDD 空單 >0.05
  com10=which(data.ret>=0.1)#Target R 空單 <=-0.1
  
  if((length(com5)!=0)&length(com10)!=0)
  {
    if(com10[1]<com5[1])
    {
      class[i]=1 
    }
  }
  
  if((length(com5)==0)&length(com10)!=0)
  {
    class[i]=1
  }  
}
#giving real class 
realmatrix=c()
#result in #3737-120=#3617
for(i in 3476:3617)
{
  realmatrix[i-3475]=as.matrix(class[i])
}

a=3476
b=3617
sum(class[a:b]==1)
sum(class[a:b]==0)

library(e1071)

data.resultsvm=c()
prematrixsvm=c()

for(i in 3476:3617)
{
  tsvm=svm(class~.,data=needdata[1:3225,],type="C-classification",cost=2,kernal="radial basis",gamma=0.1,scale=TRUE)
  
  pretsvm=predict(tsvm,testdata[i,-1])
  
  data.resultsvm[i-3475]=sum(pretsvm==testdata$class[i])/length(pretsvm)
  prematrixsvm[i-3475]=as.character.factor(pretsvm)
}  

summary(data.resultsvm)
t=table(prematrixsvm,realmatrix)
sum(diag(t))/sum(t)

#decision tree(c50)
library(C50)

data.resultc50=c()
prematrixc50=c()

for(i in 3476:3617)
{
  trainc50=C5.0(class~.,needdata[1:3225,],trials=5,control=C5.0Control(subset=FALSE,noGlobalPruning = TRUE,CF=0.25))
  
  pretc50=predict(trainc50,testdata[i,-1],trials=5,type="class")
  
  data.resultc50[i-3475]=sum(pretc50==testdata$class[i])/length(pretc50)
  prematrixc50[i-3475]=as.character.factor(pretc50)
}  

summary(data.resultc50)
t=table(prematrixc50,realmatrix)
sum(diag(t))/sum(t)  
  




