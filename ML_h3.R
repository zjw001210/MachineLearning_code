library(ISLR)
library(MASS)
library(boot)
library(ggplot2)
library(caret)
#获取工作路径
setwd('D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/work_ddl0104_18121216+19121255')
data0=read.csv('CreditCard.csv')
card=ifelse(data0$card == "yes", 1, 0)
data=data0[,-1]
data=data.frame(card,data)
##################################1。验证集方法################################
dim(data)# 1319 13
set.seed(1)
#将数据分一半为训练集
train=sample(1319,660)
MR_1=rep(0,6)
for(i in (1:6)){ 
  lm.fit=lm(card~poly(reports,log(income),dependents,active,degree=i),data=data,subset=train)
  lm.probs=predict(lm.fit,data,type = 'response')
  lm.pred=ifelse(lm.probs>0.5,1,0)
  table(lm.pred,card)
  #得到MR_1,即验证集方法的错误分类率
  MR_1[i]=1-mean(lm.pred==data$card)
}
#画图
MRdf_1=data.frame(MR_1)
ggplot(MRdf_1,aes(x=(1:6),y=MRdf_1[,1]))+geom_point(alpha=0.3,color='red',size=4,shape=18)+geom_line(size=1,col='pink')+labs(x='Degree_of_Polynomial',y='Misclassification_Rate(method=ValidationSet)')




###################################2.LOOCV方法########################
# 将原数据分为1319部分,太大了，改为100
folds = createFolds(y=data$card,k=100)
results_error = c()
MR_2=rep(0,6)
# loop
for(i in 1:100){
  for(j in 1:6){fold_test = data[folds[[i]],]#选一个作为测试集
  fold_train = data[-folds[[i]],]#剩下的为训练集
  
  fold_fit = glm(card~poly(dependents,reports,log(income),active,degree=j),data = data,family = "binomial")
  fold_predict = predict(fold_fit,type = 'response',newdata=fold_test)
  fold_predict = ifelse(fold_predict >= 0.5, 1, 0)
  fold_test$predict = fold_predict
  
  fold_error =  sum(fold_test[,1] != fold_test[,ncol(fold_test)]) / nrow(fold_test)
  results_error[i] = fold_error
  MR_2[j]=mean(results_error)
  
  #print(i)
  #print(fold_error)

  }
}
MR_2
MRdf_2=data.frame(MR_2[1:6])
ggplot(MRdf_2,aes(x=(1:6),y=MRdf_2[,1]))+geom_point(alpha=0.3,color='purple',size=4,shape=18)+geom_line(size=1,col='purple')+labs(x='Degree_of_Polynomial',y='Misclassification_Rate(method=LOOCV)')


############################3.K-fold-CV方法####################################
folds = createFolds(y=data$card,k=10) 
min = 1
num = 0
results_error = c()
MR_3=rep(0,6)
# loop
for(i in 1:10){
  for(j in 1:6){fold_test = data[folds[[i]],]#选一个作为测试集
  fold_train = data[-folds[[i]],]#剩下的为训练集
  
  fold_fit = glm(card~poly(dependents,reports,log(income),active,degree=j),data =fold_train,family = "binomial")
  fold_predict = predict(fold_fit,type = 'response',newdata=fold_test)
  fold_predict = ifelse(fold_predict >= 0.5, 1, 0)
  fold_test$predict = fold_predict
  
  fold_error =  sum(fold_test[,1] != fold_test[,ncol(fold_test)]) / nrow(fold_test)
  results_error[i] = fold_error
  MR_3[j]=mean(results_error)
  }
}
MR_3
## [1] 0.1797536 0.1782385 0.1774809 0.1880870 0.1805112 0.1873294
MRdf_3=data.frame(MR_3)
ggplot(MRdf_3,aes(x=(1:6),y=MRdf_3[,1]))+geom_point(alpha=0.3,color='blue',size=4,shape=18)+geom_line(size=1,col='blue')+labs(x='Degree_of_Polynomial',y='Misclassification_Rate(method=K-fold CV)')
















