setwd('D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/test 1')
dat=read.csv('CreditCard.csv')
library(ggplot2)
###########
####ggpairs画图
library(GGally)
ggpairs(dat, columns=1:12, aes(color=card,alpha=0.05)) + 
  ggtitle("ScatterPoint Matrix——CreditCard(color=card)(all_variable)")+
  theme_get()
###直接pairs画图###（会出现报错情况）
pairs(card~reports+age+income+share+expenditure+owner+selfemp+dependents+months+majorcards+active,panel=panel.smooth,data=dat,main="Scatter Plot",col=dat$card)
######
pairs(dat, main="Scatter Plot",pch=21, bg = c("blue", "yellow")[unclass(dat$card)],panel=panel.smooth)
###### Logistic 回归
#1.全变量回归
glm.fit=glm(as.factor(card)~.,data=dat,family=binomial)
summary(glm.fit)
#2.挑几个变量回归
glm.fit=glm(as.factor(card)~reports+share+active+months+age,data=dat,family=binomial)
summary(glm.fit)
####Logiatic Regression
glm.probs=predict(glm.fit,type='response')
glm.probs[1:10]
glm.pred=rep('no',1319)
glm.pred[glm.probs>0.5]='yes'
table(glm.pred,dat$card)
(294+1000)/1319
mean(glm.pred==dat$card)
####calculate variances
credit=read.csv('CreditCard.csv')
dev.new()
pairs(card~reports+age+income+share+expenditure+owner+selfemp+dependents+months+majorcards+active,panel=panel.smooth,data=credit,main="Scatter Plot",col=credit$card)
a=credit[credit$card=='yes',]
b=credit[credit$card=='no',]
var(a$reports) #即变量x在第一类中的方差
var(b$reports) #即变量x在第二类中的方差
qda.fit=qda(card~reports+share+active+months,data=credit)
qda.fit=qda(card~reports+share+active+months,data=credit)
qda.class=predict(qda.fit,credit)$class
table(qda.class,credit$card)
#### QDA
qda.fit=qda(card~reports+share+active+months,data=dat)
qda.fit
qda.class=predict(qda.fit,dat)$class
table(qda.class,dat$card)
mean(qda.class==dat$card)



