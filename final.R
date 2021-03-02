setwd('D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/final_paper/data')
# library data set.
art_data=read.csv('data_by_artist.csv')
# fix(art_data)
# delete non-numeric columns.
art_data_drop=art_data[,c(-1,-2,-3,-9,-10)]
# fix(art_data_drop)
names(art_data_drop)
# pre-do (if is NA)
dim(art_data_drop)
sum(is.na(art_data_drop))

# 1.draw cor-plot to look through
# draw correlation figure of variables.
library(corrplot)
# calculate cor-value
cor_pop=cor(art_data_drop)
# plot cor-plot
corrplot(cor_pop,method='color',type='upper',order='hclust',addCoef.col='black')

# 2.all-variable regression
# all var # R2=0.3795
lm.fit=lm(popularity~.,data=art_data_drop)
summary(lm.fit)

attach(art_data_drop)
# from coefficients ,we get function
pop= 5.154101e+01+2.720366e+01*danceability+
  -1.440889e+00*energy+ -2.368519e+01*valence+
  1.751740e-02*tempo+8.471983e-01*loudness+
  -1.430689e+01*acousticness+-4.612783e+00*instrumentalness+
  -8.376493e+00*liveness+-5.897215e+00*speechiness+
  4.863929e-06*duration_ms+-9.429185e-03*count

# delete energy # R2=0.3794 (删除变量，观察)
lm.fit_1=update(lm.fit,~.-energy)
summary(lm.fit_1)
# delete tempo # R2=0.379 (删除变量，观察)
lm.fit_2=update(lm.fit,~.-tempo)
summary(lm.fit_2)

# 计算每个变量的统计量画柱状图() "Estimate""Std..Error""t.value""Pr...t.."
df=data.frame(summary(lm.fit)$coef)
class(df)
names(df)
df[,5]=c('intercept','danceability','energy','valence','tempo','loudness',
         'acousticness','instrumentalness','liveness',
         'speechiness','duration_ms','count')
df
library(ggplot2)
ggplot(df,aes(V5))+
  geom_line(aes(y=Estimate,colour="Estimate",group=1),size=1.3)+
  geom_line(aes(y=Std..Error,colour='Std..Error',group=1),size=1.3,alpha=0.5)+
  geom_line(aes(y=t.value,colour='t.value',group=1),size=1.3,alpha=0.5)+
  geom_line(aes(y=Pr...t..,colour='Pr...t..',group=1),size=1.3,alpha=0.5)+
  scale_colour_manual(name = 'Legend', 
                      values =c("Estimate"='#FF6666','Std..Error'='#0099FF','t.value'='#FFCC00','Pr...t..'='blue'))+
  theme(legend.title = element_text(color="#666666", size=12,family='mono', face="bold"))+
  theme(legend.text = element_text(color="#666666", size = 12, ,family='sans',face = "bold"))+
  theme(axis.text.x = element_text(angle=0, vjust=0.7,size=11,color='#333333',face = 'bold'))+
  labs(x='Variables',y='Value',title='4 Statistics of 11 Variables',face='bold')+
  theme(axis.title.x = element_text(size = 12, family = "bold", color = '#666666', face = "bold", vjust = 0.5, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 12, family = "bold", color = "#666666", face = "bold", vjust = 0.5, hjust = 0.5,angle=0))


# draw bar plot
library(grid)
p1=ggplot(df,aes(V5,Estimate,group=1))+geom_bar(stat='identity',width = 0.3)+xlab(NULL)+
  geom_point(col='red',size=1)
p2=ggplot(df,aes(V5,Std..Error))+geom_bar(stat='identity',width = 0.3)+xlab(NULL)+
  geom_point(col='red',size=1)
p3=ggplot(df,aes(V5,t.value))+geom_bar(stat='identity',width = 0.3)+xlab(NULL)+
  geom_point(col='red',size=1)
p4=ggplot(df,aes(V5,Pr...t..))+geom_bar(stat='identity',width = 0.3)+xlab(NULL)+
  geom_point(col='red',size=1)
p1
p2
p3
p4
pdf('statistics_of_var.pdf',width=10,height=10)
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout=function(x,y)
  viewport(layout.pos.row=x,layout.pos.col=y)
print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(2,1))
print(p3,vp=vplayout(3,1))
print(p4,vp=vplayout(4,1))
dev.off()

###

# 3.for&back-ward stepwise
library(ISLR)
library(leaps)
regfit.fwd=regsubsets(popularity~.,data=art_data_drop,nvmax=11,method='forward')
summary(regfit.fwd)
# or choose backward
# regfit.bwd=regsubsets(popularity~.,data=art_data_drop,nvmax=11,method='backward')
# summary(regfit.bwd)
coef(regfit.fwd,7)
reg.fwd.sum=summary(regfit.fwd)
reg.fwd.sum
which.min(reg.fwd.sum$rss)
which.max(reg.fwd.sum$adjr2)
which.min(reg.fwd.sum$cp)
which.min(reg.fwd.sum$bic)
# draw RSS/Adjusted RSq/Cp/BIC
par(mfrow=c(2,2))
plot(reg.fwd.sum$rss,xlab='Number of Variables',ylab='RSS',type='l')
points(11,reg.fwd.sum$rss[11],col='red',cex=2,pch=20)
plot(reg.fwd.sum$adjr2,xlab='Number of Variables',ylab='Adjusted RSq',type='l')
points(10,reg.fwd.sum$adjr2[10],col='red',cex=2,pch=20)
plot(reg.fwd.sum$cp,xlab='Number of Variables',ylab='Cp',type='l')
points(10,reg.fwd.sum$cp[10],col='red',cex=2,pch=20)
plot(reg.fwd.sum$bic,xlab='Number of Variables',ylab='BIC',type='l')
points(8,reg.fwd.sum$bic[8],col='red',cex=2,pch=20)

# 4.K-fold Cross-validation
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)  
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=10
set.seed(1)
folds=sample(1:k,nrow(art_data_drop),replace=TRUE)
cv.errors=matrix(NA,k,11,dimnames=list(NULL,paste(1:11)))
cv.errors
folds
for (j in 1:k){
  best.fit=regsubsets(popularity~.,data=art_data_drop[folds!=j,],nvmax=11)
  for (i in 1:11){
  pred=predict(best.fit,art_data_drop[folds==j,],id=i)
  cv.errors[j,i]=mean((art_data_drop$popularity[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b',xlab='Number of Variables')
reg.best=regsubsets(popularity~.,data=art_data_drop,nvmax=11)
coef(reg.best,5)









