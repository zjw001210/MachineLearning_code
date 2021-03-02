library(AER)
library(boot)
library(ggplot2)
##
data('CPS1988')
set.seed(1)
s=sample(28155,1000)
dat=CPS1988[s,]
#####2
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(dat$wage~poly(dat$education,i),data=dat,)
  cv.error[i]=cv.glm(dat,glm.fit)$delta[1]
}
cv.err=data.frame(cv.error)
ggplot(cv.err,aes(x=(1:5),y=cv.err[,1]))+geom_point(alpha=0.3,color='red',size=4,shape=18)+geom_line(size=1,col='pink')+labs(x='Degree_of_Polynomial',y='Mean_Squared_Error(method=LOOCV)')

#####3
bootC = function(data,i) {
  cor(dat$education[i],dat$wage[i],
      use = "complete.obs", method = "spearman")
}
set.seed(2)
bootResult=boot(dat,bootC,2000)
plot(bootResult)
bootResult
