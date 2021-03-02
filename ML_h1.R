#1.import the path of Stock_Data.csv to read the excel.
setwd("D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/work_1_ddl1221/homework 1")
#2.create a variable named dat,output the statistics data of each column. 
dat=read.csv('Stock_Data.csv')
summary(dat)
df=data.frame(summary(dat))
write.table(df,file="D:/dfsummary_h1.csv",append=T,row.names=F)
#3.format the Date column as '%d/%m/%Y' to plot the figure.
dat$Date=as.Date(dat$Date,format='%d/%m/%Y')
plot(dat$Date,dat$F_AC, xlab='date',ylab='AC_price',col='pink')
# use ggplot 2
library(ggplot2)
ggplot(dat,aes(x=Date, y=F_AC)) + 
  geom_point(alpha=0.1,size=1.0,shape=25,fill='white',stroke=2,color='red')+
  theme(axis.title.x =element_text(size=13), axis.title.y=element_text(size=13))
#4.aimed to calculate the pre & cur price, now create 2 variables,start and end.
start1= which(dat$Date=='1987-01-02')
end1= which(dat$Date=='2006-08-31')
prePriceF=dat$F_AC[start1:end1]
start2= which(dat$Date=='1987-01-05')
end2= which(dat$Date=='2006-09-01')
curPriceF=dat$F_AC[start2:end2]
#5.calculate the return results
Return=NULL
for(i in 1:4962){
  Return=c(Return,curPriceF[i]/prePriceF[i]-1)
}
hist(Return,main='Return')
# use ggplot2
library(ggplot2)
r = data.frame(Return)
ggplot(r,aes(x=Return))+
  geom_histogram(fill='red',color='black',alpha=0.3)+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18))