######Get_Return#####
#define a variable named dat_AC only contains AC
setwd("D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/work_1_ddl1221/homework 1")
dat=read.csv('Stock_Data.csv')
Indicator=seq(3,dim(dat)[2],2)
dat_AC=(dat[,Indicator])
write.csv(dat_AC,"H2_dat_AC.csv",row.names = FALSE)


dat$Date=as.Date(dat$Date,format='%d/%m/%Y')
result=data.frame(index=(1:4962))
name=names(dat_AC)
for (o in name){
  ###
  start1= which(dat$Date=='1987-01-02')
  end1= which(dat$Date=='2006-08-31')
  prePrice=(dat[o][,1])[start1:end1]
  ###
  start2= which(dat$Date=='1987-01-05')
  end2= which(dat$Date=='2006-09-01')
  curPrice=(dat[o][,1])[start2:end2]
  ###
  Return=NULL
  for(i in 1:4962){
    Return=c(Return,curPrice[i]/prePrice[i]-1)
  }
  AC_rtn=paste(o,'_Return',sep='')
  result[AC_rtn]<-(Return)
}
result=result[,-1]

# Analyze the returns of S&P on the predictors.
summary(result)
lm.fit=lm(S.P_AC_Return~.,data=result)
sink('H2_returnsRegression.csv')
summary(lm.fit)
sink(NULL)
