library(ISLR)
fix(NCI60)
nci.labs=NCI60$labs
nci.data=NCI60$data
nci.labs
rownames(nci.data)

inf_data=read.csv('influence_data.csv')
#### PCA+k-means聚类
setwd('D:/Desktop/AAA上海大学文件/BBB课程学习资料/B-2020-2021/CLASS冬/ML_in_Business/final_paper/data')
# library data set.
art_data=read.csv('genre5.csv')
art_data
# fix(art_data)
# delete non-numeric columns.
art_genre=art_data[,c(-1,-2,-9,-10)]
# fix(art_data_drop)
names(art_genre)
art_genre
sum(is.na(art_genre$artist_main_genre))
table(art_genre$artist_main_genre)
genere=art_genre$artist_main_genre
genres=unique(art_genre$artist_main_genre)#去重
genres#部分genre
USArrests
#PCA 
apply(art_genre[,-1],2,mean)
pr.out=prcomp(art_genre[,-1],scale=TRUE)#scale 标准化处理
names(pr.out)
pr.out$center #均值
pr.out$scale #标准差
pr.out$rotation #主成分荷载向量
pr.out$x #所有变量的主成分值
pr.out
pca6=pr.out$x[,1:6]
pca6
dim(pr.out$x)
par(mfrow=c(1,1))
biplot(pr.out,scale=0,
       xlab='First Pricipal Component',
       ylab='Second Pricipal Component',
       main='Scaled',family='mono')#双标图，如果太多就画不出来
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
pve
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component ", 
     ylab="Proportion of Variance Explained ",ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", 
     ylab="Cumulative Proportion of Variance Explained ",ylim=c(0,1),type='b')

#k-means
set.seed(123)
km.out=kmeans(pca6,5,nstart =20)
km.out$cluster
par(mfrow=c(1,1))
# 画聚类图（5个比较明显）
plot(pca6, col=(km.out$cluster),
     main="K-Means Clustering Results with K=5", 
     xlab="", ylab="", pch=20, cex=0.9)






