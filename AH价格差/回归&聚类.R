library(corrplot)
library(cluster)
library(factoextra)
ahscale=read.csv('cluster.csv',sep=',')
ahscale=ahscale[,-1]
kappa(cor(scale))
kappa(cor(cc))
corr=round(cor,1)
corrplot(cor,tl.col='black',
         tl.srt=30 )

#线性回归
scale=scale[,-5]
data=as.data.frame(scale)
ahlm=cbind(ahscale$per,data)
names(ahlm)[1]='per'
#ahlm=as.data.frame(scale)
result=lm(formula=per~.,data=)
summary(result)
reduce=step(result,direction = 'back')
summary(reduce)

final=lm(formula=per~PE+PB+turn+roe+Mnshrtrd+Msmvosd+Mnvaltrd,data=lm2)
summary(final)
AIC(final)
coe=as.data.frame(final$coefficients)
coe=scale(coe)


cc=scale1[,-c(4,5,6,8,9)]

#聚类分析
options(scipen = 100)
fviz_nbclust(cc,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)
set.seed(90)
km<-kmeans(cc,iter.max=100,center=3,nstart=20)
fviz_cluster(km, cc, geom = "point",ellipse.type = "convex",
             repel = TRUE,
             labelsize=7,palette = "jco",
             ggtheme = theme_minimal())
cluster1<-round(aggregate(cc,by=list(km$cluster),FUN=mean),2)

#去除outliner
#roe
boxplot(ahlm$Msmvosd,main='roe',col='#bac3d5')
ahlm=ahlm[ahlm$roe>-10 & ahlm$roe<5,] 
boxplot(ahlm$roe,main='roe',col='#d6d8d8')


str(scale)
scale1=as.matrix(lm2)

write.csv(ah,file='ahcomplete.csv')
write.csv(cc,file='cluster.csv')

#
ah=read.csv('ahcomplete.csv',sep=',')
ah=ah[,-1]
ah=ah[,-9]
ah$turn=ah$Mnvaltrd/ah$Msmvosd
ah$turn=ah$turn/1000
ah=ah[,-8]
model=lm(formula=per~.,data=ah[,-c(1,2,3)])
summary(model)
ahlm=ah[,-c(1,2,3)]
cor=cor(ah[,-c(1,2,3)])
ahlm$PE=log(ahlm$PE)
ahlm$PB=log(ahlm$PB)
ahlm$PS=log(ahlm$PS)
ahlm$fix=log(ahlm$fix)
ahlm[,10:13]=log(ahlm[,10:13])
ahlm$turn=ah$turn
ahlm$institution=log(ah$institution)
cor2=cor(ahlm)

scale=scale(ahlm[,-1])
lm2=cbind(ahlm$per,as.data.frame(scale))
names(lm2)[1]='per'

