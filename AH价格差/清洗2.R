#input data
ah=read.csv('ah_scale.csv',sep=',')
month=read.csv('average_month.csv',sep=',')
year=read.csv('average_year.csv',sep=',')
#order by year & month
ah=ah[order(ah$month),]
ah=ah[order(ah$year),]
month[61,1]=2019
#compute month average
month$average=0
for(i in 1:61){
  a=ah$per[ah$year==month[i,]$year & ah$month==month[i,]$month]
  month[i,]$average=sum(a)/length(a)
}
year$average=0
for(i in 1:6){
  a=ah$per[ah$year==year[i,]$year]
  year[i,]$average=sum(a)/length(a)
}
#output
write.csv(month,file='average_month.csv')
write.csv(year,file='average_year.csv')

ah=read.csv('ahcomplete.csv',sep=',')
ah=ah[,-1]
ahclu=ah[,-c(7,8,9,11,12,14)]
com=read.csv('公司信息表.csv',sep=',')
com$Msmvosd=0
for(i in 1:114){
  a=ahclu$Msmvosd[ahclu$code == com[i,]$Stkcd]
  com[i,]$Msmvosd=sum(a)/length(a)
}

#聚类分析
write.csv(com,file='company.csv')
com=na.omit(com)
scale=scale(com[,-c(1,2,3,4)])
scale=cbind(com$per,scale)
names(scale)[1]='per'
scale=scale[,-8]
options(scipen = 100)
fviz_nbclust(scale,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)
set.seed(90)
scale=cbind(scale,scale(com$age))
scale=scale[,-8]
km<-kmeans(scale,iter.max=100,center=3,nstart=20)
km$size
?fviz_cluster
fviz_cluster(km,scale, geom = "point",ellipse.type = "convex",
             repel = TRUE,
             labelsize=7,palette = c("#BBB5AC", "#AF231C", "#FBDF72"),
             ggtheme = theme_minimal())
cluster1<-round(aggregate(scale,by=list(km$cluster),FUN=mean),2)
clas=as.data.frame(km$cluster)
clas=cbind(com[,1:3],clas)

write.csv(clas,file='com_cluster2.csv')

write.csv(scale,file='scale.csv')
