library(tidyr)
library(dplyr)
library(sqldf)
library(gsubfn)
#读取数据表
ahori=read.csv('ah_completed.csv',sep=',')
exchange=read.csv('exchangerate.csv',sep=',')
exchange=exchange[-106,]
exchange=exchange[-106,]
#删除NA
ah=na.omit(ahori)
#拆分日期列
ah$time=as.character(ah$time)
ah$year=substr(ah$time,1,4) #取字符串的前四位
ah$month=sub('....','',ah$time) #删去字符串的前四位
ah$time=as.numeric(ah$time)
ah$month=as.numeric(ah$month)
ah$year=as.numeric(ah$year)
ah=ah[,-1]
str(exchange$time)
exchange=separate(data=exchange, col='time', into=c('year','month'), sep = "-", remove = TRUE)
exchange$month=as.numeric(exchange$month)
exchange$year=as.numeric(exchange$year)
#merge
ahb=merge(ah,exchange,by.x=c("year","month"),by.y=c("year","month"))
ahb$hh=(ahb$h)*(ahb$exchange)
ahb$diff=ahb$a-ahb$hh
ahb$per=(ahb$a)/(ahb$hh)-1
#输出
write.csv(ahb,file='ah_scale.csv')
