library(corrplot)
ah=read.csv('ah_scale.csv',sep=',')
month=read.csv('average_month.csv',sep=',')
year=read.csv('average_year.csv',sep=',')
#
str(ah$code)
ah$code=substr(ah$code,2,7)
ah=ah[order(ah$month),]
ah=ah[order(ah$year),]
#ah$code=as.numeric(ah$code)
year=year[,-1]
month=month[,-1]
#
ahb=merge(ah,month,by.x=c("year","month"),by.y=c("year","month"))
ahb=ahb[,-8]
names(ahb)[8]= 'ttl_month'
ahb=merge(ah,year,by.x=c("year"),by.y=c("year"))

ind=read.csv('ALL.month.csv',sep=',')
ii=merge(ind,month,by.x=c('year','month'),by.y=c("year","month"))
ii=ii[order(ii$ind.code),]
ii=ii[order(ii$month),]
ii=ii[order(ii$year),]
ii$diff=ii$month.avg-ii$average
table(ii$ind.code)
par(mfrow=c(3,4))
plot(ii$diff[ii$ind.code=='B'],type="l")
lines(ii$diff[ii$ind.code=='F'],type="l")
# B  C  D  E  F  G  J  K  L  M  N  R
? plot

write.csv(ah,file='ah_scale.csv')

###
info=read.csv('公司信息表.csv',sep=',')
info$Nnindcd=substr(info$Nnindcd,1,1)
names(info)[1] <- "code"
ah=merge(ah,info,by.x=c('code'),by.y=c('code'))
bmonth=read.csv('average_month.csv',sep=',')
bmonth=bmonth[,-1]
bmonth$B.average=0
for(i in 1:61){
  a=ah$per[ah$Nnindcd == 'B' & ah$year==bmonth[i,]$year & ah$month==bmonth[i,]$month]
  bmonth[i,]$B.average=sum(a)/length(a)
}

##### 加入月转手率
ah=ah[,-4]
trade=read.csv('trade.csv',sep=',')
trade=trade[-c(1,2),]
trade=trade[,-1]
trade=na.omit(trade)
trade$Mnvaltrd=as.numeric(trade$Mnvaltrd)
trade$Msmvosd=as.numeric(trade$Msmvosd)
trade$turnover=(trade$Mnvaltrd)/(trade$Msmvosd)
trade$year=substr(trade$Trdmnt,1,4)
trade$month=substr(trade$Trdmnt,6,7)
trade=trade[,-2]
trade$year=as.numeric(trade$year)
trade$month=as.numeric(trade$month)
write.csv(trade,file='trade.csv')
trade1=cbind(trade[,1],trade[,5:7])
names(trade)[1]='code'
trade$code=as.numeric(trade$code)
ah1=merge(ahna,trade,by.x=c('code','year','month'),by.y=c('code','year','month'))
ahna=ahna[,-1]
write.csv(ahcom,file='ah-complete.csv')

####加入ratio
ahcom=ah1[,-5]
ratio=read.csv('ratio_all.csv',sep=',')
ratio=na.omit(ratio)
ratio$Symbol=substr(ratio$Symbol,2,7)
names(ratio)[2]='code'
ratio$year=substr(ratio$TradingDate,1,4)
ratio$month=sub('....','',ratio$TradingDate)
ratio=ratio[,-1]
ratio$year=as.numeric(ratio$year)
ratio$month=as.numeric(ratio$month)
ahcom=merge(ahcom,ratio,by.x=c('code','year','month'),by.y=c('code','year','month'))

####加入资产比例
asset=read.csv('asset-ratio.csv',sep=',')
asset$year=substr(asset$time,1,4)
asset$month=sub('....','',asset$time)
asset$code=substr(asset$code,2,7)
asset$year=as.numeric(asset$year)
asset$month=as.numeric(asset$month)
asset=asset[,-2]
asset$code=as.numeric(asset$code)
ahcom=merge(ahcom,ahh,by.x=c('code','year','month'),by.y=c('code','year','month'))
ahh=cbind(ahh[,1:3],ahh[10])

ahlm=ahcom[,-c(1,2,3)]
model=glm(formula=per~.,data=ahlm)
summary(model)


###
ratio=read.csv('ratio.csv',sep=',')
ratio2=read.csv('ratio 2019.csv',sep=',')
names(ratio2)[2]='code'
ratio3=read.csv('ratio 2016-2018.csv',sep=',')
str(ratio2$TradingDate)
ratio=rbind(ratio1,ratio3)
#
ratio$year=substr(ratio$TradingDate,1,4)
ratio$month=substr(ratio$TradingDate,6,7)
ratio=ratio[,-1]
ratio2$year=substr(ratio2$TradingDate,1,4)
ratio2$month=gsub("\\/..*","",ratio2$month)
ratio=rbind(ratio,ratio2)
ratio$year=as.numeric(ratio$year)
ratio$month=as.numeric(ratio$month)

write.csv(ah,file='ahcomplete.csv')

ah1$code=as.numeric(ah1$code)
ratio=na.omit(ratio)
#
ah2=merge(ah,ratio,by.x=c('code','year','month'),by.y=c('code','year','month'))
ahlm=ahcom[,-c(1,2,3)]
model=lm(formula=per~.,data=ahlm)
summary(model)

ah=ah[-1]
ratio=ratio[-1] 
ahlm=na.omit(ahlm)
cor(ahlm)


####
ah=read.csv('ahcomplete.csv',sep=',')
ah=ah[-1]
ah=na.omit(ah)
cor(ahlm)

roe=read.csv('ROE.csv',sep=',')
roe=roe[,-22]
names(roe)[1]='code'
roe$code=substr(roe$code,1,6)
roe$code=as.numeric(roe$code)
write.csv(roe,file='ROE.csv')
ahr=ah[1,c(1,2,3)]
ahr=ahcom[ah$code==2,c(1,2,3)]
ahr$institution=0
ah39=ah[ah$code==roe$code[j],]

ah=ah%>%distinct(code,year,month,.keep_all=T)


roe=read.csv('机构持股比例.csv',sep=',')

ahcom=ahr

for(j in 80:111){
  ahr=ah[ah$code==roe$code[j],c(1,2,3)]
  ahr$institution=0
for(i in 1:length(ahr[,1])){
  if(ahr$year[i]==2014){
    if(ahr$month[i]<=12 & ahr$month[i]>=9){ahr$institution[i]=roe$X2014.4[j]}
  }
  if(ahr$year[i]==2015){
    if(ahr$month[i]<=3){ahr$institution[i]=roe$X2015.1[j]}
    if(ahr$month[i]<=6 & ahr$month[i]>=4){ahr$institution[i]=roe$X2015.2[j]}
    if(ahr$month[i]<=9 & ahr$month[i]>=7){ahr$institution[i]=roe$X2015.3[j]}
    if(ahr$month[i]<=12 & ahr$month[i]>=10){ahr$institution[i]=roe$X2015.4[j]}
  }
  if(ahr$year[i]==2016){
    if(ahr$month[i]<=3){ahr$institution[i]=roe$X2016.1[j]}
    if(ahr$month[i]<=6 & ahr$month[i]>=4){ahr$institution[i]=roe$X2016.2[j]}
    if(ahr$month[i]<=9 & ahr$month[i]>=7){ahr$institution[i]=roe$X2016.3[j]}
    if(ahr$month[i]<=12 & ahr$month[i]>=10){ahr$institution[i]=roe$X2016.4[j]}
  }
  if(ahr$year[i]==2017){
    if(ahr$month[i]<=3){ahr$institution[i]=roe$X2017.1[j]}
    if(ahr$month[i]<=6 & ahr$month[i]>=4){ahr$institution[i]=roe$X2017.2[j]}
    if(ahr$month[i]<=9 & ahr$month[i]>=7){ahr$institution[i]=roe$X2017.3[j]}
    if(ahr$month[i]<=12 & ahr$month[i]>=10){ahr$institution[i]=roe$X2017.4[j]}
  }
  if(ahr$year[i]==2018){
    if(ahr$month[i]<=3){ahr$institution[i]=roe$X2018.1[j]}
    if(ahr$month[i]<=6 & ahr$month[i]>=4){ahr$institution[i]=roe$X2018.2[j]}
    if(ahr$month[i]<=9 & ahr$month[i]>=7){ahr$institution[i]=roe$X2018.3[j]}
    if(ahr$month[i]<=12 & ahr$month[i]>=10){ahr$institution[i]=roe$X2018.4[j]}
  }
  if(ahr$year[i]==2019){
    if(ahr$month[i]<=3){ahr$institution[i]=roe$X2019.1[j]}
    if(ahr$month[i]<=9 & ahr$month[i]>=4){ahr$institution[i]=roe$X2019.2[j]}
  }
}
  ahcom=rbind(ahcom,ahr)
}

ahlm=ahlm[,-1]
ah1=merge(ah,ahcom,by.x=c('code','year','month'),by.y=c('code','year','month'))
ahlm=ah1[,-c(1,2,3)]
ah1=ah1[,-4]
model=lm(formula=per~.,data=ahscale)
summary(model)
AIC(model)
write.csv(ah1,file='ahcomplete.csv')
result=step(model,direction = 'back')
summary(result)


ratio1=cbind(ratio[,1],ratio[,7],ratio[,10:11])
names(ratio1)[1]='code'
names(ratio1)[2]='ChangeRatio'
ah2=merge(ah1,ratio1,by.x=c('code','year','month'),by.y=c('code','year','month'))

ah1=read.csv('ahcomplete.csv',sep=',')
ah2=ah2[,-1]
ahna=na.omit(ah2)
#把ahlm中的变量，除了有负的roe&changeratio in，全部取log
ahscale=ahlm
ahscale$PE=log(ahscale$PE)
ahscale$PB=log(ahscale$PB)
ahscale$institution=log(ahscale$institution)
ahscale[,12:14]=log(ahscale[,12:14])
ahscale$in.=ahlm$in.
cor=cor(ahscale)
cor1=cor(ahlm)

write.csv(ahscale,file='LOG.csv')
a=result$coefficients
str(cor)
a=as.data.frame(a)

