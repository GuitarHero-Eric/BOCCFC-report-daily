spzj<-function(ldate)
{a<-loadN()
a <- a[with(a,order(a$审批一审开始时间,decreasing=T)),]
a1<-subset(a,as.Date(a$审批一审开始时间)==as.Date(ldate))
a3<-subset(a,a$进件状态=="结案")
c<-data.frame(table(a3$个人电核电话))
c<-subset(c,c$Freq>=2)
a1<-merge(a1,c,by.x = "个人电核电话",by.y="Var1",all.x=TRUE)
a2<-subset(a1,is.na(a1$Freq)==FALSE)
write.csv(a2,paste("手机号重复",Sys.Date(),".csv"))

spr<-unique(a1$审批助理处理人)
szyc<-NULL
for (j in 1:length(spr)) {
  a2<-subset(a1,审批助理处理人==spr[j])
  a3<-a2[order(a2$审批助理结束时间),]
  dif<-NULL
  for (i in 1:length(a3$审批助理结束时间)) {
    dif1<-difftime(a3$审批助理结束时间[i+1],a3$审批助理结束时间[i],units="mins")
    dif<-rbind(dif,dif1)
  }
  dif<-na.omit(dif)
  dd<-NA
  # 审助耗时
  szhs<-c(dd,dif)
  a4<-cbind(a3,szhs)
  # # 处理时效小于30s
  # a5<-subset(a4,a4$szhs<0.5)
  # 审助异常
  szyc<-rbind(szyc,a4)
}
szyc<-arrange(szyc,szhs)
szyc<-szyc[c(1:(0.03*length(szyc$申请编号))),]
spr<-unique(a1$审批一审审批人)
spyc<-NULL
for (j in 1:length(spr)) {
  a2<-subset(a1,审批一审审批人==spr[j])
  a3<-a2[order(a2$审批一审结束时间),]
  dif<-NULL
  for (i in 1:length(a3$审批一审结束时间)) {
    dif1<-difftime(a3$审批一审结束时间[i+1],a3$审批一审结束时间[i],units="mins")
    dif<-rbind(dif,dif1)
  }
  dif<-na.omit(dif)
  dd<-NA
  # 审批耗时
  sphs<-c(dd,dif)
  a4<-cbind(a3,sphs)
  # # 处理时效小于10s
  # a5<-subset(a4,a4$sphs<0.16666)
  # 审批异常
  spyc<-rbind(spyc,a4)
}
spyc<-arrange(spyc,sphs)
spyc<-spyc[c(1:(0.03*length(spyc$申请编号))),]
szyc<-szyc[,-length(szyc)]
spyc<-spyc[,-length(spyc)]
final<-rbind(szyc,spyc)
test<-unique(final)
write.csv(test,paste("审批质检",Sys.Date(),".csv"))
# 放款质检
a <- a[with(a,order(a$放款一审结束时间,decreasing=T)),]
a1<-subset(a,as.Date(a$放款一审结束时间)==as.Date(ldate))
spr<-unique(a1$放款一审审批人)
fkyc<-NULL
for (j in 1:length(spr)) {
  a2<-subset(a1,放款一审审批人==spr[j])
  a3<-a2[order(a2$放款一审结束时间),]
  dif<-NULL
  for (i in 1:length(a3$放款一审结束时间)) {
    dif1<-difftime(a3$放款一审结束时间[i+1],a3$放款一审结束时间[i],units="mins")
    dif<-rbind(dif,dif1)
  }
  dif<-na.omit(dif)
  dd<-NA
  # 放款审批耗时
  fkhs<-c(dd,dif)
  a4<-cbind(a3,fkhs)
  # # 处理时效小于30s
  # a5<-subset(a4,a4$fkhs<0.5)
  # 放款异常
  fkyc<-rbind(fkyc,a4)
}
fkyc<-arrange(fkyc,fkhs)
fkyc<-fkyc[c(1:(0.055*length(fkyc$申请编号))),]
write.csv(fkyc,paste("放款审批质检",Sys.Date(),".csv"))
}