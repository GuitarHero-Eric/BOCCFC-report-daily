#通用参数区
dhspc<-function(ldate,dhmode=c(1:2))
{a<-loadN()
b<-read.csv(paste("lab/dh/",as.character(Sys.Date()),".csv",sep=""))
开始日期<-as.Date(b$开始时间)
b<-cbind(b,开始日期)
mdate<-ldate


#max(b$开始日期)
c<-subset(b,开始日期==mdate)

a$电核开始时间[a$电核开始时间==""]<-"1900-1-3 0:00"
a$电核开始时间<-as.Date(a$电核开始时间)
a<-subset(a,电核开始时间>as.Date("2010-01-01"))
a2<-a[c("申请编号","进件日期","产品名称","区域中心","受理城市","个人电核电话",
        "单位电核电话区号","单位电核电话号码","单位电核电话分机号")]

write.csv(a2,"temp/qq.csv",row.names = F)
a<-read.csv("temp/qq.csv")

##--------个电部分--------
a$个人电核电话<-gsub("=\"","",a$个人电核电话)
a$个人电核电话<-gsub("\"","",a$个人电核电话)
a$单位电核电话区号<-gsub("=\"","",a$单位电核电话区号)
a$单位电核电话区号<-gsub("\"","",a$单位电核电话区号)
a$单位电核电话号码<-gsub("=\"","",a$单位电核电话号码)
a$单位电核电话号码<-gsub("\"","",a$单位电核电话号码)
a$单位电核电话分机号<-gsub("=\"","",a$单位电核电话分机号)
a$单位电核电话分机号<-gsub("\"","",a$单位电核电话分机号)
a$单位电核电话区号<-as.character(as.numeric(a$单位电核电话区号))
a$单位电核电话区号[a$单位电核电话区号=='0512']<-""
a$单位电核电话区号[a$单位电核电话区号=='512']<-""
a$单位电核电话区号<-gsub(" ","",a$单位电核电话区号)
a$单位电核电话号码<-gsub(" ","",a$单位电核电话号码)
a$单位电核电话分机号<-gsub(" ","",a$单位电核电话分机号)
单位电核电话1<-paste(a$单位电核电话区号,a$单位电核电话号码,sep="")
单位电核电话2<-paste(a$单位电核电话区号,a$单位电核电话号码,a$单位电核电话分机号,sep="")
a<-a[,-(ncol(a)-2):-ncol(a)]
a<-cbind(a,单位电核电话1,单位电核电话2)
abase<-a

##个电部分##
ax<-ddply(a,.(个人电核电话),summarize,最后进件日=max(as.Date(进件日期)))
判断1<-paste(ax$个人电核电话,"-",as.numeric(as.Date(ax$最后进件日)),sep="")
ax<-cbind(ax,判断1)
判断1<-paste(a$个人电核电话,"-",as.numeric(as.Date(a$进件日期)),sep="")
a<-cbind(a,判断1)
q<-merge(a,ax,by="判断1",all.x = T,all.y = F)
q2<-q[is.na(q$个人电核电话.y)==F,]
# 个数q2!=ax，因为同一天同一客户重复进件多次

#############################原表变换 注意剔除列的数据
colnames(q2)[which(colnames(q2)=="个人电核电话.x")]<-"个人电核电话"
a<-q2
# vlist为匹配出的上一工作日电核名单明细
vlist<-merge(c,a,by.x = "被叫号码",by.y="个人电核电话",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")



vlist<-subset(vlist,呼叫方式=="呼出")

####耗时过久 调整 9191条记录(尝试先匹配有的)####
vlistp2<-ddply(vlist,.(被叫号码),summarize,最后呼叫时间=max(as.POSIXct(开始时间)))
判断x<-paste(vlistp2$被叫号码,as.numeric(vlistp2$最后呼叫时间),sep="-")
vlistp2<-cbind(vlistp2,判断x)
判断y<-paste(vlist$被叫号码,as.numeric(as.POSIXct(vlist$开始时间)),sep="-")
vlist<-cbind(vlist,判断y)
qx<-merge(vlist,vlistp2,by.x="判断y",by.y="判断x",all.x = T,all.y = F)
qx<-qx[is.na(qx$最后呼叫时间)==F,]
vlist<-qx
vlist<-subset(vlist,呼叫结果=="接听")
vr<-which(colnames(vlist)=="被叫号码.x")
colnames(vlist)[vr]<-"被叫号码"


vlist2<-ddply(vlist,.(被叫号码,产品名称,区域中心),summarize,开始时间=max(as.POSIXct(开始时间)))
vlist2<-arrange(vlist2,开始时间)

开始时间2<-as.numeric(vlist2$开始时间)
区别号<-paste(vlist2$被叫号码,"-",开始时间2,sep="")
vlist2<-cbind(vlist2,区别号)

开始时间3<-as.numeric(as.POSIXct(vlist$开始时间))
区别号2<-paste(vlist$被叫号码,"-",开始时间3,sep="")
vlist<-cbind(vlist,区别号2)

vlist2<-vlist2[c("区别号","产品名称")]
vlist3<-merge(vlist2,vlist,by.x="区别号",by.y="区别号2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,开始时间)
vlist3<-vlist3[c("申请编号","通话时长","进件日期","产品名称.x","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间")]
colnames(vlist3)[which(colnames(vlist3)=="产品名称.x")]<-"产品名称"

vlist3<-subset(vlist3,产品名称=="B+C信用贷款" | 产品名称=="新易贷信用贷款" |产品名称=="B+新易贷二次营销")

vlist3$通话时长<-as.character(vlist3$通话时长)
通话时长x<-as.numeric(substr(vlist3$通话时长,7,8))+60*(as.numeric(substr(vlist3$通话时长,4,5)))+3600*(as.numeric(substr(vlist3$通话时长,1,2)))
vlist3<-cbind(vlist3,通话时长x)

vlist3<-vlist3[c("申请编号","通话时长x","进件日期","产品名称","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间","通话时长")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,电核类型=="电核个人")
qq<-subset(qq,电核结果=="通过")
qq<-qq[as.Date(qq$电核结束时间)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="申请编号",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$电核结果)==F,]

qq2<-qq2[c("申请编号","通话时长x","进件日期.x","产品名称.x","区域中心.x","受理城市.x",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

colnames(qq2)<-c("申请编号","通话时长s","进件日期","产品名称","区域中心","受理城市",
                 "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
                 "通话时长")

qq2p0<-qq2

##单电部分p1##
a<-abase
ax<-ddply(a,.(单位电核电话1),summarize,最后进件日=max(as.Date(进件日期)))
判断1<-paste(ax$单位电核电话1,"-",as.numeric(as.Date(ax$最后进件日)),sep="")
ax<-cbind(ax,判断1)
判断1<-paste(a$单位电核电话1,"-",as.numeric(as.Date(a$进件日期)),sep="")
a<-cbind(a,判断1)
q<-merge(a,ax,by="判断1",all.x = T,all.y = F)
q2<-q[is.na(q$单位电核电话1.y)==F,]
q2<-q2[q2$单位电核电话1.y!="",]
#############################原表变换 注意剔除列的数据
colnames(q2)[which(colnames(q2)=="单位电核电话1.x")]<-"单位电核电话"
a<-q2

vlist<-merge(c,a,by.x = "被叫号码",by.y="单位电核电话",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")


vlist<-subset(vlist,呼叫方式=="呼出")

####耗时过久 调整 9191条记录(尝试先匹配有的)####
vlistp2<-ddply(vlist,.(被叫号码),summarize,最后呼叫时间=max(as.POSIXct(开始时间)))
判断x<-paste(vlistp2$被叫号码,as.numeric(vlistp2$最后呼叫时间),sep="-")
vlistp2<-cbind(vlistp2,判断x)
判断y<-paste(vlist$被叫号码,as.numeric(as.POSIXct(vlist$开始时间)),sep="-")
vlist<-cbind(vlist,判断y)
qx<-merge(vlist,vlistp2,by.x="判断y",by.y="判断x",all.x = T,all.y = F)
qx<-qx[is.na(qx$最后呼叫时间)==F,]
vlist<-qx
vlist<-subset(vlist,呼叫结果=="接听")
vr<-which(colnames(vlist)=="被叫号码.x")
colnames(vlist)[vr]<-"被叫号码"

vlist2<-ddply(vlist,.(被叫号码,产品名称,区域中心),summarize,开始时间=max(as.POSIXct(开始时间)))
vlist2<-arrange(vlist2,开始时间)

开始时间2<-as.numeric(vlist2$开始时间)
区别号<-paste(vlist2$被叫号码,"-",开始时间2,sep="")
vlist2<-cbind(vlist2,区别号)

开始时间3<-as.numeric(as.POSIXct(vlist$开始时间))
区别号2<-paste(vlist$被叫号码,"-",开始时间3,sep="")
vlist<-cbind(vlist,区别号2)

vlist2<-vlist2[c("区别号","产品名称")]
vlist3<-merge(vlist2,vlist,by.x="区别号",by.y="区别号2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,开始时间)
vlist3<-vlist3[c("申请编号","通话时长","进件日期","产品名称.x","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间")]
colnames(vlist3)[which(colnames(vlist3)=="产品名称.x")]<-"产品名称"

vlist3<-subset(vlist3,产品名称=="B+C信用贷款" | 产品名称=="新易贷信用贷款" |产品名称=="B+新易贷二次营销")

vlist3$通话时长<-as.character(vlist3$通话时长)
通话时长x<-as.numeric(substr(vlist3$通话时长,7,8))+60*(as.numeric(substr(vlist3$通话时长,4,5)))+3600*(as.numeric(substr(vlist3$通话时长,1,2)))

vlist3<-cbind(vlist3,通话时长x)

vlist3<-vlist3[c("申请编号","通话时长x","进件日期","产品名称","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间","通话时长")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,电核类型=="电核单位")
qq<-subset(qq,电核结果=="通过")
qq<-qq[as.Date(qq$电核结束时间)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="申请编号",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$电核结果)==F,]

qq2<-qq2[c("申请编号","通话时长x","进件日期.x","产品名称.x","区域中心.x","受理城市.x",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

colnames(qq2)<-c("申请编号","通话时长s","进件日期","产品名称","区域中心","受理城市",
                 "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
                 "通话时长")

qq2p1<-qq2
##未完/拼接以下结果

##单电部分p2##
a<-abase
ax<-ddply(a,.(单位电核电话2),summarize,最后进件日=max(as.Date(进件日期)))
判断1<-paste(ax$单位电核电话2,"-",as.numeric(as.Date(ax$最后进件日)),sep="")
ax<-cbind(ax,判断1)
判断1<-paste(a$单位电核电话2,"-",as.numeric(as.Date(a$进件日期)),sep="")
a<-cbind(a,判断1)
q<-merge(a,ax,by="判断1",all.x = T,all.y = F)
q2<-q[is.na(q$单位电核电话2.y)==F,]
q2<-q2[q2$单位电核电话2.y!="",]
####原表变换 注意剔除列的数####
colnames(q2)[which(colnames(q2)=="单位电核电话2.x")]<-"单位电核电话"
a<-q2

vlist<-merge(c,a,by.x = "被叫号码",by.y="单位电核电话",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")

vlist<-subset(vlist,呼叫方式=="呼出")

####耗时过久 调整 9191条记录(尝试先匹配有的)####
vlistp2<-ddply(vlist,.(被叫号码),summarize,最后呼叫时间=max(as.POSIXct(开始时间)))
判断x<-paste(vlistp2$被叫号码,as.numeric(vlistp2$最后呼叫时间),sep="-")
vlistp2<-cbind(vlistp2,判断x)
判断y<-paste(vlist$被叫号码,as.numeric(as.POSIXct(vlist$开始时间)),sep="-")
vlist<-cbind(vlist,判断y)
qx<-merge(vlist,vlistp2,by.x="判断y",by.y="判断x",all.x = T,all.y = F)
qx<-qx[is.na(qx$最后呼叫时间)==F,]
vlist<-qx
vlist<-subset(vlist,呼叫结果=="接听")
vr<-which(colnames(vlist)=="被叫号码.x")
colnames(vlist)[vr]<-"被叫号码"
vlist2<-ddply(vlist,.(被叫号码,产品名称,区域中心),summarize,开始时间=max(as.POSIXct(开始时间)))
vlist2<-arrange(vlist2,开始时间)

开始时间2<-as.numeric(vlist2$开始时间)
区别号<-paste(vlist2$被叫号码,"-",开始时间2,sep="")
vlist2<-cbind(vlist2,区别号)

开始时间3<-as.numeric(as.POSIXct(vlist$开始时间))
区别号2<-paste(vlist$被叫号码,"-",开始时间3,sep="")
vlist<-cbind(vlist,区别号2)

vlist2<-vlist2[c("区别号","产品名称")]
vlist3<-merge(vlist2,vlist,by.x="区别号",by.y="区别号2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,开始时间)
vlist3<-vlist3[c("申请编号","通话时长","进件日期","产品名称.x","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间")]
colnames(vlist3)[which(colnames(vlist3)=="产品名称.x")]<-"产品名称"

vlist3<-subset(vlist3,产品名称=="B+C信用贷款" | 产品名称=="新易贷信用贷款" |产品名称=="B+新易贷二次营销")

vlist3$通话时长<-as.character(vlist3$通话时长)
通话时长x<-as.numeric(substr(vlist3$通话时长,7,8))+60*(as.numeric(substr(vlist3$通话时长,4,5)))+3600*(as.numeric(substr(vlist3$通话时长,1,2)))

vlist3<-cbind(vlist3,通话时长x)

vlist3<-vlist3[c("申请编号","通话时长x","进件日期","产品名称","区域中心","受理城市",
                 "被叫号码","坐席编号","开始时间","结束时间","通话时长")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,电核类型=="电核单位")
qq<-subset(qq,电核结果=="通过")
qq<-qq[as.Date(qq$电核结束时间)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="申请编号",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$电核结果)==F,]

qq2<-qq2[c("申请编号","通话时长x","进件日期.x","产品名称.x","区域中心.x","受理城市.x",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

colnames(qq2)<-c("申请编号","通话时长s","进件日期","产品名称","区域中心","受理城市",
                 "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
                 "通话时长")

qq2p2<-qq2
###合并2/3结果
q2f<-rbind(qq2p1,qq2p2)
q2f1<-unique(q2f)
q2f0<-qq2p0

#######个电q2f0 单电q2f1#####
std<-read.csv("lab/电核耗时标准.csv")
#先不分产品
idvd_l<-std$下限[3]
idvd_h<-std$上限[3]
wp_l<-std$下限[4]
wp_h<-std$上限[4]

q2f1<-arrange(q2f1,开始时间)
q2f0<-arrange(q2f0,开始时间)

q2f1$通话时长s<-as.numeric(q2f1$通话时长s)
q2f0$通话时长s<-as.numeric(q2f0$通话时长s)

个电低<-subset(q2f0,通话时长s<idvd_l)
个电高<-subset(q2f0,通话时长s>idvd_h)
个电正常<-subset(q2f0,通话时长s>=idvd_l & 通话时长s<=idvd_h)

单电低<-subset(q2f1,通话时长s<wp_l)
单电高<-subset(q2f1,通话时长s>wp_h)
单电正常<-subset(q2f1,通话时长s>=wp_l & 通话时长s<=wp_h)


###生成文件夹
dir.create(paste(as.character(Sys.Date())," 电核时效监控",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 电核时效监控/","新易贷-个电",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 电核时效监控/","新易贷-单电",sep = ""), showWarnings = FALSE)

if ( 1 %in% dhmode)
  
{spc_imrdh(个电正常,dir="新易贷-个电")}

if ( 2 %in% dhmode)
{spc_imrdh(单电正常,dir="新易贷-单电")}

个电低<-个电低[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

个电高<-个电高[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

个电正常<-个电正常[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
             "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
             "通话时长")]

单电低<-单电低[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

单电高<-单电高[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
           "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
           "通话时长")]

单电正常<-单电正常[c("申请编号","进件日期","产品名称","通话时长s","区域中心","受理城市",
             "客群类别","进件状态","当前流程步骤","被叫号码","坐席编号","开始时间","结束时间",
             "通话时长")]

##输出
write.csv(个电低,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-个电/1.时效不足件.csv",sep = ""))
write.csv(个电高,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-个电/2.超过时效件.csv",sep = ""))
write.csv(个电正常,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-个电/3.时效正常件.csv",sep = ""))

write.csv(单电低,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-单电/1.时效不足件.csv",sep = ""))
write.csv(单电高,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-单电/2.超过时效件.csv",sep = ""))
write.csv(单电正常,paste(as.character(Sys.Date())," 电核时效监控","/新易贷-单电/3.时效正常件.csv",sep = ""))}