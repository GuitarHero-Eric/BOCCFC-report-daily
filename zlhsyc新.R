zlhsyc<-function(ldate)
{a<-loadN()
x1<-paste(ldate," 09:30:00",sep="")
x2<-paste(ldate," 21:30:00",sep="")

a<-subset(a,当前流程步骤!="资料核实补件" & 当前流程步骤!="文字影像核查补件")
a$资料核实退件次数<-as.numeric(a$资料核实退件次数)
a$资料核实补件次数<-as.numeric(a$资料核实补件次数)

a<-subset(a,资料核实退件次数<1 & 资料核实补件次数<1)

a$资料核实开始时间[a$资料核实开始时间==""]<-"1900-1-3 0:00"
a$资料核实结束时间[a$资料核实结束时间==""]<-"1900-1-3 0:00"
a$面签结束时间[a$面签结束时间==""]<-"1900-1-3 0:00"


a$资料核实开始时间<-as.POSIXct(a$资料核实开始时间)
a$面签结束时间<-as.POSIXct(a$面签结束时间)
a$资料核实结束时间<-as.POSIXct(a$资料核实结束时间)
配偶件判定<-a$申请编号 %in% a$配偶进件关联标示
a<-cbind(a,配偶件判定)

a2<-subset(a,面签结束时间<x2 & 面签结束时间>x1)

##

axa1<-subset(a2,客群=="配偶客户")
if ((TRUE %in% (unique(as.character(axa1$配偶进件关联标示)) %in% unique(as.character(axa1$申请编号)))) == F)
{a2<-subset(a2,客群!="配偶客户" | 配偶件判定!=T)
print("无成对配偶件,程序继续")
}else{
  stop("出现成对配偶件,请检查脚本")
  break}

##demi  

资料核实时效 <- as.numeric(difftime(a2$资料核实结束时间,a2$面签结束时间,units="mins"))
a3<-cbind(a2,资料核实时效)
a3<-subset(a3,资料核实开始时间>as.POSIXct("2010-01-01 00:00:01"))
a3<-subset(a3,产品名称!="新易贷-乐享贷(等额本息)" &产品名称!="新易贷-乐享贷(轻松还)")

a4<-subset(a3,资料核实时效>30)
# 实际XYD表中超时件
a8<-round(nrow(a4)/nrow(a3),5)
总件数<-nrow(a3)



a4<-a4[c("申请编号","进件日期","产品名称","面签结束时间","资料核实开始时间","资料核实结束时间")]

#数据源为d系列时/对于正常数据做监控图
dir.create(paste(as.character(Sys.Date())," 资料核实超时监控",sep = ""), showWarnings = FALSE)
write.csv(a4,"temp/xxx.csv")

##超时件分析
tx<-read.csv("time2.csv")
# 异常件明细
a4<-read.csv("temp/xxx.csv")
a5<-merge(a4,tx,by="申请编号",all.x = T,all.y = F)
a5<-subset(a5,is.na(产品名称.y)==F)
超时件<-nrow(a5)
a8<-round(超时件/nrow(a3),5)
a5<-a5[c("申请编号","进件日期.x","产品名称.x","面签结束时间",
         "资料核实开始时间","资料核实结束时间","资料核实A最后认领时间",
         "资料核实A耗时","资料核实A人员","资料核实B最后认领时间","资料核实B耗时",
         "资料核实B人员","资料核实C最后认领时间",
         "资料核实C耗时","资料核实C人员","资料核实D最后认领时间",
         "资料核实D耗时","资料核实D人员","资料核实H最后认领时间",
         "资料核实H耗时","资料核实H人员","资料核实I最后认领时间",
         "资料核实I耗时","资料核实I人员","进件模式.客群.")]

a5$资料核实A最后认领时间<-as.character(a5$资料核实A最后认领时间)
a5$资料核实B最后认领时间<-as.character(a5$资料核实B最后认领时间)
a5$资料核实C最后认领时间<-as.character(a5$资料核实C最后认领时间)
a5$资料核实D最后认领时间<-as.character(a5$资料核实D最后认领时间)
a5$资料核实H最后认领时间<-as.character(a5$资料核实H最后认领时间)
a5$资料核实I最后认领时间<-as.character(a5$资料核实I最后认领时间)

a5$资料核实A最后认领时间[a5$资料核实A最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实B最后认领时间[a5$资料核实B最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实C最后认领时间[a5$资料核实C最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实D最后认领时间[a5$资料核实D最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实H最后认领时间[a5$资料核实H最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实I最后认领时间[a5$资料核实I最后认领时间==""]<-"1900-01-03 00:00:00"

a5$资料核实A最后认领时间[is.na(a5$资料核实A最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实B最后认领时间[is.na(a5$资料核实B最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实C最后认领时间[is.na(a5$资料核实C最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实D最后认领时间[is.na(a5$资料核实D最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实H最后认领时间[is.na(a5$资料核实H最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实I最后认领时间[is.na(a5$资料核实I最后认领时间)==T]<-"1900-01-03 00:00:00"

a5$资料核实A最后认领时间<-as.POSIXct(a5$资料核实A最后认领时间)
a5$资料核实B最后认领时间<-as.POSIXct(a5$资料核实B最后认领时间)
a5$资料核实C最后认领时间<-as.POSIXct(a5$资料核实C最后认领时间)
a5$资料核实D最后认领时间<-as.POSIXct(a5$资料核实D最后认领时间)
a5$资料核实H最后认领时间<-as.POSIXct(a5$资料核实H最后认领时间)
a5$资料核实I最后认领时间<-as.POSIXct(a5$资料核实I最后认领时间)
最后处理环节<-NULL
最后环节处理人<-NULL
for (xx in 1:length(a5[,1]))
{
  最后处理环节x<-colnames(a5[xx,c(7,10,13,16,19,22)])[which.max(as.numeric(as.character(a5[xx,c(7,10,13,16,19,22)])))]
  最后处理环节<-c(最后处理环节,最后处理环节x)
  最后环节处理人x<-as.character(a5[xx,((which(colnames(a5)==最后处理环节x))+2)])
  最后环节处理人<-c(最后环节处理人,最后环节处理人x)
}   
最后处理环节<-gsub("最后认领时间","",最后处理环节)
最后处理环节<-paste(最后处理环节,"环节",sep="")
a5<-cbind(a5,最后处理环节,最后环节处理人)
colnames(a5)[which(colnames(a5)=="进件日期.x")]<-"进件日期"
colnames(a5)[which(colnames(a5)=="产品名称.x")]<-"产品名称"

# 超时件详情
result1<-a5
比重表c<-as.data.frame(table(a5$最后处理环节))

环节A次数<-比重表c[which(比重表c[,1]=="资料核实A环节"),2]
环节B次数<-比重表c[which(比重表c[,1]=="资料核实B环节"),2]
环节C次数<-比重表c[which(比重表c[,1]=="资料核实C环节"),2]
环节D次数<-比重表c[which(比重表c[,1]=="资料核实D环节"),2]
环节H次数<-比重表c[which(比重表c[,1]=="资料核实H环节"),2]
环节I次数<-比重表c[which(比重表c[,1]=="资料核实I环节"),2]

if (length(环节A次数)==0)
{环节A次数<-0}
if (length(环节B次数)==0)
{环节B次数<-0}
if (length(环节C次数)==0)
{环节C次数<-0}
if (length(环节D次数)==0)
{环节D次数<-0}
if (length(环节H次数)==0)
{环节H次数<-0}
if (length(环节I次数)==0)
{环节I次数<-0}

环节A数c<-nrow(subset(a5,is.na(资料核实A耗时)==F))
环节B数c<-nrow(subset(a5,is.na(资料核实B耗时)==F))
环节C数c<-nrow(subset(a5,is.na(资料核实C耗时)==F))
环节D数c<-nrow(subset(a5,is.na(资料核实D耗时)==F))
环节H数c<-nrow(subset(a5,is.na(资料核实H耗时)==F))
环节I数c<-nrow(subset(a5,is.na(资料核实I耗时)==F))

环节A占比c<-round(环节A次数/环节A数c,5)
环节B占比c<-round(环节B次数/环节B数c,5)
环节C占比c<-round(环节C次数/环节C数c,5)
环节D占比c<-round(环节D次数/环节D数c,5)
环节H占比c<-round(环节H次数/环节H数c,5)
环节I占比c<-round(环节I次数/环节I数c,5)

环节<-c("总占比","资料核实A","资料核实B","资料核实C","资料核实D","资料核实H","资料核实I")
超时件<-c(超时件,环节A次数,环节B次数,环节C次数,环节D次数,环节H次数,环节I次数)
总处理件<-c(总件数,环节A数c,环节B数c,环节C数c,环节D数c,环节H数c,环节I数c)
超时占比<-c(a8,环节A占比c,环节B占比c,环节C占比c,环节D占比c,环节H占比c,环节I占比c)
超时件汇总<-cbind.data.frame(环节,超时件,总处理件,超时占比)

##全部
a4<-a3
a4<-a4[c("申请编号","进件日期","产品名称","面签结束时间","资料核实开始时间","资料核实结束时间")]

#数据源为d系列时/对于正常数据做监控图
write.csv(a4,"temp/xxx.csv")

##超时件分析
a5<-NULL
# 审批时效岗位
tx<-read.csv("time2.csv")
# 上个工作日资料核实总件数
a4<-read.csv("temp/xxx.csv")
a5<-merge(a4,tx,by="申请编号",all.x = T,all.y = F)
a5<-subset(a5,is.na(产品名称.y)==F)

a5<-a5[c("申请编号","进件日期.x","产品名称.x","面签结束时间",
         "资料核实开始时间","资料核实结束时间","资料核实A最后认领时间",
         "资料核实A耗时","资料核实A人员","资料核实B最后认领时间","资料核实B耗时",
         "资料核实B人员","资料核实C最后认领时间",
         "资料核实C耗时","资料核实C人员","资料核实D最后认领时间",
         "资料核实D耗时","资料核实D人员","资料核实H最后认领时间",
         "资料核实H耗时","资料核实H人员","资料核实I最后认领时间",
         "资料核实I耗时","资料核实I人员","进件模式.客群.")]

a5$资料核实A最后认领时间<-as.character(a5$资料核实A最后认领时间)
a5$资料核实B最后认领时间<-as.character(a5$资料核实B最后认领时间)
a5$资料核实C最后认领时间<-as.character(a5$资料核实C最后认领时间)
a5$资料核实D最后认领时间<-as.character(a5$资料核实D最后认领时间)
a5$资料核实H最后认领时间<-as.character(a5$资料核实H最后认领时间)
a5$资料核实I最后认领时间<-as.character(a5$资料核实I最后认领时间)

a5$资料核实A最后认领时间[a5$资料核实A最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实B最后认领时间[a5$资料核实B最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实C最后认领时间[a5$资料核实C最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实D最后认领时间[a5$资料核实D最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实H最后认领时间[a5$资料核实H最后认领时间==""]<-"1900-01-03 00:00:00"
a5$资料核实I最后认领时间[a5$资料核实I最后认领时间==""]<-"1900-01-03 00:00:00"

a5$资料核实A最后认领时间[is.na(a5$资料核实A最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实B最后认领时间[is.na(a5$资料核实B最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实C最后认领时间[is.na(a5$资料核实C最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实D最后认领时间[is.na(a5$资料核实D最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实H最后认领时间[is.na(a5$资料核实H最后认领时间)==T]<-"1900-01-03 00:00:00"
a5$资料核实I最后认领时间[is.na(a5$资料核实I最后认领时间)==T]<-"1900-01-03 00:00:00"

a5$资料核实A最后认领时间<-as.POSIXct(a5$资料核实A最后认领时间)
a5$资料核实B最后认领时间<-as.POSIXct(a5$资料核实B最后认领时间)
a5$资料核实C最后认领时间<-as.POSIXct(a5$资料核实C最后认领时间)
a5$资料核实D最后认领时间<-as.POSIXct(a5$资料核实D最后认领时间)
a5$资料核实H最后认领时间<-as.POSIXct(a5$资料核实H最后认领时间)
a5$资料核实I最后认领时间<-as.POSIXct(a5$资料核实I最后认领时间)

最后处理环节<-NULL
最后环节处理人<-NULL
for (xx in 1:length(a5[,1]))
{
  最后处理环节x<-colnames(a5[xx,c(7,10,13,16,19,22)])[which.max(as.numeric(as.character(a5[xx,c(7,10,13,16,19,22)])))]
  最后处理环节<-c(最后处理环节,最后处理环节x)
  最后环节处理人x<-as.character(a5[xx,((which(colnames(a5)==最后处理环节x))+2)])
  最后环节处理人<-c(最后环节处理人,最后环节处理人x)
}
最后处理环节<-gsub("最后认领时间","",最后处理环节)
最后处理环节<-paste(最后处理环节,"环节",sep="")
a5<-cbind(a5,最后处理环节,最后环节处理人)
colnames(a5)[which(colnames(a5)=="进件日期.x")]<-"进件日期"
colnames(a5)[which(colnames(a5)=="产品名称.x")]<-"产品名称"

环节A数<-nrow(subset(a5,is.na(资料核实A耗时)==F))
环节B数<-nrow(subset(a5,is.na(资料核实B耗时)==F))
环节C数<-nrow(subset(a5,is.na(资料核实C耗时)==F))
环节D数<-nrow(subset(a5,is.na(资料核实D耗时)==F))
环节H数<-nrow(subset(a5,is.na(资料核实H耗时)==F))
环节I数<-nrow(subset(a5,is.na(资料核实I耗时)==F))

比重表<-arrange(as.data.frame(table(a5$最后处理环节)),Var1)
环节A占比<-round(比重表[which(比重表[,1]=="资料核实A环节"),2]/环节A数,4)
环节B占比<-round(比重表[which(比重表[,1]=="资料核实B环节"),2]/环节B数,4)
环节C占比<-round(比重表[which(比重表[,1]=="资料核实C环节"),2]/环节C数,4)
环节D占比<-round(比重表[which(比重表[,1]=="资料核实D环节"),2]/环节D数,4)
环节H占比<-round(比重表[which(比重表[,1]=="资料核实H环节"),2]/环节H数,4)
环节I占比<-round(比重表[which(比重表[,1]=="资料核实I环节"),2]/环节I数,4)
##以上为表格统计完成
环节A耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实A最后认领时间)+as.difftime(a5$资料核实A耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节B耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实B最后认领时间)+as.difftime(a5$资料核实B耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节C耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实C最后认领时间)+as.difftime(a5$资料核实C耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节D耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实D最后认领时间)+as.difftime(a5$资料核实D耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节H耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实H最后认领时间)+as.difftime(a5$资料核实H耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节I耗时<-difftime(as.POSIXct(as.POSIXct(a5$资料核实I最后认领时间)+as.difftime(a5$资料核实H耗时,units = "hours")),as.POSIXct(a5$面签结束时间),units="mins")
环节A平均耗时<-mean(subset(环节A耗时,环节A耗时>0))
环节B平均耗时<-mean(subset(环节B耗时,环节B耗时>0))
环节C平均耗时<-mean(subset(环节C耗时,环节C耗时>0))
环节D平均耗时<-mean(subset(环节D耗时,环节D耗时>0))
环节H平均耗时<-mean(subset(环节H耗时,环节H耗时>0))
环节I平均耗时<-mean(subset(环节I耗时,环节I耗时>0))
环节A次数c<-比重表[which(比重表[,1]=="资料核实A环节"),2]
环节B次数c<-比重表[which(比重表[,1]=="资料核实B环节"),2]
环节C次数c<-比重表[which(比重表[,1]=="资料核实C环节"),2]
环节D次数c<-比重表[which(比重表[,1]=="资料核实D环节"),2]
环节H次数c<-比重表[which(比重表[,1]=="资料核实H环节"),2]
环节I次数c<-比重表[which(比重表[,1]=="资料核实I环节"),2]
if (length(环节A次数)==0)
{环节A次数<-0}
if (length(环节B次数)==0)
{环节B次数<-0}
if (length(环节C次数)==0)
{环节C次数<-0}
if (length(环节D次数)==0)
{环节D次数<-0}
if (length(环节H次数)==0)
{环节H次数<-0}
if (length(环节I次数)==0)
{环节I次数<-0}
if (length(环节A次数c)==0)
{环节A次数c<-0}
if (length(环节B次数c)==0)
{环节B次数c<-0}
if (length(环节C次数c)==0)
{环节C次数c<-0}
if (length(环节D次数c)==0)
{环节D次数c<-0}
if (length(环节H次数c)==0)
{环节H次数c<-0}
if (length(环节I次数c)==0)
{环节I次数c<-0}

if (length(环节A占比)==0)
{环节A占比<-0}
if (length(环节B占比)==0)
{环节B占比<-0}
if (length(环节C占比)==0)
{环节C占比<-0}
if (length(环节D占比)==0)
{环节D占比<-0}
if (length(环节H占比)==0)
{环节H占比<-0}
if (length(环节I占比)==0)
{环节I占比<-0}

环节<-c("资料核实A","资料核实B","资料核实C","资料核实D","资料核实H","资料核实I")
最后完成次数<-c(环节A次数c,环节B次数c,环节C次数c,环节D次数c,环节H次数c,环节I次数c)
完成总数<-c(环节A数,环节B数,环节C数,环节D数,环节H数,环节I数)
最后完成占比<-c(环节A占比,环节B占比,环节C占比,环节D占比,环节H占比,环节I占比)
平均耗时<-c(环节A平均耗时,环节B平均耗时,环节C平均耗时,环节D平均耗时,环节H平均耗时,环节I平均耗时)

汇总表<-cbind.data.frame(环节,最后完成次数,完成总数,最后完成占比,平均耗时)
write.csv(汇总表,paste(as.character(Sys.Date())," 资料核实超时监控/汇总表.csv",sep = ""))
write.csv(超时件汇总,paste(as.character(Sys.Date())," 资料核实超时监控/超时件汇总.csv",sep = ""))
write.csv(result1,paste(as.character(Sys.Date())," 资料核实超时监控/超时件详情.csv",sep = ""))
}