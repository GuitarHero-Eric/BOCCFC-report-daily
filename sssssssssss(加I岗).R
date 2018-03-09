zlhspc<-function(ldate,rules=c(1:8),obn=3)
{ldate<-as.Date(ldate)
# 1资料核实B时间为负值
data<-read.csv("time.csv")
# 备注时间单位默认为Hour，转化为second
data[,c(5,8,11,14,17,20,23,26,29,32)]<-data[,c(5,8,11,14,17,20,23,26,29,32)]*60*60

a1<-data[,c(1:4,7:9)]
a2<-data[,c(1:4,10:12)]
a3<-data[,c(1:4,13:15)]
a4<-data[,c(1:4,16:18)]
a5<-data[,c(1:4,28:30)]
a6<-data[,c(1:4,31:33)]

a1<-na.omit(a1)
a2<-na.omit(a2)
a3<-na.omit(a3)
a4<-na.omit(a4)
a5<-na.omit(a5)
a6<-na.omit(a6)

a1<-a1[as.Date(a1$资料核实A最后认领时间)==ldate,]
a2<-a2[as.Date(a2$资料核实B最后认领时间)==ldate,]
a3<-a3[as.Date(a3$资料核实C最后认领时间)==ldate,]
a4<-a4[as.Date(a4$资料核实D最后认领时间)==ldate,]
a5<-a5[as.Date(a5$资料核实H最后认领时间)==ldate,]
a6<-a6[as.Date(a6$资料核实I最后认领时间)==ldate,]

a1<-a1[c(order(as.POSIXct(a1$资料核实A最后认领时间))),]
a2<-a2[c(order(as.POSIXct(a2$资料核实B最后认领时间))),]
a3<-a3[c(order(as.POSIXct(a3$资料核实C最后认领时间))),]
a4<-a4[c(order(as.POSIXct(a4$资料核实D最后认领时间))),]
a5<-a5[c(order(as.POSIXct(a5$资料核实H最后认领时间))),]
a6<-a6[c(order(as.POSIXct(a6$资料核实I最后认领时间))),]

# b为用时高于标准，c为用时低于标准,d为正常用时
b1<-subset(a1,a1[,6]>300)
c1<-subset(a1,a1[,6]<40)

b2<-subset(a2,a2[,6]>300)
c2<-subset(a2,a2[,6]<30)

b3<-subset(a3,a3[,6]>300)
c3<-subset(a3,a3[,6]<30)

b4<-subset(a4,a4[,6]>180)
c4<-subset(a4,a4[,6]<25)

b5<-subset(a5,a5[,6]>300)
c5<-subset(a5,a5[,6]<30)

b6<-subset(a6,a6[,6]>300)
c6<-subset(a6,a6[,6]<30)

d1<-subset(a1,a1[,6]>=40&a1[,6]<=300)

d2<-subset(a2,a2[,6]>=30&a2[,6]<=300)

d3<-subset(a3,a3[,6]>=30&a3[,6]<=300)

d4<-subset(a4,a4[,6]>=25&a4[,6]<=180)

d5<-subset(a5,a5[,6]>=30&a5[,6]<=300)

d6<-subset(a6,a6[,6]>=30&a6[,6]<=300)
# 筛除率
环节A筛除率<-(1-(nrow(d1)/nrow(a1)))
环节B筛除率<-(1-(nrow(d2)/nrow(a2)))
环节C筛除率<-(1-(nrow(d3)/nrow(a3)))
环节D筛除率<-(1-(nrow(d4)/nrow(a4)))
环节H筛除率<-(1-(nrow(d5)/nrow(a5)))
环节I筛除率<-(1-(nrow(d6)/nrow(a6)))
处理日期<-as.character(ldate)
筛除率<-as.data.frame(cbind(处理日期,环节A筛除率,环节B筛除率,环节C筛除率,环节D筛除率,环节H筛除率,环节I筛除率))

#数据源为d系列时/对于正常数据做监控图
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实A耗时",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实B耗时",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实C耗时",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实D耗时",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实H耗时",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实I耗时",sep = ""), showWarnings = FALSE)

###写入筛除率统计
write.csv(筛除率,paste(as.character(Sys.Date())," 资料核实时效监控/","筛除率汇总.csv",sep = ""))

###写入超出/不足时间要求的件
write.csv(b1,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实A耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c1,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实A耗时/2.不足目标时间件.csv",sep = ""))
write.csv(b2,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实B耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c2,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实B耗时/2.不足目标时间件.csv",sep = ""))
write.csv(b3,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实C耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c3,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实C耗时/2.不足目标时间件.csv",sep = ""))
write.csv(b4,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实D耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c4,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实D耗时/2.不足目标时间件.csv",sep = ""))
write.csv(b5,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实H耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c5,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实H耗时/2.不足目标时间件.csv",sep = ""))
write.csv(b6,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实I耗时/1.超出目标时间件.csv",sep = ""))
write.csv(c6,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实I耗时/2.不足目标时间件.csv",sep = ""))
write.csv(d1,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实A耗时/3.目标时间内件.csv",sep = ""))
write.csv(d2,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实B耗时/3.目标时间内件.csv",sep = ""))
write.csv(d3,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实C耗时/3.目标时间内件.csv",sep = ""))
write.csv(d4,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实D耗时/3.目标时间内件.csv",sep = ""))
write.csv(d5,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实H耗时/3.目标时间内件.csv",sep = ""))
write.csv(d6,paste(as.character(Sys.Date())," 资料核实时效监控/","资料核实I耗时/3.目标时间内件.csv",sep = ""))
###写入监控图等相关文件
spc_zlhsX(d1,rules=rules,obn=obn)
spc_zlhsX(d2,rules=rules,obn=obn)
spc_zlhsX(d3,rules=rules,obn=obn)
spc_zlhsX(d4,rules=rules,obn=obn)
spc_zlhsX(d5,rules=rules,obn=obn)
spc_zlhsX(d6,rules=rules,obn=obn)}

