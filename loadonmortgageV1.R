loadonmortgage<-function(date="today")
{
  if (date=="today")
  {aa<-as.character(format(Sys.Date(),"%Y%m%d"))}else{aa<-as.character(date)}
  download.file(paste("ftp://yunying:YYftp%232015@172.16.6.238/","乐享贷放款形式日报",as.character(format(Sys.Date(),"%Y%m%d")),".CSV",sep=""),
                paste("乐享贷放款形式日报",as.character(format(Sys.Date(),"%Y%m%d")),".CSV",sep=""))
  a<-read.csv(paste("乐享贷放款形式日报",aa,".CSV",sep=""),stringsAsFactors = F)
  # 他证未提交数
  b<-subset(a,a$放款审批形式=="收件收据"&a$当前进件状态!="结案")
  c<-ddply(b,.(区域中心,城市),summarize,number=length(进件编号))
  colnames(c)<-c("区域中心","城市","他证未提交数")
  # 异常结案数
  b1<-subset(a,a$放款审批形式=="收件收据"&a$上传.押品异常情况报送表.结束日期!="")
  b1<-ddply(b1,.(城市),summarize,number=length(进件编号))
  if (length(b1$城市)==0) {
    b1<-matrix(ncol = 2)
    colnames(b1)<-c("城市","异常结案数")
  }
  # 超期未收回
  b2<-subset(a,a$放款审批形式=="收件收据"&a$当前进件状态!="结案")
  c1<-subset(b2,b2$超时天数>1&b2$超时天数<=5)
  c2<-subset(b2,b2$超时天数>5&b2$超时天数<=10)
  c3<-subset(b2,b2$超时天数>10&b2$超时天数<=20)
  c4<-subset(b2,b2$超时天数>20)
  c11<-ddply(c1,.(城市),summarize,number=length(进件编号))
  if (length(c11$城市)==0) {
    c11<-matrix(ncol = 2)
    colnames(c11)<-c("城市","逾期5天以内数")
  }
  c21<-ddply(c2,.(城市),summarize,number=length(进件编号))
  if (length(c21$城市)==0) {
    c21<-matrix(ncol = 2)
    colnames(c21)<-c("城市","逾期6-10天数")
  }
  c31<-ddply(c3,.(城市),summarize,number=length(进件编号))
  if (length(c31$城市)==0) {
    c31<-matrix(ncol = 2)
    colnames(c31)<-c("城市","逾期11-20天数")
  }
  c41<-ddply(c4,.(城市),summarize,number=length(进件编号))
  if (length(c41$城市)==0) {
    c41<-matrix(ncol = 2)
    colnames(c41)<-c("城市","逾期20天以上数")
  }
  call<-merge(c11,c21,by = "城市",all = TRUE)
  call<-merge(call,c31,by = "城市",all = TRUE)
  call<-merge(call,c41,by = "城市",all = TRUE)
  final<-merge(c,call,by = "城市",all.x = TRUE)
  final<-merge(final,b1,by = "城市",all = TRUE)
  write.csv(final,paste("乐享贷他证收回情况",aa,".csv",sep=""))
}