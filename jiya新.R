jiya<-function(xxx1,xxx2)
  #xxx1,xxx2分别代表电核及审批/放款审批两个阶段内经过休息日的天数，经过1个休息日即为一天
  #一般：#周一     xxx1=2 xxx2=2
  #周二     xxx1=2 xxx2=0
  #周三至五 xxx1=0 xxx2=0
{
  a<-loadN()
  
  a<-subset(a,产品名称=="B+C信用贷款"|产品名称=="新易贷信用贷款"|产品名称=="信用贷-中行联动")
  
  a<-subset(a,进件状态=="审核中"|进件状态=="未处理")
  
  #环节归类
  a$当前流程步骤<-gsub(c("新增初审|机审推荐|初审电核退回|客群选择|机审录入|客群确认|初审确认|初审授权|机审确认"),"初审",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("预审认领|申请表录入|补充信息录入|录入内容确认|预审确认"),"预审",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("面签认领|资料核实补件|信息查询及确认|审批一审补件|风险决策|面签客户认证|面签|附件上传确认|面签确认|面签助理|面签助理提交"),"面签",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("影响核查|文字核查|资料核实"),"资料核实",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("电核单位|电核个人|电核"),"电核",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("审批助理|审批二审|审批一审"),"审批",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("结果告知|用款申请"),"结果告知",a$当前流程步骤)
  a$当前流程步骤<-gsub(c("放款一审|放款二审"),"放款审批",a$当前流程步骤)
  a$当前流程步骤[grepl("补件",a$当前流程步骤,fixed=F)==T]<-"补件中"
  
  #寻找积压件1
  #假设假日为xxx天/4个时间依次为
  zlhstime1<-as.POSIXct(paste(as.character(Sys.Date()-1)," 20:00:00",sep=""),origin="1970-01-01 00:00:00",tz="Asia/Shanghai")
  dhtime1<-as.POSIXct(paste(as.character(Sys.Date()-2-xxx1)," 12:00:00",sep=""),origin="1970-01-01 00:00:00",tz="Asia/Shanghai")
  sptime1<-as.POSIXct(paste(as.character(Sys.Date()-1-xxx2)," 18:00:00",sep=""),origin="1970-01-01 00:00:00",tz="Asia/Shanghai")
  fksptime1<-as.POSIXct(paste(as.character(Sys.Date()-1-xxx2)," 18:00:00",sep=""),origin="1970-01-01 00:00:00",tz="Asia/Shanghai")
  
  a$面签结束时间[a$面签结束时间==""]<-"1900-1-3 0:00"
  a$资料核实结束时间[a$资料核实结束时间==""]<-"1900-1-3 0:00"
  a$电核结束时间[a$电核结束时间==""]<-"1900-1-3 0:00"
  a$结果告知结束时间[a$结果告知结束时间==""]<-"1900-1-3 0:00"
  
  zlhs<-as.POSIXct(a$面签结束时间,tz="Asia/Shanghai")
  dh<-as.POSIXct(a$资料核实结束时间,tz="Asia/Shanghai")
  sp<-as.POSIXct(a$电核结束时间,tz="Asia/Shanghai")
  fksp<-as.POSIXct(a$结果告知结束时间,tz="Asia/Shanghai")
  
  a<-cbind(a,zlhs,dh,sp,fksp)
  
  #寻找积压件2
  #1)资料核实
  qqa<-subset(a,当前流程步骤=="资料核实")
  qqa$zlhs<-as.POSIXct(qqa$zlhs)
  qqa2<-subset(qqa,zlhs<zlhstime1)
  if (length(qqa2[,1])>0)
  {for (i1 in 1:nrow(qqa2))
  {a[a$申请编号==qqa2$申请编号[i1],]$当前流程步骤<-"资料核实滞留"}
  }
  
  #2)电核
  qqb<-subset(a,当前流程步骤=="电核")
  qqb$dh<-as.POSIXct(qqb$dh)
  qqb2<-subset(qqb,dh<dhtime1)
  if (length(qqb2[,1])>0)
  {for (i2 in 1:nrow(qqb2))
  {a[a$申请编号==qqb2$申请编号[i2],]$当前流程步骤<-"电核滞留"}
  }
  
  #3)审批
  qqc<-subset(a,当前流程步骤=="审批")
  qqc$sp<-as.POSIXct(qqc$sp)
  qqc2<-subset(qqc,sp<sptime1)
  if (length(qqc2[,1])>0)
  {for (i3 in 1:nrow(qqc2))
  {a[a$申请编号==qqc2$申请编号[i3],]$当前流程步骤<-"审批滞留"}
  }
  
  #4)放款审批
  qqd<-subset(a,当前流程步骤=="放款审批")
  qqd$fksp<-as.POSIXct(qqd$fksp)
  qqd2<-subset(qqd,fksp<fksptime1)
  if (length(qqd2[,1])>0)
  {for (i4 in 1:nrow(qqd2))
  {a[a$申请编号==qqd2$申请编号[i4],]$当前流程步骤<-"放款审批滞留"}
  }
  
  #汇总结果1/总计
  aa<-ddply(a,.(当前流程步骤,区域中心),summarize,number=length(申请编号))
  aaa<-dcast(aa,区域中心~当前流程步骤)
  aaa[is.na(aaa)]<-0
  flist<-c("区域中心","初审","预审","面签","资料核实","资料核实滞留","电核","电核滞留","审批","审批滞留","结果告知","放款审批","放款审批滞留","补件中")
  play<-matrix(0,nrow=nrow(aaa),ncol=length(setdiff(flist,colnames(aaa))))
  aaa2<-cbind(aaa,play)
  colnames(aaa2)<-c(colnames(aaa),setdiff(flist,colnames(aaa)))
  
  #汇总结果2/具体件汇总
  
  
  #output
  bbb<-aaa2[flist]
  write.csv(bbb,paste(as.character(Sys.Date())," 盘库清单.csv",sep=""))
  
  #outputv2
  if (length(qqa2[,1])>0)
  {qqa2<-qqa2[c("申请编号","区域中心","受理城市","产品名称","客群","进件日期")]
  write.xlsx(qqa2,paste(as.character(Sys.Date())," 盘库清单积压件明细.xlsx",sep=""),sheetName="资料核实",append = T)}
  
  if (length(qqb2[,1])>0)
  {qqb2<-qqb2[c("申请编号","区域中心","受理城市","产品名称","客群","进件日期")]
  write.xlsx(qqb2,paste(as.character(Sys.Date())," 盘库清单积压件明细.xlsx",sep=""),sheetName="电核",append = T)}
  
  if (length(qqc2[,1])>0)
  {qqc2<-qqc2[c("申请编号","区域中心","受理城市","产品名称","客群","进件日期")]
  write.xlsx(qqc2,paste(as.character(Sys.Date())," 盘库清单积压件明细.xlsx",sep=""),sheetName="审批",append = T)}
  
  if (length(qqd2[,1])>0)
  {qqd2<-qqd2[c("申请编号","区域中心","受理城市","产品名称","客群","进件日期")]
  write.xlsx(qqd2,paste(as.character(Sys.Date())," 盘库清单积压件明细.xlsx",sep=""),sheetName="放款审批",append = T)}
}