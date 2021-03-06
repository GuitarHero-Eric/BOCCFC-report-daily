wrepoXV<-function(XXX=0)
  ###
{
  a<-loadN()
  # 误退补件剔除
  b<-read.csv("lab/误退补件报表.csv",stringsAsFactors = F)
  b$申请编号<-gsub(" ","",b$申请编号)
  for (n in 1:nrow(b)){
    if (duplicated(b$申请编号)[n] ==TRUE) {
      (print(paste("第",n+1,"行出现重复申请编号,请检查误退补件报表")))
    }
  }
  b<-b[!duplicated(b$申请编号),]
  a<-merge(a,b,by = "申请编号",all =TRUE )
  a$资料核实误补件次数[is.na(a$资料核实误补件次数)]<-0
  a$资料核实补件次数<-a$资料核实补件次数-a$资料核实误补件次数
  a$资料核实误退件次数[is.na(a$资料核实误退件次数)]<-0
  a$资料核实退件次数<-a$资料核实退件次数-a$资料核实误退件次数
  
  # c1为退件次数误差件，c2为补件误差件
  c1<-a[a$资料核实退件次数<0,]
  c2<-a[a$资料核实补件次数<0,]
  if (nrow(c1)!=0){
    print("资料核实退件次数<0")
  }
  
  if (nrow(c2)!=0){
    print("资料核实补件次数<0")
  }
  
  
  #加放款误退补件剔除
  b<-read.csv("lab/放款误退补件报表.csv",stringsAsFactors = F)
  b$申请编号<-gsub(" ","",b$申请编号)
  for (n in 1:nrow(b)){
    if (duplicated(b$申请编号)[n] ==TRUE) {
      (print(paste("第",n+1,"行出现重复申请编号,请检查误退补件报表")))
    }
  }
  b<-b[!duplicated(b$申请编号),]
  a<-merge(a,b,by = "申请编号",all =TRUE )
  a$放款审批误补件次数[is.na(a$放款审批误补件次数)]<-0
  a$放款审批补件次数<-a$放款审批补件次数-a$放款审批误补件次数
  a$放款审批误退件次数[is.na(a$放款审批误退件次数)]<-0
  a$放款审批退件次数<-a$放款审批退件次数-a$放款审批误退件次数
  
  b<-read.csv("lab/节假日表.csv",stringsAsFactors = F)
  b$休息日<-as.POSIXct(b$休息日)
  
  
  
  a$放款二审结束时间[a$放款二审结束时间==""]<-"1900-1-3 0:00"
  a$结果告知结束时间[a$结果告知结束时间==""]<-"1900-1-3 0:00"
  a$面签开始时间[a$面签开始时间==""]<-"1900-1-3 0:00"
  a$进件日期<-as.Date(a$进件日期)
  
  最终处理时间<-"1900-1-3 0:00"
  最终处理时间<-as.POSIXct(最终处理时间)
  a<-cbind.data.frame(a,最终处理时间)
  a$最终处理时间[as.POSIXct(a$最终处理时间)<as.POSIXct(a$放款二审结束时间)]<-a$放款二审结束时间[as.POSIXct(a$最终处理时间)<as.POSIXct(a$放款二审结束时间)]
  a$最终处理时间[as.POSIXct(a$最终处理时间)<as.POSIXct(a$结果告知结束时间)]<-a$结果告知结束时间[as.POSIXct(a$最终处理时间)<as.POSIXct(a$结果告知结束时间)]
  
  
  aa1<-as.POSIXct(a$面签开始时间)
  aa2<-as.POSIXct(a$最终处理时间)
  a$区域中心<-as.character(a$区域中心)
  
  ##特殊情况对于各环节时效影响
  时效条件剔除判断<-"FALSE"
  a<-cbind(a,时效条件剔除判断)
  a$时效条件剔除判断<-as.character(a$时效条件剔除判断)
  
  ###标记后整件剔除区域
  #湖北
  a$时效条件剔除判断[a$区域中心=="湖北区域中心" & a$进件日期>=as.Date("2016-06-29") & a$进件日期<as.Date("2016-07-18")]<-"TRUE"
  
  #河北
  a$时效条件剔除判断[a$区域中心=="河北区域中心" & a$进件日期>=as.Date("2016-07-19") & a$进件日期<as.Date("2016-07-22")]<-"TRUE"
  
  #安徽
  a$时效条件剔除判断[a$区域中心=="安徽区域中心" & a$进件日期>=as.Date("2016-07-06") & a$进件日期<as.Date("2016-07-24")]<-"TRUE"
  ###END of Mi1
  # 面签横跨休息日天数
  kl2<-NULL
  for (p1 in 1:length(aa1))
  {kl<-length((aa1[p1]>b$休息日)[(aa1[p1]>b$休息日)==T])
  kl2<-rbind(kl2,kl)}
  # 放款审批结束时间横跨休息日天数
  km2<-NULL
  for (p2 in 1:length(aa2))
  {km<-length((aa2[p2]>b$休息日)[(aa2[p2]>b$休息日)==T])
  km2<-rbind(km2,km)}
  # 耗时横跨休息日时间  单位天
  kn2<-km2-kl2
  
  b<-read.csv("lab/节假日表.csv",stringsAsFactors = F)
  
  addi<-as.Date(a$面签开始时间) %in% as.Date(b$休息日)
  addi[addi==F]<-0
  addi[addi==T]<-1
  # 面签开始时间是否为休息日，1为是，0为否
  addi2<-as.Date(a$面签开始时间) %in% as.Date(b$休息日)
  
  ##kn3实际操作天数
  kn3<-((as.Date(aa2)-as.Date(aa1))-(kn2+addi))+1
  # kn4为包含非工作时间全流程耗时
  kn4<-((aa2-aa1)-(kn2*24*60*60))/60/60
  
  
  kn5<-24-hour(aa1)
  kn5[kn5==24]<-0
  
  kn7<-minute(aa1)
  kn7[kn7!=0]<-(kn7[kn7!=0]*-1)/60
  # 面签当天所耗时长  单位小时
  kn5<-kn5+kn7
  
  bbb<-cbind(a,kn3,kn4,addi2,kn5)
  
  bbb$kn5[bbb$addi2==F]<-0
  bbb$kn5[bbb$addi2==T]<-bbb$kn5[bbb$addi2==T]+9
  kn5<-bbb$kn5
  
  kn6<-((aa2-aa1)-(kn2*24*60*60)-(kn5*60*60))/60/60
  
  bbb<-cbind(bbb,kn6)
  ###
  colnames(bbb)[ncol(bbb)-4]<-"实际操作天数"
  colnames(bbb)[ncol(bbb)]<-"全流程总耗时"
  
  
  ###
  kk<-subset(bbb,select=c(申请编号,进件日期,区域中心,受理城市,受理网点,推荐网点,产品名称,客群,进件状态,当前流程步骤上级归纳,当前流程步骤,是否T3,实际操作天数,全流程总耗时,初审开始时间,
                              初审结束时间,初审结果,初审人,初审人编号,预审开始时间,预审结束时间,预审结果,预审人,预审人编号,
                              面签开始时间,面签结束时间,面签结果,面签人,面签人编号,资料核实开始时间,资料核实结束时间,
                              资料核实补件次数,资料核实退件次数,电核开始时间,电核结束时间,电核补件次数,电核退件次数,审批助理开始时间,
                              审批助理结束时间,审批助理处理人,审批一审开始时间,审批一审结束时间,申请额度,首次放款时间,
                              审批补件次数,审批退件次数,审批捞件次数,审批结果,审批一审审批人,结果告知开始时间,结果告知结束时间,结果告知结果,
                              放款一审开始时间,放款一审结束时间,放款一审审批人,放款二审开始时间,放款二审结束时间,放款二审审批人,放款审批补件次数,放款审批退件次数,放款审批结果,放贷金额,
                              首逾标识,时效条件剔除判断,机构简称,已动用进件编号1,已动用进件编号2,已动用进件编号3,渠道初审标识,自动审批标识,结案时间,是否二次进件))  #最终处理时间
  
  ###以上
  kk$全流程总耗时[kk$全流程总耗时<0]<-kk$全流程总耗时[kk$全流程总耗时<0]+24
  
  kk$初审开始时间[kk$初审开始时间==""]<-"1900-1-3 0:00"
  kk$初审结束时间[kk$初审结束时间==""]<-"1900-1-3 0:00"
  
  kk$预审开始时间[kk$预审开始时间==""]<-"1900-1-3 0:00"
  kk$预审结束时间[kk$预审结束时间==""]<-"1900-1-3 0:00"
  
  kk$面签开始时间[kk$面签开始时间==""]<-"1900-1-3 0:00"
  kk$面签结束时间[kk$面签结束时间==""]<-"1900-1-3 0:00"
  
  kk$资料核实开始时间[kk$资料核实开始时间==""]<-"1900-1-3 0:00"
  kk$资料核实结束时间[kk$资料核实结束时间==""]<-"1900-1-3 0:00"
  
  kk$审批助理开始时间[kk$审批助理开始时间==""]<-"1900-1-3 0:00"
  kk$审批助理结束时间[kk$审批助理结束时间==""]<-"1900-1-3 0:00"
  
  kk$审批一审开始时间[kk$审批一审开始时间==""]<-"1900-1-3 0:00"
  kk$审批一审结束时间[kk$审批一审结束时间==""]<-"1900-1-3 0:00"
  
  kk$结果告知开始时间[kk$结果告知开始时间==""]<-"1900-1-3 0:00"
  kk$结果告知结束时间[kk$结果告知结束时间==""]<-"1900-1-3 0:00"
  kk$放款一审结束时间[kk$放款一审结束时间==""]<-"1900-1-3 0:00"
  kk$放款二审结束时间[kk$放款二审结束时间==""]<-"1900-1-3 0:00"
  kk$放款一审开始时间[kk$放款一审开始时间==""]<-"1900-1-3 0:00"
  kk$放款二审开始时间[kk$放款二审开始时间==""]<-"1900-1-3 0:00"
  kk$电核开始时间[kk$电核开始时间==""]<-"1900-1-3 0:00"
  kk$电核结束时间[kk$电核结束时间==""]<-"1900-1-3 0:00"
  
  
  最终处理日期<-"1900-1-3"
  kk<-cbind.data.frame(kk,最终处理日期)
  kk$最终处理日期<-as.character(kk$最终处理日期)
  kk$最终处理日期[as.Date(kk$初审开始时间)<as.Date(kk$初审结束时间)]<-kk$初审结束时间[as.Date(kk$初审开始时间)<as.Date(kk$初审结束时间)]
  kk$最终处理日期[as.Date(kk$初审开始时间)>as.Date(kk$初审结束时间)]<-kk$初审开始时间[as.Date(kk$初审开始时间)>as.Date(kk$初审结束时间)]
  kk$最终处理日期[as.Date(kk$初审开始时间)==as.Date(kk$初审结束时间)]<-kk$初审结束时间[as.Date(kk$初审开始时间)==as.Date(kk$初审结束时间)]
  
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$预审开始时间)]<-kk$预审开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$预审开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$预审结束时间)]<-kk$预审结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$预审结束时间)]
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$面签开始时间)]<-kk$面签开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$面签开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$面签结束时间)]<-kk$面签结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$面签结束时间)]
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$资料核实开始时间)]<-kk$资料核实开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$资料核实开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$资料核实结束时间)]<-kk$资料核实结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$资料核实结束时间)]
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$审批助理开始时间)]<-kk$审批助理开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$审批助理开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$审批助理结束时间)]<-kk$审批助理结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$审批助理结束时间)]
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$审批一审开始时间)]<-kk$审批一审开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$审批一审开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$审批一审结束时间)]<-kk$审批一审结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$审批一审结束时间)]
  
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$结果告知开始时间)]<-kk$结果告知开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$结果告知开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$结果告知结束时间)]<-kk$结果告知结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$结果告知结束时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$放款一审开始时间)]<-kk$放款一审开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$放款一审开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$放款一审结束时间)]<-kk$放款一审结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$放款一审结束时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$放款二审开始时间)]<-kk$放款二审开始时间[as.Date(kk$最终处理日期)<=as.Date(kk$放款二审开始时间)]
  kk$最终处理日期[as.Date(kk$最终处理日期)<=as.Date(kk$放款二审结束时间)]<-kk$放款二审结束时间[as.Date(kk$最终处理日期)<=as.Date(kk$放款二审结束时间)]
  
  kk$最终处理日期<-as.character(kk$最终处理日期)
  
  
  
  ###影响处理时效后修正区域
  ##1.5个重叠条件 8/11
  con1<-as.POSIXct(kk$资料核实开始时间)>=as.POSIXct("2016-07-26 15:00:00") & as.POSIXct(kk$资料核实结束时间)<=as.POSIXct("2016-07-26 19:50:00") & kk$全流程总耗时>12
  con2<-as.POSIXct(kk$电核开始时间)>=as.POSIXct("2016-07-26 15:00:00") & as.POSIXct(kk$电核结束时间)<=as.POSIXct("2016-07-26 19:50:00") & kk$全流程总耗时>12
  con3<-as.POSIXct(kk$资料核实开始时间)>=as.POSIXct("2016-07-27 09:52:00") & as.POSIXct(kk$资料核实结束时间)<=as.POSIXct("2016-07-27 17:22:00") & kk$全流程总耗时>12
  con4<-as.POSIXct(kk$电核开始时间)>=as.POSIXct("2016-07-27 09:52:00") & as.POSIXct(kk$电核结束时间)<=as.POSIXct("2016-07-27 17:22:00") & kk$全流程总耗时>12
  con5<-as.POSIXct(kk$面签结束时间)>=as.POSIXct("2016-07-27 16:00:00") & as.POSIXct(kk$面签结束时间)<=as.POSIXct("2016-07-28 04:00:00") & kk$受理网点=="贵州贵阳受理中心"  & kk$全流程总耗时>12
  
  kk$全流程总耗时[con1|con2|con3|con4|con5]<-kk$全流程总耗时[con1|con2|con3|con4|con5]-12
  
  ##2.3个分散条件 8/11
  kk$全流程总耗时[kk$申请编号=="BO20160719006038"]<-(kk$全流程总耗时[kk$申请编号=="BO20160719006038"])-20
  
  kk$全流程总耗时[kk$申请编号=="BO20160718007023"]<-(kk$全流程总耗时[kk$申请编号=="BO20160718007023"])-19.5
  
  kk$全流程总耗时[kk$申请编号=="BO20160720004780"]<-(kk$全流程总耗时[kk$申请编号=="BO20160720004780"])-17
  
  ##广东 8/12
  con6<-kk$进件状态=="结案" & kk$区域中心=="广东区域中心" & as.Date(kk$最终处理日期)=="2016-08-03" & kk$全流程总耗时>24
  kk$全流程总耗时[con6]<-kk$全流程总耗时[con6]-24
  
  ##贵州 9/12
  con912<-kk$受理网点=="贵州遵义受理中心" & as.Date(kk$进件日期)<=as.Date("2016-09-09") & as.Date(kk$放款二审结束时间)>as.Date("2016-09-09")
  kk$全流程总耗时[con912]<-kk$全流程总耗时[con912]-24
  
  #####T3剔除标记字段加入 161008
  T3剔除标记<-0
  kk<-cbind(kk,T3剔除标记)
  kk$T3剔除标记[kk$申请编号=="BO20160923008806"]<-1
  kk$T3剔除标记[kk$申请编号=="BO20160923010415"]<-1
  
  ##海南10/21
  con1021<-as.POSIXct(kk$面签开始时间)<=as.POSIXct("2016-10-18 00:00:01") & as.POSIXct(kk$放款二审结束时间)>=as.POSIXct("2016-10-19 00:00:01") & kk$区域中心=="海南区域中心"  & kk$全流程总耗时>24
  kk$全流程总耗时[con1021]<-kk$全流程总耗时[con1021]-24
  
  ##广西三月三
  con1021<-as.POSIXct(kk$面签开始时间)<=as.POSIXct("2017-03-31 00:00:01") & as.POSIXct(kk$放款二审结束时间)>=as.POSIXct("2017-04-05 00:00:01") & kk$区域中心=="广西区域中心"  & kk$全流程总耗时>48
  kk$全流程总耗时[con1021]<-kk$全流程总耗时[con1021]-48
  
  ###END of Mi2
  #colnames(kk)[colnames(kk)=="最终处理时间"]<-"结案时间"
  # kk1<-kk[!duplicated(kk$申请编号),]
  #write.csv(kk1,paste(as.character(Sys.Date())," 日报数据24(ren)test.csv",sep=""))
  write.csv(kk,paste(as.character(Sys.Date())," 日报数据test12.csv",sep=""))
}