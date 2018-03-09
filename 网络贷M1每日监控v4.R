loadonline<- function(j=0)
{
  file.remove("网络贷对日还回收率.xlsx")
  # DAY5M1回收率
  final<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    a<-subset(a,a$逾期阶段=="M1"&a$逾期天数=="5")
    a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
    a2<-sum(a$本金余额)
    a3<-rbind(a1,a2)
    b<-loadW(as.character(Sys.Date()-i-1))
    b<-subset(b,b$电催产品分类=="微贷款"|b$电催产品分类=="有还"|b$电催产品分类=="有借"|b$电催产品分类=="中银微贷款"|b$电催产品分类=="信用金")
    b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
    b<-subset(b,b$逾期阶段=="M1"&b$逾期天数=="4")
    b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum (本金余额))
    b2<-sum(b$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
    c2<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    final<-rbind(final,c2)
  }
  write.xlsx(final,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M1DAy5回收率",append = T)
  
  # DAY20M1回收率
  final<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    a<-subset(a,a$逾期阶段=="M1"&a$逾期天数=="20")
    a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
    a2<-sum(a$本金余额)
    a3<-rbind(a1,a2)
    b<-loadW(as.character(Sys.Date()-i-16))
    b<-subset(b,b$电催产品分类=="微贷款"|b$电催产品分类=="有还"|b$电催产品分类=="有借"|b$电催产品分类=="中银微贷款"|b$电催产品分类=="信用金")
    b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
    b<-subset(b,b$逾期阶段=="M1"&b$逾期天数=="4")
    b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum(本金余额))
    b2<-sum(b$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
    c2<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    final<-rbind(final,c2)
  }
  write.xlsx(final,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M1DAy20回收率",append = T)
  # Day30M1回收率
  final<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    a<-subset(a,a$逾期阶段=="M1"&a$逾期天数=="30")
    a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
    a2<-sum(a$本金余额)
    a3<-rbind(a1,a2)
    b<-loadW(as.character(Sys.Date()-i-26))
    b<-subset(b,b$电催产品分类=="微贷款"|b$电催产品分类=="有还"|b$电催产品分类=="有借"|b$电催产品分类=="中银微贷款"|b$电催产品分类=="信用金")
    b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
    b<-subset(b,b$逾期阶段=="M1"&b$逾期天数=="4")
    b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum(本金余额))
    b2<-sum(b$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
    c2<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    final<-rbind(final,c2)
  }
  write.xlsx(final,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M1Day30回收率",append = T)
  
  # Day60M2M3回收率
  finalm23<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    a<-subset(a,a$逾期阶段=="M3"&a$逾期天数=="90")
    a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
    a2<-sum(a$本金余额)
    a3<-rbind(a1,a2)
    b<-loadW(as.character(Sys.Date()-i-59))
    b<-subset(b,b$电催产品分类=="微贷款"|b$电催产品分类=="有还"|b$电催产品分类=="有借"|b$电催产品分类=="中银微贷款"|b$电催产品分类=="信用金")
    b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
    b<-subset(b,b$逾期阶段=="M2"&b$逾期天数=="31")
    b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum(本金余额))
    b2<-sum(b$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
    c2<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    finalm23<-rbind(finalm23,c2)
  }
  write.xlsx(finalm23,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M2M3Day60回收率",append = T)
  
  # 全账龄逾期状态
  finald<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    # 逾期
    ad<-subset(a,a$逾期天数!="0")
    a1<-ddply(ad,.(电催产品分类),summarize,逾期本金余额=sum(本金余额))
    a2<-sum(ad$本金余额)
    a3<-rbind(a1,a2)
    b1<-ddply(a,.(电催产品分类),summarize,总计本金余额=sum(本金余额))
    b2<-sum(a$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    c2<-c1$逾期本金余额/c1$总计本金余额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    finald<-rbind(finald,c2)
  }
  write.xlsx(finald,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还全账龄逾期比率",append = T)
  
  # DAY1逾期状态
  finald1<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    a<-subset(a,a$月结日==as.character(Sys.Date()+29))
    # 逾期
    ad<-subset(a,a$逾期天数=="1")
    a1<-ddply(ad,.(电催产品分类),summarize,逾期本金余额=sum(本金余额))
    a2<-sum(ad$本金余额)
    a3<-rbind(a1,a2)
    
    b1<-ddply(a,.(电催产品分类),summarize,总计本金余额=sum(本金余额))
    b2<-sum(a$本金余额)
    b3<-rbind(b1,b2)
    c1<-merge(a3,b3,by = "电催产品分类")
    c2<-c1$逾期本金余额/c1$总计本金余额
    c2<-data.frame(t(c2))
    colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
    finald1<-rbind(finald1,c2)
  }
  write.xlsx(finald1,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还Day1逾期比率",append = T)
  
  # 首次逾期
  firdue<-NULL
  for (i in c(j:0)) {
    a<-loadW(as.character(Sys.Date()-i))
    a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
    a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
    b<-subset(a,a$放款日期==as.character(Sys.Date()-32))
    c<-subset(a,a$逾期天数==1)
  }
  # write.xlsx(final,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M1DAy5回收率",append = T)
  # a<-subset(a,a$电催产品分类=="微贷款"|a$电催产品分类=="有还"|a$电催产品分类=="有借"|a$电催产品分类=="中银微贷款"|a$电催产品分类=="信用金")
  # a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
  # a<-subset(a,a$逾期阶段=="M1"&a$逾期天数=="4")
  # a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
  # a2<-sum(a$本金余额)
  # a3<-rbind(a1,a2)
  # b<-loadW(as.character(Sys.Date()-i-1))
  # b<-subset(b,b$电催产品分类=="微贷款"|b$电催产品分类=="有还"|b$电催产品分类=="有借"|b$电催产品分类=="中银微贷款"|b$电催产品分类=="信用金")
  # b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
  # b<-subset(b,b$逾期阶段=="M1"&b$逾期天数=="4")
  # b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum (本金余额))
  # b2<-sum(b$本金余额)
  # b3<-rbind(b1,b2)
  # c1<-merge(a3,b3,by = "电催产品分类")
  # # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
  # c2<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
  # c2<-data.frame(t(c2))
  # colnames(c2)<-c(as.character(c1[1,1]),as.character(c1[2,1]),as.character(c1[3,1]),as.character(c1[4,1]),as.character(c1[5,1]))
  # final<-rbind(final,c2)
  # 
  # 
  
  # # Day30M4+回收率
  # finalm4m7<-NULL
  # for (i in c(0:5)) {
  #   a<-loadW(as.character(Sys.Date()-i))
  #   a<-subset(a,a$额度产品=="新易贷-微贷款-京东金融"|a$额度产品=="新易贷-微贷款-乐贷款"|a$额度产品=="新易贷-微贷款-摩尔龙"|a$额度产品=="新易贷-微贷款-赏鱼袋"|a$额度产品=="新易贷-微贷款-小牛速贷"|a$额度产品=="新易贷-微贷款-一点即用"
  #             |a$额度产品=="新易贷-微贷款-易家贷"|a$额度产品=="新易贷-微贷款-友圈贷"|a$额度产品=="新易贷-微贷款-中银贷"|a$额度产品=="新易贷-圆周率分期"|a$额度产品=="信用金"|a$额度产品=="有还"|a$额度产品=="有借"|a$额度产品=="中银微贷款")
  #   a<-subset(a,a$还款日设定区分=="首次消费时确定还款日")
  #   a<-subset(a,a$逾期阶段=="M7"&a$逾期天数=="210")
  #   a1<-ddply(a,.(电催产品分类),summarize,本金余额末额=sum(本金余额))
  #   a2<-sum(a$本金余额)
  #   a3<-rbind(a1,a2)
  #   b<-loadW(as.character(Sys.Date()-i-119))
  #   b<-subset(b,b$额度产品=="新易贷-微贷款-京东金融"|b$额度产品=="新易贷-微贷款-乐贷款"|b$额度产品=="新易贷-微贷款-摩尔龙"|b$额度产品=="新易贷-微贷款-赏鱼袋"|b$额度产品=="新易贷-微贷款-小牛速贷"|b$额度产品=="新易贷-微贷款-一点即用"
  #             |b$额度产品=="新易贷-微贷款-易家贷"|b$额度产品=="新易贷-微贷款-友圈贷"|b$额度产品=="新易贷-微贷款-中银贷"|b$额度产品=="新易贷-圆周率分期"|b$额度产品=="信用金"|b$额度产品=="有还"|b$额度产品=="有借"|b$额度产品=="中银微贷款")
  #   b<-subset(b,b$还款日设定区分=="首次消费时确定还款日")
  #   b<-subset(b,b$逾期阶段=="M4"&b$逾期天数=="91")
  #   b1<-ddply(b,.(电催产品分类),summarize,本金余额初额=sum(本金余额))
  #   b2<-sum(b$本金余额)
  #   b3<-rbind(b1,b2)
  #   c1<-merge(a3,b3,by = "电催产品分类")
  #   # c<-(sum(sum(b$本金余额)-sum(a$本金余额)))/sum(b$本金余额)
  #   c1<-(c1$本金余额初额-c1$本金余额末额)/c1$本金余额初额
  #   c1<-data.frame(t(c1))
  #   colnames(c1)<-c("其他","微贷款","有还有借","中银微贷款","合计")
  #   finalm4m7<-rbind(finalm4m7,c1)
  # }
  # write.xlsx(finalm4m7,"网络贷对日还回收率.xlsx",sheetName="网络贷对日还M4回收率",append = T)
}




