downloadfile <- function(date="today")
{
  if (date=="today")
  {date <- as.character(format(Sys.Date(),"%Y%m%d"))}else{
    date<-as.Date(date)
    date <- as.character(format(date,"%Y%m%d"))}
  
  customerfile <- paste("客户产品余额五级分类日报_",date,".zip",sep="")
  #定义XYD表文件名
  download.file(paste("ftp://fengxian:fengxian@172.16.6.238/",customerfile,sep=""),customerfile)
  #下载当日XYD文件
  unzip(customerfile,exdir = "daily_report")
  #解压XYD文件
  file.remove(customerfile)
  #删除XYD压缩文件
  a <- c(0,1)
  for (i in a) {
    customerfilecsv <- paste("客户产品余额五级分类日报",date,"_",i,".csv",sep="")
    customerfilecsv <- paste("daily_report/",customerfilecsv,sep="")
    xt1 <- read.csv(customerfilecsv)
    if (i==0)
    {xtt <- xt1}
    else
    {xtt <- rbind(xtt,xt1)}
  }
  
  product_clsssify <- paste("模板/","贾哥产品归类",".CSV",sep="")
  product_clsssify <- read.csv(product_clsssify)
  
  city_clsssify <- paste("模板/","贾哥区域城市",".CSV",sep="")
  city_clsssify <- read.csv(city_clsssify)
  xtt <- merge(xtt,product_clsssify,by="额度产品",all.x=T)
  xtt <- merge(xtt,city_clsssify,by="受理区域",all.x=T)
  
  library(sqldf)
  library(tidyr)
  # 全产品
  product_sum <- sqldf("select 产品 as ClASSFY,逾期阶段 AS PERIOD,sum(本金余额) AS BALANCE,COUNT(本金余额) as CUSTOMER_NUM from xtt where 本金余额>0 group by 产品,逾期阶段")
  product_sum1<-product_sum[,c(1,2,3)]
  product_sum2<-product_sum[,c(1,2,4)]
  product_sum1<-spread(product_sum1,PERIOD,BALANCE)
  product_sum1<-product_sum1[order(product_sum1[,2],decreasing = T),]
  product_sum1[is.na(product_sum1)]<-0
  product_sum2<-spread(product_sum2,PERIOD,CUSTOMER_NUM)
  product_sum2<-product_sum2[order(product_sum2[,2],decreasing = T),]
  product_sum2[is.na(product_sum2)]<-0
  write.xlsx(product_sum1,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"金额",sep = ""),append = T)
  write.xlsx(product_sum2,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"户数",sep = ""),append = T)
  
  # 商户贷产品
  commercial <- sqldf("select 额度产品 as ClASSFY,逾期阶段 as PERIOD,sum(本金余额) AS BALANCE,COUNT(本金余额) as CUSTOMER_NUM 
                      FROM xtt 
                      where 本金余额>0
                      and 产品 in ('商户专享贷 ')
                      group by 额度产品,逾期阶段")
  commercial1<-commercial[,c(1,2,3)]
  commercial2<-commercial[,c(1,2,4)]
  commercial1<-spread(commercial1,PERIOD,BALANCE)
  commercial2<-spread(commercial2,PERIOD,CUSTOMER_NUM)
  commercial1<-commercial1[order(commercial1[,2],decreasing = T),]
  commercial1[is.na(commercial1)]<-0
  commercial2<-commercial2[order(commercial2[,2],decreasing = T),]
  commercial2[is.na(commercial2)]<-0
  write.xlsx(commercial1,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"商户金额",sep = ""),append = T)
  write.xlsx(commercial2,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"商户户数",sep = ""),append = T)
  # 商户贷省份
  commercialprovince<-sqldf("select 区域中心 as PROVINCE ,逾期阶段 as PERIOD,sum(本金余额) AS BALANCE,COUNT(本金余额) as CUSTOMER_NUM
                            FROM xtt 
                            where 本金余额>0
                            and 产品 = '商户专享贷 '
                            group by 区域中心,逾期阶段")
  commercialprovince$PROVINCE[is.na(commercialprovince$PROVINCE)]<-"上海"
  commercialprovince<-sqldf("select PROVINCE,PERIOD,sum(BALANCE) as BALANCE,sum(CUSTOMER_NUM) as CUSTOMER_NUM
                            from commercialprovince 
                            group by PROVINCE,PERIOD")
  
  commercialprovince1<-commercialprovince[,c(1,2,3)]
  commercialprovince2<-commercialprovince[,c(1,2,4)]
  commercialprovince1<-spread(commercialprovince1,PERIOD,BALANCE)
  commercialprovince2<-spread(commercialprovince2,PERIOD,CUSTOMER_NUM)
  commercialprovince2<-commercialprovince2[order(commercialprovince2[,2],decreasing = T),]
  commercialprovince2[is.na(commercialprovince2)]<-0
  commercialprovince1<-commercialprovince1[order(commercialprovince1[,2],decreasing = T),]
  commercialprovince1[is.na(commercialprovince1)]<-0
  write.xlsx(commercialprovince1,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"商户区域金额",sep = ""),append = T)
  write.xlsx(commercialprovince2,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"商户区域户数",sep = ""),append = T)
  # 现金贷
  cash<-sqldf("select 区域中心 as PROVINCE,产品 as ClASSFY,还款日设定区分 as paypattern,逾期阶段 as PERIOD,sum(本金余额) AS BALANCE,COUNT(本金余额) as CUSTOMER_NUM 
              FROM xtt where 产品 = '乐享贷/A+B' or 产品 ='新易贷信用贷'
              group by 区域中心,产品,还款日设定区分,逾期阶段")
  cash1<-cash[,c(1:5)]
  cash2<-cash[,c(1:4,6)]
  cash1<-spread(cash1,PERIOD,BALANCE)
  cash2<-spread(cash2,PERIOD,CUSTOMER_NUM)
  cash1<-cash1[order(cash1[,2],decreasing = T),]
  cash1[is.na(cash1)]<-0
  cash2<-cash2[order(cash2[,2],decreasing = T),]
  cash2[is.na(cash2)]<-0
  cash1<-arrange(cash1,cash1$PROVINCE,cash1$ClASSFY,cash1$paypattern)
  cash2<-arrange(cash2,cash2$PROVINCE,cash2$ClASSFY,cash2$paypattern)
  cash1$paypattern<-gsub("产品属性确定还款日","对月还款",cash1$paypattern)
  cash1$paypattern<-gsub("首次消费时确定还款日","对日还款",cash1$paypattern)
  cash2$paypattern<-gsub("产品属性确定还款日","对月还款",cash2$paypattern)
  cash2$paypattern<-gsub("首次消费时确定还款日","对日还款",cash2$paypattern)
  
  write.xlsx(cash1,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"现金贷金额",sep = ""),append = T)
  write.xlsx(cash2,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"现金贷户数",sep = ""),append = T)
  
  
  # 网络贷
  internet<-sqldf("select 额度产品 as ClASSFY,逾期阶段 as PERIOD,sum(本金余额) AS BALANCE,COUNT(本金余额) as CUSTOMER_NUM
                  FROM xtt 
                  where 产品 = '对日还款产品线—网络贷款'
                  group by 额度产品,逾期阶段")
  
  internet1<-internet[,c(1,2,3)]
  internet2<-internet[,c(1,2,4)]
  internet1<-spread(internet1,PERIOD,BALANCE)
  internet2<-spread(internet2,PERIOD,CUSTOMER_NUM)
  internet1<-internet1[order(internet1[,2],decreasing = T),]
  internet1[is.na(internet1)]<-0
  internet2<-internet2[order(internet2[,2],decreasing = T),]
  internet2[is.na(internet2)]<-0
  write.xlsx(internet1,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"网络贷金额",sep = ""),append = T)
  write.xlsx(internet2,paste("daily_report/产品线逾期监控日报表",date,".xlsx",sep=""),sheetName=paste((date),"网络贷户数",sep = ""),append = T)
  product_error<-xtt[is.na(xtt$产品),]
  province_error<-xtt[is.na(xtt$区域中心),]
  
  table(province_error$受理区域)
  table(product_error$额度产品)
}
