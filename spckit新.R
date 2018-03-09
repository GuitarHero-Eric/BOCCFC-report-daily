setwd("E:\\R路径")
###调用函数
##01.多图拼接
source("pac/multiplot.R")

##02.读取行列名去引号(CSV读取异常时可用)
source("pac/transN新.R")
source("pac/transN(4).R")
# source("pac/transN(二四合并).R")
##03.筛选函数I(与transN衔接)
source("pac/dfilt.R")

##04.筛选函数II(单独读取时用)
source("pac/dfilt_s.R")  

##05.标准I-MR做图
source("pac/spc_imr.R")  

##06.spc_imr函数异常点结果报告生成
source("pac/spc_imr2.R")

##07.末点判异
source("pac/spc_lpa.R")

##08.监控日报生成(四个基本指标)
source("pac/spcdr.R")

##09.特殊监控日报I(可指定任意指标进行拼接)
source("pac/spcdr_sp.R")

##10.特殊监控日报II(四个基本指标+征信评分研究日报)
source("pac/spcdrF.R")

##11.特殊监控日报III(四个基本指标+首逾率)
source("pac/spcdrS.R")

##12.指标研究用日报(任意一个指标+三个首逾率)
source("pac/spcdr_res.R")
source("pac/spcdr_res.R")

##13.标准xbar-s做图
source("pac/spc_xbar_s.R")

##14.配合xbar-s图的sbar运行图
source("pac/spc_xbar_sbar.R")

##15.占比型指标偏移度测试
source("pac/pro_bia.R")

##16.单独创建筛选列表
source("pac/dfilt_new.R")

##17.处理时效分析报告生成
source("pac/pro_eff.R")

##18.weizhi
source("pac/spc_xbarSS.R")

##19.
#old
source("pac/spcrepo_sp.R")

#new
source("pac/newspcdrs.R")

##20.
source("pac/spc_imrS.R")

##21.
source("pac/mixrepo.R")

##22.
source("pac/temp1.R")

##23.
source("pac/temp2.R")

##24.
source("pac/temp3.R")

##25.
source("pac/spc_xbar_sAA.R")

##26.
source("pac/spc_xbar_sbarAA.R")

##27.
source("pac/jiya新.R")
# source("pac/jiya.R")
# source("pac/jiya(2,4合并).R")

##28.
source("pac/load2.R")

##29.
source("pac/timex.R")

##29.
source("pac/spcrepo_sp3.R")

##29.
source("pac/spcdrF3.R")

##29.
source("pac/mixrepo3.R")

##30
source("pac/syl2.R")

##31
source("pac/syl.R")

##32
source("pac/colnamecorr.R")

##33
source("pac/ftablex.R")
source("pac/ftablet.R")

##34
source("pac/timex2x.R")

##35
# source("pac/wrepoXa2.R")
# source("pac/wrepoXa24.R")
source("pac/wrepoXa24renV1.R")
##35
source("pac/m1.R")

##35
source("pac/m2m3.R")

##36
source("pac/dher.R")

##37
source("pac/m4p.R")

##38
# source("pac/dherx.R")
# source("pac/dherx新.R")
source("pac/dherx-V1.R")
##39
source("pac/ywlspc.R")

##40
source("pac/ywlspc_imrx.R")

##41
source("pac/sssssssssss(加I岗).R")

##42
source("pac/spctype2.R")

##36
source("pac/dhmrepo.R")

##36
source("pac/timex3.R")

##36
source("pac/zlhsyc新.R")
# source("pac/zlhsyc(配偶贷改版).R")

##36
source("pac/kpart1.R")

# source("pac/kpart1xin.R")


##36
source("pac/kpart2.R")
##37
source("pac/五级分类.R")
source("pac/m1.R")
source("pac/m2m3.R")
source("pac/审批质检.R")
source("pac/transX.R")
source("pac/transW.R")
source("pac/loadonmortgageV1.R")
source("pac/网络贷M1每日监控v4.R")
###调用相关程序包       
library("stringr")
library("plyr")
library("ggplot2")
library("Cairo")
library("cairoDevice")
library("grid")
library("reshape2")
library("xlsx")
library("lubridate")
library("Johnson")
library(sqldf)
library(tidyr)