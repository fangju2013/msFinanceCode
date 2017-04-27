#draw the statistic graph by the different product

library(ggplot2)
library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from data_basic.fj_clctAllInfo",as.is=T)
odbcClose(ch)
attach(raw.data)

#choose the prod_cd to the given form 
prod_handle <- function(df,str){
  tmp <- df[,str] %>% as.character()
  for(i in 1:length(tmp)){
    if (is.na(tmp[i])) tmp[i] <- ''
    else {
      if (substr(tmp[i],1,1) == '1') tmp[i] <- '1#'
      else if (substr(tmp[i],1,1) == '2') tmp[i] <- '2#'
      else if (substr(tmp[i],1,1) == '3') tmp[i] <- '3#'
      else tmp[i] <- '4#'
    }
  }
  df[,str] <- tmp
  return(df)
}

raw.data <- prod_handle(raw.data,'prod_cd')

# delete the prod_cd count which is less than the given number 
prodHandle <- function(df,str){
  prodCount <- table(df[,str])
  prodCount <- prodCount[prodCount <= 1000]
  tmp <- df[,str]
  tmp[tmp %in% names(prodCount)] <- NA
  df[,str] <- tmp
  return(df)
}

raw.data <- prodHandle(raw.data,'prod_cd')

#plot the bar by the different product.
p <- ggplot(raw.data[which(!(is.na(raw.data[,'prod_cd']) | raw.data[,'prod_cd']=='')),],
            aes(prod_cd[which(!(is.na(prod_cd) | prod_cd==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='产品编号',title='整体催收各产品条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p
#------------------------------------------------------------------
#########1101 prod_cd:
raw.data1 <- raw.data[which(raw.data[,'prod_cd'] == '1101'),]

p <- ggplot(raw.data1, aes(log(as.numeric(sum_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收次数(对数化)',title='1101产品催收次数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1,aes(log(as.numeric(sum_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收次数(对数化)',title='1101产品催收次数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1, aes(log(as.numeric(day_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收天数(对数化)',title='1101产品催收天数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1,aes(log(as.numeric(day_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收天数(对数化)',title='1101产品催收天数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1,aes(house_type))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='1101产品催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1[which(raw.data1[,'max_ovdue_period'] !=0),],
            aes(as.numeric(max_ovdue_period[which(max_ovdue_period!=0)]))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='逾期天数',title='1101产品逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1[which(!(is.na(raw.data1[,'work_life']) | raw.data1[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='1101产品工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1[which(!(is.na(raw.data1[,'edu_degree']) | raw.data1[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='1101产品催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1[which(!(is.na(raw.data1[,'unit_type']) | raw.data1[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='1101产品催收工作单位条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data1[which(!(is.na(raw.data1[,'bor_purp']) | raw.data1[,'bor_purp']=='')),],
            aes(bor_purp[which(!(is.na(bor_purp) | bor_purp==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='贷款用途',title='1101产品催收贷款用途条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

####------------------------------------------------------------------------
#########3104 prod_cd:
raw.data2 <- raw.data[which(raw.data[,'prod_cd'] == '3104'),]

p <- ggplot(raw.data2, aes(log(as.numeric(sum_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收次数(对数化)',title='3104产品催收次数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2,aes(log(as.numeric(sum_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收次数(对数化)',title='3104产品催收次数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2, aes(log(as.numeric(day_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收天数(对数化)',title='3104产品催收天数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2,aes(log(as.numeric(day_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收天数(对数化)',title='3104产品催收天数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2,aes(house_type))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='3104产品催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2[which(raw.data2[,'max_ovdue_period'] !=0),],
            aes(as.numeric(max_ovdue_period[which(max_ovdue_period!=0)]))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='逾期天数',title='3104产品逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2[which(!(is.na(raw.data2[,'work_life']) | raw.data2[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='3104产品工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2[which(!(is.na(raw.data2[,'edu_degree']) | raw.data2[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='3104产品催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2[which(!(is.na(raw.data2[,'unit_type']) | raw.data2[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='3104产品催收工作单位条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data2[which(!(is.na(raw.data2[,'bor_purp']) | raw.data2[,'bor_purp']=='')),],
            aes(bor_purp[which(!(is.na(bor_purp) | bor_purp==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='贷款用途',title='3104产品催收贷款用途条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p
####-----------------------------------------------------------------------
#########3301 prod_cd:
raw.data3 <- raw.data[which(raw.data[,'prod_cd'] == '3301'),]

p <- ggplot(raw.data3, aes(log(as.numeric(sum_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收次数(对数化)',title='3301产品催收次数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3,aes(log(as.numeric(sum_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收次数(对数化)',title='3301产品催收次数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3, aes(log(as.numeric(day_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收天数(对数化)',title='3301产品催收天数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3,aes(log(as.numeric(day_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收天数(对数化)',title='3301产品催收天数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3,aes(house_type))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='3301产品催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3[which(raw.data3[,'max_ovdue_period'] !=0),],
            aes(as.numeric(max_ovdue_period[which(max_ovdue_period!=0)]))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='逾期天数',title='3301产品逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3[which(!(is.na(raw.data3[,'work_life']) | raw.data3[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='3301产品工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3[which(!(is.na(raw.data3[,'edu_degree']) | raw.data3[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='3301产品催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3[which(!(is.na(raw.data3[,'unit_type']) | raw.data3[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='3301产品催收工作单位条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data3[which(!(is.na(raw.data3[,'bor_purp']) | raw.data3[,'bor_purp']=='')),],
            aes(bor_purp[which(!(is.na(bor_purp) | bor_purp==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='贷款用途',title='3301产品催收贷款用途条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p
####---------------------------------------------------------------------
#########3302 prod_cd:
raw.data4 <- raw.data[which(raw.data[,'prod_cd'] == '3302'),]

p <- ggplot(raw.data4, aes(log(as.numeric(sum_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收次数(对数化)',title='3302产品催收次数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4,aes(log(as.numeric(sum_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收次数(对数化)',title='3302产品催收次数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4, aes(log(as.numeric(day_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收天数(对数化)',title='3302产品催收天数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4,aes(log(as.numeric(day_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收天数(对数化)',title='3302产品催收天数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4,aes(house_type))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='3302产品催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4[which(raw.data4[,'max_ovdue_period'] !=0),],
            aes(as.numeric(max_ovdue_period[which(max_ovdue_period!=0)]))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='逾期天数',title='3302产品逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4[which(!(is.na(raw.data4[,'work_life']) | raw.data4[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='3302产品工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4[which(!(is.na(raw.data4[,'edu_degree']) | raw.data4[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='3302产品催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4[which(!(is.na(raw.data4[,'unit_type']) | raw.data4[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='3302产品催收工作单位条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data4[which(!(is.na(raw.data4[,'bor_purp']) | raw.data4[,'bor_purp']=='')),],
            aes(bor_purp[which(!(is.na(bor_purp) | bor_purp==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='贷款用途',title='3302产品催收贷款用途条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p
