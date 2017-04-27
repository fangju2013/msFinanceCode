library(ggplot2)
library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select cust_id,belong_city,house_type,total_accts,max_ovdue_period,
                      work_life,edu_degree,unit_type from dsst.fdl_cust_clctcust_ext_chain 
                      where chain_status = 'active'",as.is=T)
odbcClose(ch)

# get the raw data and the target data 
raw.data <- unique(raw.data)
targetData <- X5103_clct_record
targetData <- targetData %>% as.data.frame()
targetData[,2] <- targetData[,2] %>% as.character()
analysisData <- left_join(targetData,raw.data,by=c('cust_id'))

analysisData[analysisData == ''] <- NA

# plot the picture of the analysisData 
attach(analysisData)
p <- ggplot(analysisData, aes(as.numeric(total_accts))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=20,fill='blue')
p <- p+labs(x='客户总账户数',title='客户总账户数密度图') +
   theme(plot.title = element_text(size = 20,face = 'bold')) +
   theme(strip.text.y = element_text(angle = 30))
p


p <- ggplot(analysisData[!is.na(house_type),],aes(house_type[!is.na(house_type)]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='整体催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

maxOvdue <- max_ovdue_period[analysisData[,'max_ovdue_period']!=0] %>% as.numeric()
maxOvdue <- cut(maxOvdue,breaks = unique(quantile(maxOvdue,probs=seq(0,1,0.1),
                                                  na.rm = T)),include.lowest = T)
p <- ggplot(data = data.frame(x = maxOvdue), mapping = aes(x = factor(x), y = ..count..)) + 
     geom_bar(stat = 'count', fill = 'white', colour = 'darkgreen')
p <- p + labs(x='逾期天数区间',title='整体逾期天数区间直方图',y='频数')+
  theme(plot.title = element_text(size = 30,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(analysisData[analysisData[,'max_ovdue_period'] !=0,],
            aes(as.numeric(max_ovdue_period[max_ovdue_period!=0]))) +
  geom_histogram(fill = 'white', colour = 'darkgreen',position = 'stack',binwidth = 4) 
p <- p + labs(x='逾期天数',title='整体逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 15)) +
  theme(strip.text.y = element_text(angle = 30))
p


workLife <- work_life[analysisData[,'work_life']!=0] %>% as.numeric()
workLife <- cut(workLife,breaks = unique(quantile(workLife,probs=seq(0,1,0.1),
                                                  na.rm = T)),include.lowest = T)
workLife <- workLife[!is.na(workLife)]
p <- ggplot(data = data.frame(x = workLife), mapping = aes(x = factor(x), y = ..count..)) + 
  geom_bar(stat = 'count', fill = 'steelblue', colour = 'darkgreen')
p <- p + labs(x='工作年限区间',title='整体工作年限区间直方图',y='频数')+
  theme(plot.title = element_text(size = 30,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p


p <- ggplot(analysisData[which(!(is.na(analysisData[,'work_life']) | analysisData[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='整体工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(analysisData[which(!(is.na(analysisData[,'edu_degree']) | analysisData[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='整体催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(analysisData[which(!(is.na(analysisData[,'unit_type']) | analysisData[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='整体催收工作单位条形图')+
  theme(plot.title = element_text(size = 10,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

# pie 
dt <- data.frame(table(unit_type))
dt = dt[order(dt$Freq, decreasing = TRUE),]
myLabel = as.vector(dt$unit_type) 
myLabel = paste(myLabel, "(", round(dt$Freq / sum(dt$Freq) * 100, 2), "%)", sep = "") 
p = ggplot(dt, aes(x = "", y = Freq, fill = unit_type)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank(), legend.position = "top") + 
  scale_fill_discrete(breaks = dt$unit_type, labels = myLabel) + 
  theme(axis.text.x = element_blank()) + 
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), x = sum(Freq)/20, label = myLabel), size = 5)
p
#-----------------------------------------------------------------------

p <- ggplot(raw.data[which(!(is.na(raw.data[,'bor_purp']) | raw.data[,'bor_purp']=='')),],
            aes(bor_purp[which(!(is.na(bor_purp) | bor_purp==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='贷款用途',title='整体催收贷款用途条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

