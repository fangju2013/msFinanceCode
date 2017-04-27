library(ggplot2)
library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from data_basic.fj_clctAllInfo",as.is=T)
odbcClose(ch)
attach(raw.data)

p <- ggplot(raw.data, aes(log(as.numeric(sum_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收次数(对数化)',title='整体催收次数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data,aes(log(as.numeric(sum_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收次数(对数化)',title='整体催收次数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data, aes(log(as.numeric(day_num)+1))) + geom_density(colour='blue')
p <- p+geom_histogram(position="identity",alpha=0.3,aes(y=..density..),bins=30,fill='blue')
p <- p+labs(x='催收天数(对数化)',title='整体催收天数密度图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data,aes(log(as.numeric(day_num)+1))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='催收天数(对数化)',title='整体催收天数直方图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data,aes(house_type))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='住房类型',title='整体催收住房条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data[which(raw.data[,'max_ovdue_period'] !=0),],
            aes(as.numeric(max_ovdue_period[which(max_ovdue_period!=0)]))) +
  geom_histogram(bins=30,fill='blue')
p <- p + labs(x='逾期天数',title='整体逾期天数直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data[which(!(is.na(raw.data[,'work_life']) | raw.data[,'work_life']=='')),],
            aes(as.numeric(work_life[which(!(is.na(work_life) | work_life==''))]))) +
  geom_histogram(bins=20,fill='blue')
p <- p + labs(x='工作年限',title='整体工作年限直方图',y='频数')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data[which(!(is.na(raw.data[,'edu_degree']) | raw.data[,'edu_degree']=='')),],
            aes(edu_degree[which(!(is.na(edu_degree) | edu_degree==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='受教育程度',title='整体催收受教育程度条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
  theme(strip.text.y = element_text(angle = 30))
p

p <- ggplot(raw.data[which(!(is.na(raw.data[,'unit_type']) | raw.data[,'unit_type']=='')),],
            aes(unit_type[which(!(is.na(unit_type) | unit_type==''))]))+
  geom_bar(position = "stack",na.rm = T,width = 0.5,fill = 'blue')+
  coord_flip()
p <- p+labs(y='频数',x='工作单位',title='整体催收工作单位条形图')+
  theme(plot.title = element_text(size = 20,face = 'bold')) +
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


