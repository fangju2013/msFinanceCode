library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)

# get the data from the impala
clct.time <- sqlQuery(ch,"select * from data_basic.fj_contra_clct_time ",as.is=T)
clct.time <- unique(clct.time)
repay.time <- sqlQuery(ch,"select * from data_basic.fj_contra_repay_time ",as.is=T)
repay.time <- unique(repay.time)
close(ch)

names(clct.time) <- c('contra_no','time','flag')
names(repay.time) <- c('contra_no','time','flag')

# join the two dataframe 
raw.data <- rbind(clct.time,repay.time)

# sort the dataframe according to the giving colnames
#sort.data <- raw.data[order(raw.data$contra_no,raw.data$time),]
sort.data <- arrange(raw.data,contra_no,time)

row.names(sort.data) <- 1:dim(raw.data)[1]

#diff the giving colname 
contra.diff <- diff(sort.data$contra_no %>% as.numeric())
time.diff <- diff(sort.data$time %>% as.Date() )
flag.diff <- diff(sort.data$flag %>% as.numeric())

# merge the vector to the dataframe
diffDataFrame <- cbind(contra.diff,time.diff,flag.diff) %>% as.data.frame()

#get the dataframe which is according with the giving condition 
idx <- which(diffDataFrame$contra.diff == 0 & diffDataFrame$flag.diff == -1 & 
               diffDataFrame$time.diff >= 0)
newData <- cbind(sort.data[idx,],diffDataFrame[idx,])

# select the colnames which are useful
newData <- select(newData,contra_no,time.diff)

# statistic the max,min,mean and var 
newData.gp <- group_by(newData,contra_no)
statis.data <- summarize(newData.gp,
                         clct.max = max(time.diff),
                         clct.min = min(time.diff),
                         clct.mean = round(mean(time.diff),2),
                         clct.var = round(var(time.diff),2))

#save to the impala 
sqlSave(ch,statis.data,'data_basic.fj_clct_statics_tmp',verbose=T, fast=T, append=F)
