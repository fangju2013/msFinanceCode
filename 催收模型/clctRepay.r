library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select distinct contra_no,cust_id from data_basic.zpr_contra5_0309 
                     where contra_no is not NULL and rpy_num is not NULL",as.is=T)
odbcClose(ch)

# get the target data and the raw data 
targetData <- X5103_clct_record %>% as.data.frame()
resultData <- left_join(targetData,raw.data,by=c('contra_no'))

result <- c(sum(!is.na(resultData[,1])),sum(!is.na(resultData[,3])),
            round(sum(!is.na(resultData[,3]))/sum(!is.na(resultData[,1])),4))

names(result) <- c('催收人数','已还款人数','还款比例')
result <- data.frame(t(result))
write.csv(result,file = 'clctResult.csv',fileEncoding = 'gbk',row.names = F)
