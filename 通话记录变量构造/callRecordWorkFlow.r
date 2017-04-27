library(dplyr)
library(data.table)
library(parallel)
library(stringr)

library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from data_basic.fj_call_record_all limit 1000",
                     stringsAsFactors = F, as.is=T)
odbcClose(ch)
# get the analysis data 
analysisData <- unique(raw.data)

# find the strange signs
strange.list <- find_strange_sign(analysisData) 
 
# handle the strange signs
#no strange signs,so needn't handle them 

# handle the name field 
analysisData <- nameTranFunction(analysisData)

# handle the number field
analysisData <- numberTranFunction(analysisData)

# handle the type field
analysisData <- typeTranFunction(analysisData)

# handle the call_time field
analysisData <- callTimeTranFunction(analysisData)

# add the variables 
analysisData$callTimeType <- eval(parse(text = paste0("paste0(analysisData[,",
                    which(names(analysisData)=='call_time'),"],","analysisData[,",
                    which(names(analysisData)=='type'),"])")))
analysisData$numberType <- eval(parse(text = paste0("paste0(analysisData[,",
                    which(names(analysisData)=='number'),"],","analysisData[,",
                    which(names(analysisData)=='type'),"])")))
analysisData$numberCallTimeType <- eval(parse(text = paste0("paste0(analysisData[,",
                    which(names(analysisData)=='number'),"],","analysisData[,",
                    which(names(analysisData)=='call_time'),"],","analysisData[,",
                    which(names(analysisData)=='type'),"])")))

# calculate the feature 
callRecordVar <- var_calculate(analysisData)

# write the data in the local 
write.csv(callRecordVar,file = '~/callRecordVar.csv',row.names = F,fileEncoding='gbk')

# write the variable names in the local  
callRecordVarName <- names(callRecordVar)
write.csv(callRecordVarName,file = 'callRecordVarName.csv',fileEncoding = 'gbk')
