library(dplyr)
library(parallel)
library(data.table)
library(stringr)

library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select cust_id,contra_no,loan_pmt_due_dt,last_pmt_due_dt,last_rpy_dt 
                    from dsst.fdl_acct_det_chain where loan_pmt_due_dt >= '2017-01-01' 
                    and loan_pmt_due_dt <= '2017-04-06' and contra_no is not null", 
                     stringsAsFactors = F,as.is = T)
odbcClose(ch)

#get the analysis data 
analysisData <- raw.data
analysisData <- analysisData[!duplicated(analysisData),]

#get the variables and the target 
repayVarFunc <- function(vec){
  if(sum(!is.na(vec))>0){
    installCount <- length(vec)
    repayTimes <- sum(!is.na(vec))
    repayIntime <- sum(if_else(vec<=0 & vec>-30,1,0),na.rm = T)
    
    repayOverdue <- sum(if_else(vec>0 & vec<=15,1,0),na.rm = T)
    repayOverdue15 <- sum(if_else(vec>15 & vec<=30,1,0),na.rm = T)
    repayOverdue30 <- sum(if_else(vec>30 & vec<=-30,1,0),na.rm = T)
    
    if(sum(vec[vec<=0 & vec>-30],na.rm = T) != 0){
      tmp <- vec[vec<=0 & vec>-30]
      repayIntimeMax <- max(tmp , na.rm = T)
      repayIntimeMin <- min(tmp , na.rm = T)
      repayIntimeMean <- mean(tmp , na.rm = T)
    }else{
      repayIntimeMax <- -9999
      repayIntimeMin <- -9999
      repayIntimeMean <- -9999
    }
    
    if(sum(vec[vec>0 & vec <= -30],na.rm = T) != 0){
      tmp <- vec[vec>0 & vec <= -30]
      repayOverdueMax <- max(tmp,na.rm = T)
      repayOverdueMin <- min(tmp,na.rm = T)
      repayOverdueMean <- mean(tmp,na.rm = T)
    }else{
      repayOverdueMax <- -9999
      repayOverdueMin <- -9999
      repayOverdueMean <- -9999
    }
    
    repayMean <- mean(vec,na.rm = T)
    
    if(sum(!is.na(vec))>1){
      repayVar <- var(vec,na.rm = T)
    }else{
      repayVar <- -9999
    }
    
    repayResult <- cbind(installCount,repayTimes,repayIntime,repayOverdue,
                         repayOverdue15,repayOverdue30,repayIntimeMax,repayIntimeMin,
                         repayIntimeMean,repayOverdueMax,repayOverdueMin,repayOverdueMean,
                         repayMean,repayVar)
    varNames <<- colnames(repayResult)
    result <- paste(repayResult,collapse = '#')
    return(result)
  }else{
    repayResult <- c(rep(-9999,length(varNames)))
    result <- paste(repayResult,collapse = '#')
    return(result)
  }
}


getVarTarg <- function(loan_pmt_dt,last_pmt_dt,last_rpy_dt){
  loan_pmt_dt <- as.Date(loan_pmt_dt)
  last_pmt_dt <- as.Date(last_pmt_dt)
  last_rpy_dt <- as.Date(last_rpy_dt)
  tmp <- which.max(loan_pmt_dt)
  targ <- as.numeric(last_rpy_dt[tmp] - last_pmt_dt[tmp])
  if(sum(!is.na(as.numeric(last_rpy_dt[-tmp] - last_pmt_dt[-tmp])))>0){
    Var <- as.numeric(last_rpy_dt[-tmp] - last_pmt_dt[-tmp]) 
  }else{
    Var <- NA
  }
  vars <- repayVarFunc(Var)
  result <- paste(targ,vars,sep = '#')
  return(result)
}

#get the variable result 
g.b <- group_by(analysisData,contra_no)
varDf <- summarise(g.b,value = getVarTarg(loan_pmt_due_dt,last_pmt_due_dt,last_rpy_dt))
varDf <- data.frame(varDf,stringsAsFactors = F)
tmpDf <- do.call(rbind,str_split(varDf$value,'#'))
colnames(tmpDf) <- c('targ',varNames)
tmpDf <- data.frame(tmpDf,stringsAsFactors = F)
repayResult <- cbind(varDf[1],tmpDf)
repayResult <- repayResult[!is.na(as.numeric(repayResult$targ)),]


#save the result data to the type of Rdata
save(repayResult,file = 'repayResult.Rdata')
write.csv(repayResult,file='repayResult.csv',fileEncoding = 'gbk',row.names = F)


