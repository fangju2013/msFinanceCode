library(dplyr)
library(parallel)
library(data.table)
library(stringr)
library(xgboost)
library(MLmetrics)

library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select cust_id,loan_pmt_due_dt,last_pmt_due_dt,last_rpy_dt from 
                     dsst.fdl_acct_det_chain where loan_pmt_due_dt >= '2017-02-01' 
                     and loan_pmt_due_dt <= '2017-03-15'", stringsAsFactors = F,as.is =T)
odbcClose(ch)

#check the data types and transform the data types if necessary
analysisData <- raw.data[!duplicated(raw.data),]

#get the max loan_pmt_due_dt of every user
maxDtGp <- group_by(analysisData,cust_id)
maxDtDf <- summarise(maxDtGp,maxDt = max(loan_pmt_due_dt))
maxDtDf <- data.frame(maxDtDf,stringsAsFactors = F)

# save the max loan_pmt_due_dt data to the type of Rdata
save(maxDtDf,file = 'maxDtDf.Rdata')

#get the result data  
g.b <- group_by(analysisData,cust_id)
varDf <- summarise(g.b,value = getVarTarg(loan_pmt_due_dt,last_pmt_due_dt,last_rpy_dt))
varDf <- data.frame(varDf,stringsAsFactors = F)
tmpDf <- do.call(rbind,str_split(varDf$value,'/'))
colnames(tmpDf) <- c('targ','value')
tmpDf <- data.frame(tmpDf,stringsAsFactors = F)
tmpdf <- do.call(rbind,str_split(tmpDf$value,'#'))
colnames(tmpdf) <- varNames
repayResult <- cbind(varDf[1],tmpDf[1],tmpdf)
repayResult <- data.frame(repayResult,stringsAsFactors = F)

# save the result data to the type of Rdata
save(repayResult,file = 'repayResult.Rdata')

# transform the data type and set Inf or NaN to NA 
for(i in names(repayResult)[-1]){
  tmpVar <- as.character(repayResult[,i]) %>% as.numeric
  tmp <- which(is.infinite(tmpVar) | is.nan(tmpVar))
  repayResult[tmp,i] <- NA
}

# join the result data and the max loan_pmt_due_dt data 
repayResult <- left_join(repayResult,maxDtDf,by=c('cust_id'))
for(i in names(repayResult)[2:17]){
  repayResult[,i] <- as.character(repayResult[,i]) %>% as.numeric()
}

# get the train data and the validation data 
repayResult <- repayResult[!is.na(repayResult[,'targ']),]
repayResult <- arrange(repayResult,maxDt)
repayResult <- miss_handle(repayResult,'targ',num.val=-9999)

trainDf <- filter(repayResult,maxDt < '2017-03-01')
targ.train <- if_else(trainDf$targ<=0 & trainDf$targ>=-15,1,0) %>% as.numeric
table(targ.train)

trainX <- trainDf[,-c(1,2,18)] %>% as.matrix
dtrain <- xgb.DMatrix(data = trainX,label = targ.train,missing = -9999)

validationDf <- filter(repayResult,maxDt >= '2017-03-01',maxDt <= '2017-03-03')
targ.validation <- if_else(validationDf$targ<=0&validationDf$targ>=-15,1,0)%>%as.numeric
validationX <- validationDf[,-c(1,2,18)] %>% as.matrix
dvalidation <- xgb.DMatrix(data=validationX,label=targ.validation,missing = -9999)
# set.seed(123)
# trainIdx <- sample(1:nrow(repayResult),1000000,replace = F)
# trainDf <- repayResult[trainIdx,]
# targ.train <- if_else(trainDf$targ<=0 & trainDf$targ>=-10,0,1) %>% as.numeric
# trainX <- trainDf[,-c(1,2)] %>% as.matrix()
# dtrain <- xgb.DMatrix(data = trainX,label = targ.train,missing = -9999)
# 
# set.seed(123)
# validatonResult <- repayResult[-trainIdx,]
# validationIdx <- sample(1:nrow(validatonResult),300000,replace = F)
# validationDf <- validatonResult[validationIdx,]
# targ.validation <- if_else(validationDf$targ<=0&validationDf$targ>=-10,0,1)%>%as.numeric
# validationX <- validationDf[,-c(1,2)] %>% as.matrix
# dvalidation <- xgb.DMatrix(data=validationX,label=targ.validation,missing = -9999)

# train the model by the xgboost model 
para.train <- list(eta = 0.1,
                   gamma = 0,
                   max_depth = 1,
                   min_child_weight = 3,
                   scale_pos_weight = 0.4,
                   subsample = 0.1,
                   colsample_bytree = 0.1,
                   lambda = 0,
                   alpha = 0,
                   objective = 'binary:logistic',
                   early_stopping_rounds = 300)

set.seed(123)
bsts.train <- xgb.train(data = dtrain,paras = para.train,verbose = 1,nround = 100,
          eval_metric = 'auc',watchlist = list('train'=dtrain,'validation'=dvalidation))

xgb.importance(colnames(trainX),model = bsts.train)


# check the model 
getMissRatio <- function(df,targ,miss = NULL){
  samp.l <- nrow(df)
  miss.ratio <- c()
  for(i in names(df)){
    if(i!=targ){
      miss.l <- sum(df[,i] == miss)
      tmp <- round(miss.l/samp.l,2)
      names(tmp) <- i
      miss.ratio <- c(miss.ratio,tmp)
    }
  }
  return(miss.ratio)
}

getMissRatio(trainDf,'targ',miss = -9999)
getMissRatio(validationDf,'targ',miss = -9999)

# train the model by the logistic regression model  
LrDf <- cbind(targ.train,trainX)
LrDf <- data.frame(LrDf,stringsAsFactors = F)
lrResult <- glm(targ.train~.,data=LrDf,family=binomial(link="logit"))



