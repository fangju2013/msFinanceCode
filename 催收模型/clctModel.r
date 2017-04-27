library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
data <- sqlQuery(ch,"select* from data_basic.fj_clctallinfo",stringsAsFactors = F,as.is = T)
odbcClose(ch)

#data preprocess 
library(dplyr)
analysisData <- data
#map the data type of the each variable 
varsDelete <- c("fst_ovdue_date","is_ovdue","last_op_date","out_clct_dt")
varsID <- c("cust_id","cust_cd","contra_no","appl_no")
varsCat <- c("belong_city","house_type","edu_degree","unit_type","bor_purp","clct_time_promisepay",
             "prod_cd")
varsCon <- c("sum_num","day_num","total_accts","last_ovdue_period","max_ovdue_period","work_life",
             "ovdue_prin","curt_bal","bor_amt","loan_term","paid_instal","total_instal",
             "total_paid_amt","total_instal_bal","cur_ovdue_instal","ovdue_inst","ovdue_penalty",
             "ovdue_tms","accum_ovdue_tms","total_ovdue_amt")
target <- c("ovdue_period")

#define the target variable 
Flag <- if_else(analysisData[,target] > 30,1,0)
analysisData$Flag <- Flag %>% as.numeric()
#delete the useless variable 
analysisData <- select(analysisData,-c(fst_ovdue_date,is_ovdue,last_op_date,out_clct_dt,
                                       ovdue_period,cust_id,cust_cd,contra_no,appl_no,
                                       max_ovdue_period,total_ovdue_amt,ovdue_prin))
#find the strange sign
strange.list <- find_strange_sign(analysisData,10)[1]
#set those strange values to NA as a missing value
analysisData <- stran_sign_handle(analysisData,strange.list)
#transfrom the data tyepes 
varsChr <- names(analysisData)[names(analysisData) %in% varsCat]
analysisData[,varsChr] <- lapply(analysisData[,varsChr],as.factor)
varsNum <- names(analysisData)[names(analysisData) %in% varsCon]
analysisData[,varsNum] <- lapply(analysisData[,varsNum],as.numeric)
#add flag of NA and NULL
analysisData <- addFlagNaFunction(analysisData,target='Flag')
#handle the missing values
analysisData <- miss_handle(analysisData,targ = 'Flag',dis.val = '#missing',num.val = 9999999)

#dummy the category variables 
library(dummies)
analysisData <- dummy.data.frame(analysisData)

# library(Matrix)
# analysisData <- sparse.model.matrix(Flag~.,analysisData)

#cut the dataframe into train and test parts 
library(xgboost)
trainTestList <-balance_cut(analysisData,targ='Flag')
train <- trainTestList$train
test <- trainTestList$test

tmp <- which(names(train) == 'Flag')
train.mat <- train[,-tmp] %>% as.matrix()
train.l <- train[,'Flag'] %>% as.character() %>% as.numeric()
dtrain <- xgb.DMatrix(data = train.mat,label = train.l)

test.mat <- test[,-tmp] %>% as.matrix()
test.l <- test[,'Flag'] %>% as.character() %>% as.numeric()
dtest <- xgb.DMatrix(data = test.mat,label=test.l)

# train the model using the xgb.cv 
para.cv <- list(objective = 'binary:logistic',
                eta = 0.2,
                max_depth = 1,
                nthread = 8,
                gamma = 0,
                min_child_weight = 1,
                scale_pos_weight =1, #类别不平衡时使用
                subsample = 0.1,
                colsample_bytree = 0.6,
                lambda = 0,
                alpha = 0,
                early_stopping_rounds = 30)
set.seed(123)
bsts.cv <- xgb.cv(booster = 'gbtree',params = para.cv,data = dtrain,verbose = T,nfold = 5,
                  nrounds = 100,eval_metric = 'auc',prediction = T)

#check the result by using the xgb.cv
library(MLmetrics)
ks <- KS_Stat(bsts.cv$pred,train.l)
auc <- AUC(bsts.cv$pred,train.l)

#train the model using the xgb.train 
para.train <- list(eta = 0.2,
                   gamma = 0,
                   max_depth = 1,
                   min_child_weight = 1,
                   scale_pos_weight = 0.4,
                   subsample = 0.1,
                   colsample_bytree = 0.1,
                   lambda = 0,
                   alpha = 0,
                   objective = 'binary:logistic',
                   early_stopping_rounds = 30)

set.seed(123)
bsts.train <- xgb.train(data = dtrain,paras = para.train,verbose = 1,nround = 50,
                        eval_metric = 'auc',watchlist = list('train'=dtrain,'test'=dtest))

xgb.importance(colnames(train.mat),model = bsts.train)
# predict the test data
# preds <- predict(bsts.cv,newdata = dtest)

