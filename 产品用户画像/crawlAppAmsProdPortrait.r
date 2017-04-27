library(dplyr)
library(stringr)
library(RODBC)

#get the data from the impala 
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from data_basic.fj_sms_prod", 
                     as.is=T)

#random choose the sampling by the giving ratio
random_samp <- function(df,targ,ratio){
  df[,targ] <- as.character(df[,targ])
  tmp <- unique(df[,targ])
  prod1.num <- which(df[,targ]==tmp[1])
  prod2.num <- which(df[,targ]==tmp[2])
  prod3.num <- which(df[,targ]==tmp[3])
  prod4.num <- which(df[,targ]==tmp[4])
  prod1.l <- length(prod1.num)*ratio
  prod2.l <- length(prod2.num)*ratio
  prod3.l <- length(prod3.num)*ratio
  prod4.l <- length(prod4.num)*ratio
  prod1.sam <- sample(prod1.num,prod1.l,replace=F)
  prod2.sam <- sample(prod2.num,prod2.l,replace=F)
  prod3.sam <- sample(prod3.num,prod3.l,replace=F)
  prod4.sam <- sample(prod4.num,prod4.l,replace=F)
  all.samples <- c(prod1.sam,prod2.sam,prod3.sam,prod4.sam)
  for(i in 1:5){
    all.samples <- sample(all.samples,length(all.samples),replace=F)
  }
  subDf <- df[all.samples,]
  return(subDf)
}

sub.raw.data <- random_samp(raw.data,'prod_cd',0.1)

#collect the key word which is about the finance 
keyword <- c('消费金融','贷款','申请','支付','欠款','信用','逾期','扣款','欺诈','诈骗','额度',
             '还款','到期','银行卡','账单','分期','金额','资金','授权','提现','付款','账户','投资',
             '外汇','汇率','理财','转账','利率','还清','欠钱')

#statistic the keyword by all the different prod 
tmp.prod <- unique(sub.raw.data[,2])

prod_4108 <- filter(sub.raw.data,prod_cd == tmp.prod[1])
prod_4106 <- filter(sub.raw.data,prod_cd == tmp.prod[2])
prod_5103 <- filter(sub.raw.data,prod_cd == tmp.prod[3])
prod_4103 <- filter(sub.raw.data,prod_cd == tmp.prod[4])
 
 #statistic the length of the vector
word_length <- function(vec){
  vec.length <- sum(!is.na(vec))
  return(vec.length)
}

word_stats <- function(df,keyword){
  keyword.mat <- matrix(0,ncol=length(keyword),nrow=length(df[,1]))
  for(i in 1:length(keyword)){
    keyword.mat[,i] <- str_match(df[,1],keyword[i])
  }
  wordfreq <- apply(keyword.mat,2,word_length)
  statis.prod <- data.frame(keyword=keyword,wordfreq=wordfreq,stringsAsFactors=F)
  return(statis.prod)
}

statis.prod4108 <- word_stats(prod_4108,keyword)
statis.prod4106 <- word_stats(prod_4106,keyword)
statis.prod5103 <- word_stats(prod_5103,keyword)
statis.prod4103 <- word_stats(prod_4103,keyword)

# write the dataframe to the giving file 
write.csv(statis.prod4108,file = '~/statis_prod4108.csv',fileEncoding='gbk',row.names = F)
write.csv(statis.prod4106,file = '~/statis_prod4106.csv',fileEncoding='gbk',row.names = F)
write.csv(statis.prod5103,file = '~/statis_prod5103.csv',fileEncoding='gbk',row.names = F)
write.csv(statis.prod4103,file = '~/statis_prod4103.csv',fileEncoding='gbk',row.names = F)

