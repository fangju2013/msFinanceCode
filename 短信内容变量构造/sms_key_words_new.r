# get the key words

key_words <- c("金融","违约","银行","贷款","信用卡","消费","额度","申请","办卡","支付","还清",
               "余额","还款额","还款金额","账单","借记卡","贷记卡","不足","还款","还钱","不还")

bank_words_temp <- c("国家开发银行","中国进出口银行","中国农业发展银行",
                     "中国工商银行","中国农业银行","中国银行","中国建设银行",
                     "交通银行","中信银行","中国光大银行","华夏银行","广发银行",
                     "招商银行","兴业银行","民生银行","恒丰银行","浙商银行","渤海银行",
                     "中国邮政储蓄银行","中行","农行","建行","交行","工行")

bank_suffix <- c("银行","农行","交行","信用社")

get_bank_word <- function(company_words,bank_suffix,bank_words){
  for(i in bank_suffix){
    bank_index <- grep(i,company_words)
    bank_words_temp=company_words[bank_index]
    if(length(bank_index)!=0){company_words <- company_words[-bank_index]}
    bank_words=c(bank_words,bank_words_temp)
  }
  return(list(unique(bank_words),company_words))
}

company_words <- unname(unlist(read.table("/home/ju.fang/JD_TB_dataAnalysis/key_words",stringsAsFactors = F)))

r=get_bank_word(company_words = company_words,bank_suffix = bank_suffix,bank_words = bank_words_temp)
bank_words <- r[[1]]
company_words <- r[[2]]


# match the type of the company 
varsTypeFunc <- function(df){
  df <- data.frame(df,stringsAsFactors = F)
  types <- unique(df[,2])
  typeList <- list()
  for(i in 1:length(types)){
    tmp <- which(df[,2] == types[i])
    typeList[[i]] <- df[,1][tmp]
  }
  names(typeList) <- types
  return(typeList)
}

typesList <- varsTypeFunc(companyName)


mainBank <- c('中国工商银行','中国农业银行','中国银行','中国建设银行','农行','建行',
              '工行','中行')
secBank <- c('交通银行','招商银行','中信银行','交行','招行')
otherBank <- append(setdiff(setdiff(words_list[[2]],mainBank),secBank),
                    typesList[['otherBank']])

mainBank <- paste0(mainBank,collapse = '|')
secBank <- paste0(secBank,collapse = '|')
otherBank <- paste0(otherBank,collapse = '|')

mainConsume <- c("国美消费金融","万达消费金融","平安消费金融","北银消费金融",
                 "兴业消费金融","湖北消费金融","苏宁消费金融","海尔消费金融",
                 "中银消费金融","招联消费金融","锦程消费金融","捷信消费金融",
                 "马上消费金融")
otherConsume <- c("京东白条","阿里花呗","百度有钱")

mainConsume <- paste0(mainConsume,collapse = '|')
otherConsume <- paste0(otherConsume,collapse = '|')

Pay <- typesList[['pay']]
Loan <- typesList[['loan']]
Operator <- typesList[['operator']]
Social <- typesList[['social']]
Travel <- typesList[[6]]
Employ <- typesList[[7]]
Electronic <- typesList[[8]]
Grouppurchase <- typesList[[9]]
Search <- typesList[[10]]
Median <- typesList[[11]]
Delivery <- typesList[[12]]
Video <- typesList[[13]]
Tour <- typesList[[14]]
Insurance <- typesList[[15]]
Bond <- typesList[[16]]
Lovemerry <- typesList[[17]]

Pay <- paste0(Pay,collapse = '|')
Loan <- paste0(Loan,collapse = '|')
Operator <- paste0(Operator,collapse = '|')
Social <- paste0(Social,collapse = '|')
Travel <- paste0(Travel,collapse = '|')
Employ <- paste0(Employ,collapse = '|')
Electronic <- paste0(Electronic,collapse = '|')
Grouppurchase <- paste0(Grouppurchase,collapse = '|')
Search <- paste0(Search,collapse = '|')
Median <- paste0(Median,collapse = '|')
Delivery <- paste0(Delivery,collapse = '|')
Video <- paste0(Video,collapse = '|')
Tour <- paste0(Tour,collapse = '|')
Insurance <- paste0(Insurance,collapse = '|')
Bond <- paste0(Bond,collapse = '|')
Lovemerry <- paste0(Lovemerry,collapse = '|')

words_list <- list(key_words=key_words,mainBank=mainBank,secBank=secBank,
                   otherBank=otherBank,mainConsume=mainConsume,otherConsume=otherConsume,
                   Pay=Pay,Loan=Loan,Operator=Operator,Social=Social,Travel=Travel,
                   Employ=Employ,Electronic=Electronic,Grouppurchase=Grouppurchase,
                   Search=Search,Median=Median,Delivery=Delivery,Video=Video,Tour=Tour,
                   Insurance=Insurance,Bond=Bond,Lovemerry=Lovemerry)

tmp <- names(words_list)[1]
listTmp <- sapply(words_list[[1]],list)
names(listTmp) <- paste0(tmp,'_',names(listTmp))
words_list <- append(listTmp,words_list[2:length(words_list)])
