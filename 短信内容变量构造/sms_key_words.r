key_words <- c("金融","违约","银行","贷款","信用卡","消费","额度","申请","办卡","支付","还清",
               "余额","还款额","还款金额","账单","借记卡","贷记卡","不足","还款","还钱","不还")

bank_words_temp <- c("国家开发银行","中国进出口银行","中国农业发展银行",
                     "中国工商银行","中国农业银行","中国银行","中国建设银行",
                     "交通银行","中信银行","中国光大银行","华夏银行","广发银行",
                     "招商银行","兴业银行","民生银行","恒丰银行","浙商银行","渤海银行",
                     "中国邮政储蓄银行","中行","农行","建行","交行","工行")

consume_words <- c("京东白条","阿里花呗","国美消费金融","万达消费金融",
                   "平安消费金融","百度有钱","北银消费金融","兴业消费金融",
                   "湖北消费金融","苏宁消费金融","海尔消费金融","中银消费金融",
                   "招联消费金融","锦程消费金融","捷信消费金融","马上消费金融")

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

words_list <- list(key_words=key_words,bank_words=bank_words,consume_words=consume_words,company_words=company_words)

