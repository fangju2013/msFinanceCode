library(dplyr)
library(parallel)
library(data.table)
library(stringr)

library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select custorm_id,content from crawler.app_sms_hive 
                 where custorm_id is not NULL and length(custorm_id) < 18 limit 1000",
                 stringsAsFactors = F,as.is = T)
# get the analysis data 
analysisData <- unique(raw.data)
userData <- filter(analysisData,custorm_id == '11882000')

# calculate the variables 
 #计算每个人关键词类型的总数目
gen_key_words_type_n <- function(result,key_words_type){
  n_type <- apply(result,1,sum)
  var_name <- paste0('sms_all_',key_words_type)
  df_n_type <- data.frame(n_type)
  names(df_n_type) <- var_name
  return(df_n_type)
}
 
 #计算关键词类型里的每个关键词匹配数目的list代码
gen_key_word_n_code <- function(key_words){
  summarise_code_l <- list()
  code_end <- 'data.frame('
  for(i in 1:length(key_words)){
    var_name <- paste0('sms_n_',key_words[i])
    left <- paste0(var_name)
    right <- paste0("key_word_num(userData$content,'",key_words[i],"')")
    code_t <- paste0(left,'=',right)
    summarise_code <- paste0(code_end,code_t,')')
    summarise_code_l[[i]] <- summarise_code
  }
  return(summarise_code_l)
}


f_eval <- function(x){eval(parse(text = x))}

 #统计每个关键词与每个人收到的短信内容的匹配数目
key_word_num <- function(content,key_word){
  sum(!is.na(str_match(content,key_word)))
}

 #解析gen_key_word_n_code生成的代码，得到最终的结果
sms_n_key_word <- function(data,key_words){
  library(dplyr)
  library(dtplyr)
  userData <- tbl_dt(data)
  key_word_n_code <- gen_key_word_n_code(key_words)
  result_l <- mclapply(key_word_n_code,f_eval,mc.cores = 2)
  result <- Reduce(cbind,result_l)
  return(result)
}

 #某个关键词类型的统计结果
f_sms_n_chuan <- function(data,words_list,key_words_type){
  word_list <- words_list[[key_words_type]]
  code <- sprintf("sms_n_key_word(data,word_list)")
  result_t <- eval(parse(text = code))
  key_words_type_n <- gen_key_words_type_n(result = result_t,key_words_type=key_words_type)
  result <- cbind(result_t,key_words_type_n)
  return(result)
}

# 统计一个人的所有短信内容的撞词结果
stats_key_words_num <- function(data,words_list){
  words_list <- words_list
  custorm_id <- unique(data$custorm_id)
  df <- data.frame(custorm_id = custorm_id)
  key_words_type <- names(words_list)
  for(i in 1:length(key_words_type)){
    tmp <- f_sms_n_chuan(data,words_list,key_words_type[i])
    df <- cbind(df,tmp)
  }
  return(df)
}

t0 <- Sys.time()
userSmsContentDf <- stats_key_words_num(userData,words_list)
t1 <- Sys.time()
print(t1-t0)
