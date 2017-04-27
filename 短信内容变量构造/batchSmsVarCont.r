library(dplyr)
library(parallel)
library(data.table)
library(stringr)

library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select custorm_id,content from crawler.app_sms_hive where 
            custorm_id is not NULL and length(custorm_id) < 18 and length(content) > 5
            order by rand() limit 1000", stringsAsFactors = F,as.is = T)
odbcClose(ch)
# get the analysis data 
analysisData <- unique(raw.data)


stats_key_words_num <- function(data,words_list){
  words_list <<- words_list
  key_words_type <<- names(words_list)
  data <- group_by(data,custorm_id)
  df <- summarise(data,value = f_sms_n_chuan(content))
  df <- as.data.frame(df,stringsAsFactors = F)
  tmpDf <- do.call(rbind,str_split(df$value,'#'))
  colnames(tmpDf) <- nameResult
  result <- cbind(df[1],tmpDf)
  return(result)
}


f_sms_n_chuan <- function(content){
  key_word_num <- function(content,key_word){
    sum(!is.na(str_match(content,key_word)))
  }
  
  f_eval <- function(x){eval(parse(text = x))}
  
  for (i in 1:length(key_words_type)){
    tmp <- key_words_type[i]
    word_list <- words_list[[tmp]]
      
    summarise_code_l <- list()
    code_end <- 'data.frame('
    for(j in 1:length(word_list)){
      var_name <- paste0('sms_n_',word_list[j])
      left <- paste0(var_name)
      right <- paste0("key_word_num(content,'",word_list[j],"')")
      code_t <- paste0(left,'=',right)
      summarise_code <- paste0(code_end,code_t,')')
      summarise_code_l[[j]] <- summarise_code
    }
    
    result_l <- mclapply(summarise_code_l,f_eval,mc.cores = 2)
    result_t <- Reduce(cbind,result_l)
    key_words_type_n <- gen_key_words_type_n(result = result_t,key_word_type = tmp)
    tmpdf <- cbind(result_t,key_words_type_n)
    if(i==1) 
      result <- tmpdf
    else 
      result <- cbind(result,tmpdf)
  }
  nameResult <<- names(result) 
  result <- paste(result,collapse = '#')
  return(result)
}


gen_key_words_type_n <- function(result,key_word_type){
  n_type <- apply(result,1,sum)
  var_name <- paste0('sms_all_',key_word_type)
  df_n_type <- data.frame(n_type)
  names(df_n_type) <- var_name
  return(df_n_type)
}

#write the data in the local 
smsContentDf <- stats_key_words_num(analysisData,words_list)
write.csv(smsContentDf,file="smsContentDf.csv",fileEncoding = 'gbk',row.names = F)

#write the variable names in the local
smsContentVarName <- names(smsContentDf)
write.csv(smsContentVarName,file="smsContentVarName.csv",fileEncoding='gbk')

