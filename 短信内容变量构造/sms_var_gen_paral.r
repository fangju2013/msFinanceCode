#关键字匹配
gen_word_entropy <- function(result,key_words_type,time_limit){
  entropy <- apply(result,1,entropy)
  entropy[is.na(entropy)]=0
  var_time <- paste0('_',time_limit)
  var_name <- paste0('sms_key_word_entropy_',key_words_type,var_time)
  df_entropy <- data.frame(entropy)
  names(df_entropy)=var_name
  return(df_entropy)
}

gen_key_words_type_n <- function(result,key_words_type,time_limit){
  n_type <- entropy <- apply(result,1,sum)
  var_time <- paste0('_',time_limit)
  var_name <- paste0('sms_all_',key_words_type,var_time)
  df_n_type <- data.frame(n_type)
  names(df_n_type)=var_name
  return(df_n_type)
}




gen_key_word_n_code <- function(key_words,time_limit){
  summarise_code_l <- list()
  code_end <- 'data.frame('
  var_time <- paste0('_',time_limit)
  for(i in 1:length(key_words)){
    var_name <- paste0('sms_n_',key_words[i],var_time)
    left=paste0(var_name)
    right=paste0('key_word_num(data$content,\'',key_words[i],'\')')
    code_t=paste0(left,'=',right)
    summarise_code=paste0(code_end,code_t,')')
    summarise_code_l[[i]]=summarise_code
  }
  return(summarise_code_l)
}

f_eval=function(x){eval(parse(text = x))}

key_word_num <- function(content,key_word){sum(!is.na(unlist(str_match_all(content,
                                                                           key_word))))}

sms_n_key_word <- function(data,time_limit="all",key_words){
  if(time_limit!='all'){data=data[data$datediff<as.numeric(time_limit),]}
  library(dplyr)
  library(dtplyr)
  # data <- tbl_dt(data)
  key_word_n_code=gen_key_word_n_code(key_words = key_words,time_limit = time_limit)
  result_l <- mclapply(key_word_n_code,f_eval,mc.cores = 2)
  result=Reduce(cbind,result_l)
  return(result)
}


f_sms_n_chuan <- function(data,words_list,key_words_type,times11){
  n_times <- length(times11)
  result <- list()
  n=1
  word_list <- words_list[[key_words_type]]
  for(j in 1:n_times){
    code <- sprintf("sms_n_key_word(data,times11[%d],word_list)",j)
    result_t <- eval(parse(text = code))
    word_entropy <- gen_word_entropy(result = result_t,key_words_type = key_words_type,
                                     time_limit = times11[j])
    key_words_type_n <- gen_key_words_type_n(result = result_t,key_words_type = 
                                               key_words_type,time_limit = times11[j])
    result[[n]] <- cbind(result_t,word_entropy,key_words_type_n)
    n=n+1
  }
  result <- Reduce(cbind,result)
  return(result)
}  

eval_func_func <- function(code_list,i){
  result_l <- mclapply(code_list,f_eval,mc.cores = 2)
  result <- Reduce(cbind,result_l)
  word_entropy <- gen_word_entropy(result = result,key_words_type = key_words_type,
                                   time_limit = times11[i])
  key_words_type_n <- gen_key_words_type_n(result = result,key_words_type = key_words_type,
                                           time_limit = times11[i])
  result <- cbind(result,word_entropy,key_words_type_n)
  result_final[[i]] <- result
}


gen_bing_final_code <- function(times11,words_list,breaks,key_words_type){
  n_times <- length(times11)
  result_final <<- list()
  code_list_list <<- list()
  code_final <- list()
  for (i in 1:n_times) {
    time_limit <- times11[i]
    n=1
    code_list <- list()
    word_list <<- list()
    for(j in 1:breaks_n){
      word_list[[j]] <<- words_list[[key_words_type]][breaks %in% levels(breaks)[j]]
      code_list[[n]] <- sprintf("sms_n_key_word(data,times11[%d],word_list[[%d]])",i,j)
      n=n+1
    }
    code_list_list[[i]] <<- code_list
    code_final[[i]] <- sprintf('eval_func_func(code_list = code_list_list[[%d]],i = %d)',i,i)
    
  }
  return(code_final)
}


f_sms_n_bing <- function(code_final){
  result_l <- mclapply(code_final,f_eval,mc.cores = 8)
  result <- Reduce(cbind,result_l)
  return(result)
} 


chuan_run <- function(code_list){
  result_l <- mclapply(code_list,f_eval,mc.cores = 1)
  result <- Reduce(cbind,result_l)
}

bing_run <- function(code_list){
  result_l <- mclapply(code_list,f_eval,mc.cores = 1)
  result <- Reduce(cbind,result_l)
}


sms_key_word_run_code <- function(data,words_list,times11){
  #############################################
  ####1，产生串行和并行的code
  #############################################
  code_chuan <<- list()
  code_bing <<- list()
  n_times <- length(times11)
  for (key_words_type in names(words_list)) {
    n_word <- length(words_list[[key_words_type]])
    words_threshold <- 200
    if (n_word<words_threshold) {
      code_chuan[[key_words_type]] <<- sprintf("f_sms_n_chuan(data = data,words_list = 
                                               words_list,key_words_type = '%s',times11 = 
                                               times11)",key_words_type)
    }else{
      key_words_type <<- key_words_type
      breaks_n <<- floor(n_word/words_threshold)
      breaks <- cut(1:n_word,breaks = breaks_n)
      code_final <<- gen_bing_final_code(times11,words_list,breaks,key_words_type)
      code_bing[[key_words_type]] <<- sprintf("f_sms_n_bing(code_final)")
    }
  }
  code_run_list <- list()
  
  code_run_list[[1]] <- 'chuan_run(code_chuan)'
  code_run_list[[2]] <- 'bing_run(code_bing)'
  return(code_run_list)
}

sms_key_word_run <- function(code_run_list){
  #############################################
  #2，执行code
  ############################################
  result_l <- mclapply(code_run_list,f_eval,mc.cores = 2)
  result <- Reduce(cbind,result_l)
  result <- result[,-duplicated(names(result))]
  return(result)
}

sms_key_word_res <- function(data){
  df <- data.frame(appl_no=unique(data$appl_no))
  times11 <<- c('all','14','30','90','180','270')
  n_times <<- length(times11)
  words_list <<- words_list
  code_run_list <<- sms_key_word_run_code(data,words_list,times11)
  result <- sms_key_word_run(code_run_list)
  result <- cbind(df,result)
  return(result)
}
