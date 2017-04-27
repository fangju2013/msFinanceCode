gen_df_code <- function(breaks,time_limit,moment){
  # "data.frame(df,sms_length_0_10=dummy[,1],sms_length_10_20=dummy[,2],
  # sms_length_20_50=dummy[,3],sms_length_50_70=dummy[,4],sms_length_70_Inf=dummy[,5])"
  
  code_end='data.frame(df'    #"data.frame(df,"
  var_time <- paste0("_",as.character(time_limit),'_',as.character(moment))
  for(i in (1:(length(breaks)-1))){
    var_name=paste0('sms_length','_',as.character(breaks[i]),'_',as.character(breaks[i+1]),
                    var_time)
    left=paste0(var_name) #sms_length_0_10
    right=paste0('dummy[,',as.character(i),']') #dummy[,1]
    code_t=paste0(left,'=',right)
    code_end=paste0(code_end,',',code_t)
  }
  code_end=paste0(code_end,')')
  return(code_end)
}

gen_summarise_code <- function(breaks,time_limit,moment){
  code_end='dplyr::summarise(df_group'
  var_time <- paste0("_",as.character(time_limit),'_',as.character(moment))
  for(i in (1:(length(breaks)-1))){
    var_name=paste0('sms_length','_',as.character(breaks[i]),'_',
                    as.character(breaks[i+1]),var_time)
    left=paste0(var_name,'_num')
    right=paste0('sum(',var_name,')')
    code_t=paste0(left,'=',right)
    code_end=paste0(code_end,',',code_t)
  }
  code_end=paste0(code_end,')')
  return(code_end)
}

gen_breaks <- function(data,breaks){
  if(length(data[,1])==0){
    df <- data.frame(appl_no='999999')
    content_length <- 0
  }else{
    df <- data.frame(appl_no=data$appl_no)
    content_length <- ifelse(is.na(data$content),0,nchar(data$content))
  }
  df$content_length <- content_length
  breaks_result=cut(content_length,breaks = breaks,include.lowest = T)
  df$breaks <- breaks_result
  return(df)
}

gen_df_entropy <- function(df,breaks,time_limit,moment){
  library(dplyr)
  df_group<- group_by(df,appl_no)
  var_time <- paste0("_",as.character(time_limit),'_',as.character(moment))
  entropy_name <- paste0('sms_length_entropy_',as.character(length(breaks)),var_time)
  summarise_code <- paste0('dplyr::summarise(df_group,',entropy_name,"=entropy(
                           table(breaks)))")
  df_entropy <- eval(parse(text = summarise_code))
  df_entropy <- data.frame(df_entropy)
  return(df_entropy)
}


sms_length_breaks_gen_df <- function(data,breaks,time_limit,moment){
  df <- gen_breaks(data,breaks = breaks)
  library(nnet)
  breaks_result <- df$breaks
  dummy <- class.ind(breaks_result)
  df_code=gen_df_code(breaks = breaks,
                      time_limit = time_limit,
                      moment = moment)
  df_dummy=eval(parse(text = df_code))
  library(dplyr)
  df_group<- group_by(df_dummy,appl_no)
  summarise_code=gen_summarise_code(breaks = breaks,
                                    time_limit = time_limit,
                                    moment = moment)
  df_break_num=eval(parse(text = summarise_code))
  df_break_num=data.frame(df_break_num)
  library(entropy)
  df_entropy <- gen_df_entropy(df = df,
                               breaks = breaks,
                               time_limit = time_limit,
                               moment = moment)
  df_end <- merge(df_break_num,df_entropy,by.x='appl_no')
  return(df_end)
}

sms_length_breaks_time_gen_df <- function(data,breaks,time_limit='all',moment='all'){
  if(time_limit!='all'){data=data[data$datediff<as.numeric(time_limit),]}
  if(moment!='all'){data=data[data$sms_moment==moment,]}
  sms_length_breaks_var <- sms_length_breaks_gen_df(data,
                                                    breaks = breaks,
                                                    time_limit = time_limit,
                                                    moment = moment)
  return(sms_length_breaks_var)
}


f_eval=function(x){eval(parse(text = x))}

gen_merge_df <- function(data,breaks,times,moments){
  code <- list()
  library(parallel)
  n=1
  for (i in 1:length(times)){
    for(j in 1:length(moments)){
      for (k in 1:length(breaks)){
        sms_length_breaks_time_gen_df_code=sprintf("sms_length_breaks_time_gen_df(data = 
                                                   data,breaks=breaks[[%d]],time_limit = 
                                                   times[%d],moment=moments[%d])",k,i,j)
        code[[n]]=sms_length_breaks_time_gen_df_code
        n=n+1
      }
    }
  }
  result_l <- mclapply(code,f_eval,mc.cores = 8)
  result=Reduce(cbind,result_l)
  result=result[,!duplicated(names(result))]#还可以改进
  result[is.na(result)]=0
  return(result)
}

sms_length_breaks_res <- function(data){
  times <<-  c('all','14','30','90','180','270')
  moments <<- c('all','midnight','deepnight','morning','am','noon','pm','night')
  breaks <<- list(b1=c(0,30,60,100,150,Inf),
                 b2=c(0,10,20,30,40,50,60,70,80,90,100,130,150,Inf),
                 b3=c(0,20,40,60,80,100,Inf),
                 b4=c(0,5,10,15,20,40,70,Inf))
  result <- gen_merge_df(data = data,breaks = breaks,times = times,moments = moments)
  return(result)
}
