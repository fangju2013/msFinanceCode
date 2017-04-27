sms_length_gen_var <- function(sms_length){
  if(length(sms_length)==0||is.na(sms_length)){sms_length=0}
  library(moments)
  sms_length_n=sum(!(sms_length==0),na.rm = T)
  sms_length_min=min(sms_length,na.rm =T)
  sms_length_max=max(sms_length,na.rm =T)
  sms_length_mean=round(mean(sms_length,na.rm =T),2)
  sms_length_var=round(var(sms_length,na.rm =T),2)
  sms_length_0.25=round(as.numeric(quantile(sms_length,0.25,na.rm =T)),2)
  sms_length_0.5=round(as.numeric(quantile(sms_length,0.5,na.rm =T)),2)
  sms_length_0.75=round(as.numeric(quantile(sms_length,0.75,na.rm =T)),2)
  sms_length_skewness=round(skewness(sms_length,na.rm =T),2)
  sms_length_kurtosis=round(kurtosis(sms_length,na.rm =T),2)

  var_result=cbind(sms_length_n,sms_length_min,sms_length_max,
                   sms_length_mean,sms_length_var,
                   sms_length_0.25,sms_length_0.5,sms_length_0.75,
                   sms_length_skewness,sms_length_kurtosis)
  return(var_result)
}


sms_length_gen_df <- function(data,time_limit='all',moment='all'){
  appl_no <- unique(data$appl_no)
  n <- length(appl_no)
  sms_var_df=data.frame()
  for(i in 1:length(appl_no)){
    if(i%%100==0){
      t1=Sys.time()
      jd=sprintf("当前time_limit:%s,当前moment:%s,共%d个用户,现在计算第%d个用户,
                 已经计算了%s秒",time_limit,moment,n,i,t1-t0)
      print(jd)
    }
    user_data=data[data$appl_no==appl_no[i],]
    
    if(time_limit!='all'){user_data=user_data[user_data$datediff<as.numeric(time_limit),]}
    if(moment!='all'){user_data=user_data[user_data$sms_moment==moment,]}
    
    #计算短信长度
    sms_length=ifelse(is.na(user_data$content)|is.null(user_data$content),0,
                      nchar(user_data$content))
    
    sms_length_var = sms_length_gen_var(sms_length)
    sms_var_df=rbind(sms_var_df,sms_length_var)
  }
  var_names=names(sms_var_df)
  var_names_new=paste(var_names,time_limit,moment,sep = '_')
  names(sms_var_df)=var_names_new
  return(sms_var_df)
}


sms_length_gen_res <- function(data){
  t0 <<- Sys.time()
  appl_no <- unique(data$appl_no)
  df=data.frame(appl_no)
  times=c('all','14','30','90','180','270')
  moments=c('all','midnight','deepnight','morning','am','noon','pm','night')
  l=list()
  l[[1]]=df
  n=2
  print("正在计算每个用户短信长度变量")
  for (i in 1:length(times)){
    for(k in 1:length(moments)){
      r_0=sms_length_gen_df(data = data,time_limit = times[i],moment=moments[k])
      l[[n]]=r_0
      n=n+1
    }
  }
  print("合并计算结果")
  result <- Reduce(cbind,l) 
  result[is.na(result)]=0
  return(result)
}



