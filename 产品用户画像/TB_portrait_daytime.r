library(dplyr)
library(fBasics)
require(bitops)
library(RCurl)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from crawler.crawl_taobao_order_detail_hive limit 10000",as.is=T)

#get the data information of the user
id = '3767000' 
UserInfo <- dplyr::filter(raw.data,custorm_id == id,sts_order == '交易成功')

#check the data types
str(UserInfo) 
#find the strange sign
strange.list <- find_strange_sign(UserInfo,10) # no strange sign

#statistic the  frequently used variables by the month
stats_var_month_func <- function(df,time,amt){
  tmptime <- substr(df[,time],1,7)
  gp_df <- data.frame(buy_date = tmptime,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,buy_date)
  gp_stats_df <- summarise(gp_df,
                           q1 = quantile(amts,c(1/4)),
                           q2 = quantile(amts,c(1/2)),
                           q3 = quantile(amts,c(3/4)),
                           skew = skewness(amts),
                           kurt = kurtosis(amts),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T),
                           max = max(amts,na.rm = T),
                           min = min(amts,na.rm = T),
                           sd = sd(amts,na.rm = T))
  stats.df <- as.data.frame(gp_stats_df,stringsAsFactors = F)
  stats.df.name <- names(stats.df)
  stats.df.name <- paste('Month',stats.df.name,sep = '_')
  names(stats.df) <- stats.df.name
  return(stats.df)
}

#statistic the  frequently used variables by the hour
stats_var_hour_func <- function(df,time,amt,id = id){
  tmphour <- substr(as.character(df[,time]),12,13)
  tmphour <- cut(tmphour%>%as.numeric(),breaks = c(0,7,13,15,19,24),
              labels = c('Midnight','Morning','Noon','Afternoon','Evening'),
              right = FALSE)
  tmphour <- tmphour%>%as.character()
  gp_df <- data.frame(buyHour = tmphour,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,buyHour)
  gp_stats_df <- summarise(gp_df,
                           freq = n(),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T))
  tmp <- data.frame(gp_stats_df,stringsAsFactors = F)
  tmpName <- c()
  for (i in as.character(tmp[,1])){
    for (j in names(tmp)[-1]){
      tmpName <- c(tmpName,paste(i,j,sep = '_'))
    }
  }
  tmpName <- c('custorm_id',tmpName)
  tmp <- as.matrix(tmp[,-1])
  tmp <- as.vector(t(tmp))
  tmp <- c(id,tmp)
  stats.df <- data.frame()
  stats.df <- rbind(stats.df,tmp)
  names(stats.df) <- tmpName
  return(stats.df)
}

stats_var_hour_func(UserInfo,'time_buy','amt_order',id = id)

#statistic the frequently used variables by the weekdays
stats_var_week_func <- function(df,time,amt,id = id){
  tmptime <- as.character(df[,time]) %>% as.Date()
  weekday <- weekdays(tmptime)
  weekday <- ifelse(weekday %in% c('Saturday','Sunday'),'NWD','WD')
  gp_df <- data.frame(week = weekday,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,week)
  gp_stats_df <- summarise(gp_df,
                           freq = n(),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T))
  tmp <- data.frame(gp_stats_df,stringsAsFactors = F)
  tmpName <- c()
  for (i in as.character(tmp[,1])){
    for (j in names(tmp)[-1]){
      tmpName <- c(tmpName,paste(i,j,sep = '_'))
    }
  }
  tmpName <- c('custorm_id',tmpName)
  tmp <- as.matrix(tmp[,-1])
  tmp <- as.vector(t(tmp))
  tmp <- c(id,tmp)
  stats.df <- data.frame()
  stats.df <- rbind(stats.df,tmp)
  names(stats.df) <- tmpName
  return(stats.df)
}

stats_var_week_func(UserInfo,'time_buy','amt_order',id = id)

#statistic the frequently used variables by the weekdays and the hour
stats_var_hourWeek_func <- function(df,time,amt,id = id){
  timetmp <- df[,time]
  timptmp <- timetmp %>% as.character()
  tmphour <- substr(timptmp,12,13)
  tmphour <- cut(tmphour%>%as.numeric(),breaks = c(0,7,13,15,19,24),
                 labels = c('Midnight','Morning','Noon','Afternoon','Evening'),
                 right = FALSE)
  tmphour <- tmphour%>%as.character()
  tmptime <- timetmp %>% as.Date()
  weekday <- weekdays(tmptime)
  weekday <- ifelse(weekday %in% c('Saturday','Sunday'),'NWD','WD')
  hourWeek <- paste0(tmphour,weekday)
  gp_df <- data.frame(hourWeek = hourWeek,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,hourWeek)
  gp_stats_df <- summarise(gp_df,
                           freq = n(),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T))
  tmp <- data.frame(gp_stats_df,stringsAsFactors = F)
  tmpName <- c()
  for (i in as.character(tmp[,1])){
    for (j in names(tmp)[-1]){
      tmpName <- c(tmpName,paste(i,j,sep = '_'))
    }
  }
  tmpName <- c('custorm_id',tmpName)
  tmp <- as.matrix(tmp[,-1])
  tmp <- as.vector(t(tmp))
  tmp <- c(id,tmp)
  stats.df <- data.frame()
  stats.df <- rbind(stats.df,tmp)
  names(stats.df) <- tmpName
  return(stats.df)
}

stats_var_hourWeek_func(UserInfo,'time_buy','amt_order',id = id)

#get the holiday by the giving date from the api of "http://www.easybots.cn/api/holiday.php?d="
is.holiday <- function(x){ #x is a string, e.g: x = "20130101"
  url.x = paste("http://www.easybots.cn/api/holiday.php?d=", x, sep ="")
  pURL = url(url.x)
  temp = readLines(pURL, warn = F)
  close(pURL)
  #temp = getURL(url.x)
  #temp<-gsub(pattern="\"",replacement="",temp)
  sign = substr(temp, 14, 14)
  if(sign == "0") return("weekday")
  if(sign == "1") return("weekend")
  if(sign == "2") return("holiday")
}

getDate <- function(date){
  date <- data.frame(date,stringsAsFactors = F)
  tmpdate <- date[,1] %>% as.character()
  tmpdate <- str_replace_all(tmpdate,'-','')
  tmpdate <- as.character(tmpdate)
  dateformat <- vapply(tmpdate,is.holiday,FUN.VALUE = 'weekday')
  dateformat <- as.vector(dateformat)
  date$dateformat <- dateformat
  date <- data.frame(date,stringsAsFactors = F)
  return(date)
}


#statistic the frequently used variables by the holidays
stats_var_Week_func <- function(df,time,amt,id = id){
  tmptime <- df[,time]
  tmptime <- tmptime %>% as.character()
  tmptime <- substr(tmptime,1,10)
  tmptime <- str_replace_all(tmptime,'-','')
  tmptime <- as.character(tmptime)
  tmptime <- vapply(tmptime,is.holiday,FUN.VALUE = 'weekday')
  tmptime <- as.vector(tmptime)
  gp_df <- data.frame(hourWeek = tmptime,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,hourWeek)
  gp_stats_df <- summarise(gp_df,
                           freq = n(),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T))
  tmp <- data.frame(gp_stats_df,stringsAsFactors = F)
  tmpName <- c()
  for (i in as.character(tmp[,1])){
    for (j in names(tmp)[-1]){
      tmpName <- c(tmpName,paste(i,j,sep = '_'))
    }
  }
  tmpName <- c('custorm_id',tmpName)
  tmp <- as.matrix(tmp[,-1])
  tmp <- as.vector(t(tmp))
  tmp <- append(id,tmp)
  stats.df <- data.frame()
  stats.df <- rbind(stats.df,tmp)
  names(stats.df) <- tmpName
  return(stats.df)
}

stats_var_Week_func(UserInfo,'time_buy','amt_order',id = id)

#another version to get the holidays
  #get the holiday from the dataframe of date 
mapDate <- function(df,vec){
  vec <- as.Date(vec) %>% as.character()
  result <- vector(length = length(vec))
  tmpdate <- as.Date(df[,1] %>% as.character()) %>% as.character()
  for (i in 1:length(vec)){
    idx <- which(vec[i] == tmpdate)
    result[i] <- df[idx,2]
  }
  result <- as.character(result)
  return(result)
}

stats_var_Week_func_new <- function(df,time,amt,id = id){
  tmptime <- df[,time]
  tmptime <- mapDate(date,tmptime)
  gp_df <- data.frame(hourWeek = tmptime,amts = as.numeric(df[,amt]))
  gp_df <- group_by(gp_df,hourWeek)
  gp_stats_df <- summarise(gp_df,
                           freq = n(),
                           mean = mean(amts,na.rm = T),
                           medi = median(amts,na.rm = T),
                           sum = sum(amts,na.rm = T))
  tmp <- data.frame(gp_stats_df,stringsAsFactors = F)
  tmpName <- c()
  for (i in as.character(tmp[,1])){
    for (j in names(tmp)[-1]){
      tmpName <- c(tmpName,paste(i,j,sep = '_'))
    }
  }
  tmpName <- c('custorm_id',tmpName)
  tmp <- as.matrix(tmp[,-1])
  tmp <- as.vector(t(tmp))
  tmp <- append(id,tmp)
  stats.df <- data.frame()
  stats.df <- rbind(stats.df,tmp)
  names(stats.df) <- tmpName
  return(stats.df)
}

stats_var_Week_func_new(UserInfo,'time_buy','amt_order',id = id)
