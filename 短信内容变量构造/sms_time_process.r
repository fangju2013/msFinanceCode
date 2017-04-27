time_process<- function(data,t1='sms_time',t2='appl_sbm_tm'){
  library(dplyr)
  data = data[(nchar(data$custorm_id)==7),]
  sms_time_tran <- function(time){
    # if(nchar(time)==14){sms_date =strptime(time,format='%Y%m%d%H%M%S')}
    # else{sms_date =strptime(time,format='%Y-%m-%d %H:%M:%S')}
    sms_date <- if_else(nchar(time)==14,strptime(time,format='%Y%m%d%H%M%S'),
                        strptime(time,format='%Y-%m-%d %H:%M:%S'))
    return(as.character(sms_date))
  }
  
  t1=data[,t1]
  # t1 = apply(as.matrix(t1),1,sms_time_tran)
  t1=sms_time_tran(t1)
  t2= data[,t2]
  
  datediff=as.numeric(difftime(t2,t1,units = 'days'))
  data$datediff = datediff
  
  require(lubridate)
  get_hour <- function(time){
    hour=hour(time)
    return(hour)
  }
  
  # hour = apply(as.matrix(t1),1,get_hour)
  hour=hour(t1)
  data$sms_hour = hour
  
  get_moments <- function(hour){
    require(dplyr)
    moment <- case_when(
      hour>=0 & hour<2 ~ "midnight",
      hour>=2 & hour<7 ~ "deepnight",
      hour>=7 & hour<9 ~ "morning",
      hour>=9 & hour<12 ~ "am",
      hour>=12 & hour<14 ~ "noon",
      hour>=14 & hour<19 ~ "pm",
      hour>=19 & hour<=23 ~ "night"
    )
    return(moment)
  }
  
  # moment = apply(as.matrix(hour),1,get_moments)
  moment <- get_moments(hour)
  data$sms_moment = moment
  
  return(data)
  
}