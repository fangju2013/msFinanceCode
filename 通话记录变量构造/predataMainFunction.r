# To Date Convert Function
Date_Convert_Func <- function(date_format){
  date_format <- as.character(date_format)
  date_cond1 <- grepl('[0-9]{4}-[0-9]{2}-[0-9]{2}',date_format)  #return the bool 
  date_cond2 <- grepl('[0-9]{8}',date_format)
  date_cond3 <- grepl('[0-9]{6}',date_format)
  date_cond4 <- grepl('[0-9]{4}-[0-9]{2}',date_format)
  date_char <- ifelse(date_cond1,substr(date_format,1,10),
                      ifelse(date_cond2,paste0(substr(date_format,1,4),'-',substr(date_format,5,6),'-',substr(date_format,7,8)),
                             ifelse(date_cond3,paste0(substr(date_format,1,4),'-',substr(date_format,5,6),'-','01'),
                                    ifelse(date_cond4,paste0(date_format,'-01'),NA))))
  result <- as.Date(date_char)
  return(result)
}

# e.g:
date_format <- c('20160101','2016-02-12','2016-03','201607')
Date_Convert_Func(date_format)

# Month condition Function
Cond_Mon_Func <- function(appl_time,x){
  appl_time <- strptime(appl_time,'%Y-%m-%d')
  x <- strptime(x,'%Y-%m-%d')
  if (length(x) != 0){
    OM <- difftime(appl_time,x,units='days') <= 30 
    TM <- difftime(appl_time,x,units='days') <= 60
    SM <- difftime(appl_time,x,units='days') <= 180
    OW <- difftime(appl_time,x,units='days') <= 7
    result <- data.frame(OM,TM,SM,OW)
  }
  else{result <- data.frame(OM=as.integer(0),TM=as.integer(0),SM=as.integer(0),OW=as.integer(0))}
  return(result)
}


# e.g:
Cond_Mon_Func(raw.data$appl_sbm_tm_date,raw.data$time_contact_new)

# Week Day Function
Cond_Week_Func <- function(x){
  
  if (length(x) != 0){
    x <- as.Date(x)
    Weekday <- weekdays(x)
    WD <- Weekday %in% c('Monday','Tuesday','Wednestay','Thursday','Friday')
    NWD <- Weekday %in% c('Saturday','Sunday')
    WW <- T
    result <- data.frame(WD,NWD,WW)
  }else{result <- data.frame(WD=integer(0),NWD=integer(0),WW=integer(0))}
  return(result)
}

# e.g:
x <- c('2016-01-02','2016-11-29','2016-10-10','2016-12-20','2016-12-25')
Cond_Week_Func(x)

##Day Time Function
Day_Time_Func <- function(x){
  if (length(x) != 0){
    hour = substr(x,12,13)
    MORN <- hour %in% c('06','07','08','09','10','11')
    NOON<-hour %in% c('12','13','14','15','16','17')
    NIGT<-hour %in% c('18','19','20','21','22','23')
    MIDNIGT<-hour %in% c('24','01','02','03','04','05')
    WW<-T
    result <- data.frame(MORN,NOON,NIGT,MIDNIGT,WW)
  }else {result <- data.frame(MORN=integer(0),NOON=integer(0),NIGT=integer(0),MIDNIGT=integer(0),WW=integer(0))}
  return(result)
}

#Contact Type Function
Contact_Type_Func <- function(x){
  if(length(x) != 0){
    LOC <- x == '1'
    INTER <- x == '2'
    OTH <- x == '3'
    WW <- T
    result <- data.frame(LOC,INTER,OTH,WW)
  }else{result <- data.frame(LOC=integer(0),INTER=integer(0),OTH=integer(0),WW=integer(0))}
  return(result)
}

#e.g:
x <- c('1','2','3','1','3','2','2','3','1','1','2','1')
Contact_Type_Func(x)

#call type function
Call_Type_Func <- function(x){
  if(length(x)!=0){
    ACT <- x == '1'
    NEG <- x == '2'
    WW <- T
    result <- data.frame(ACT,NEG,WW)
  }else{result <- data.frame(ACT=integer(0),NEG=integer(0),WW=integer(0))}
  return(result)
}

#e.g:
x <- c('1','2','2','1','1','1','2','2','1','2','')
Call_Type_Func(x)

#Message type function
Msg_Type_Func <- function(x){
  if(length(x)!=0){
    SENT <- x == '1'
    RECEV <- x == '2'
    OTH <- x == '3'
    WW <- T
    result <- data.frame(SENT,RECEV,OTH,WW)
  }else{result <- data.frame(SENT=integer(0),RECEV=integer(0),OTH=integer(0),WW=integer(0))}
  return(result)
}

#Carry city function
Carry_City_Func <- function(x){
  if(length(x)!=0){
    MAIN <- grepl('BEIJING',x) | grepl('SHANGHAI',x) | grepl('TIANJIN',x) | grepl('CHONGQING',x)
    SECD <- grepl('FUJIAN',x) | grepl('GUANGDONG',x) | grepl('SHANDONG',x) | grepl('SICHUAN',x) | grepl('ZHEJIANG',x)
    OTH <- !(MAIN | SECD)
    result <- data.frame(MAIN,SECD,OTH)
  }else{result <- data.frame(MAIN=integer(0),SECD=integer(0),OTH=integer(0))}
  return(result)
}

#e.g:
x <- c('BEIJING','SHANGHAI','TIANJIN','GUANGDONG','SHANDONG','ANHUI','FUJIAN')
Carry_City_Func(x)

#Carry type function
Carry_Type_Func <- function(x){
  if(length(x)!=0){
    NET <- grepl('NET',x)
    MOB <- grepl('MOBILE',x)
    UNI <- grepl('UNICOM',x)
    result <- data.frame(NET,MOB,UNI)
  }else{result <- data.frame(NET=integer(0),MOB=integer(0),UNI=integer(0))}
  return(result)
}

#e.g:
x <- c('NET','MOBILE','UNICOM','','NET','UNICOM','MOBILE','','NET')
Carry_Type_Func(x)

#Special phone function
Handle_Phone_Func <- function(x){
  if (length(x)!=0){
    CAR <- grepl('10086|10010|10000',x)
    MAIN <- grepl('95588|95599|95566|95533|95555',x)
    SEC <- grepl('95559|95528|95561|95595|95501|95568|95558|95508|95577',x)
    COMM <- grepl('4000271262|4009987101|4009987103',x)
    CASH <- grepl('02180203636|10101058|4008635151|01057140272|4000055002|4008902180|4008055855|4007910888|4006099400|4008323696|4006589966|4007777711|10100360|4008970288|4008821802|4009688821|4000805055|4008181868|4008230011|4008400500|4007771268',x)
    FINA <- grepl('95118|4006695526|4008295195|4008888172|4000012222|95561|4006999999|4008365365|11183|4008689966',x)
    result <- data.frame(CAR,MAIN,SEC,COMM,CASH,FINA)
  }else{result <- data.frame(CAR=integer(0),MAIN=integer(0),SEC=integer(0),COMM=integer(0),CASH=integer(0),FINA=integer(0))}
  return(result)
}

#e.g:
x <- c('10086','10000','10010','95501','95508','95577','95508','95568','','95533')
Handle_Phone_Func(x)

#Entropy Calculation
entropy <- function(x){
  if(length(unique(x))>=30){
    quantile <- quantile(x,seq(0,1,0.25))
    y <- cut(x,unique(quantile),include.lowest=T,right=T)
    freq <- table(y)/length(y)
    entropy <- -sum(freq*log2(freq))
  }else{entropy <- 9999999}
  return(entropy)
}

#e.g:
x <- c(12,12,13,14,12,13,14,14,15,14,8,4,7,9,10,2,3,6,23,34,13,14,23)
entropy(x)

#Skewness and Kurtosis Caculation
skewness <- function(x){
  skew <- ifelse(length(x)>=30,(sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/length(x))^(3/2),9999999)
  return(skew)
}

kurtosis <- function(x){
  kurt<-ifelse(length(x)>=30,(sum((x-mean(x))^4)/length(x))/(sum((x-mean(x))^2)/length(x))^2-3,9999999)
  return(kurt)
}

#e.g:
x <- c(12,12,13,14,12,13,14,14,15,14,8,4,7,9,10,2,3,6,23,34,13,14,23)
skewness(x)
kurtosis(x)

# NA To Zero
NaToZero <- function(x){
  if(!is.factor(x)){
    x[is.na(x)] <- 0
  }else{ x <- x}
  return(x)
}

#e.g:
x <- c(1,2,3,NA,98,NA,0,3,6)
NaToZero(x)

#map the two dataframe for the given column 
dict_map <- function(df1,df2){
  for (i in names(df1)){
    tmp <- which(df2[,1] == i)
    tmpDf <- df2[tmp,]
    for (j in 1:length(tmpDf[,3])){
      tmp1 <- which(df1[,i] == tmpDf[j,3])
      df1[tmp1,i] <- tmpDf[j,2]
    }
  }
  return(df1)
}

#Variable Name Mapping
First_offline_Func <- function(varlist){
  var_sep <- strsplit(varlist,'_')
  var_caculation <- tolower(unlist(lapply(var_sep,'[[',1)))
  var_month <- unlist(lapply(var_sep,'[[',2))
  var_week <- unlist(lapply(var_sep,'[[',3))
  var_daytime <- unlist(lapply(var_sep,'[[',4))
  var_contact_type <- unlist(lapply(var_sep,'[[',5))
  var_call_type <- unlist(lapply(var_sep,'[[',6))
  var_object <- unlist(lapply(var_sep,'[[',7))
  var_object[var_object=='ContactDuration'] = 'num_contact'
  var_object[var_object=='ContactCounts'] = 'num_contact'
  var_object[var_object=='ContactMemberCounts'] = 'phone_contact'
  
  cond_month_expr <- paste0(var_month,'raw.data$',var_month)
  cond_week_expr <- paste0(var_week,'raw.data$',var_week)
  cond_daytime_expr <- paste0(var_daytime,'raw.data$',var_daytime)
  cond_contactType_expr <- paste0(var_contact_type,'raw.data$',var_contact_type)
  cond_callType_expr <- paste0(var_call_type,'raw.data$',var_call_type)
  eval(parse(text=c(cond_month_expr,cond_week_expr,cond_daytime_expr,cond_contactType_expr,cond_callType_expr)))
  ###get met data######
  mobile_call_expr<-paste0('df_met<-raw.data[',var_month,'&',var_week,'&',var_daytime,'&',var_contact_type,'&',var_call_type,',]')
  eval(parse(text = mobile_call_expr))
  df_met<-df_met[!is.na(df_met$appl_no),]
  ###dataset's nrow is 0 or not#####
  flag<-nrow(df_met)==0
  if(flag){
    df1<-data.frame(appl_no=integer(0),var1=integer(0),stringsAsFactors = FALSE)
  } else{
    df_tb<-data.table(df_met)
    df_cal_expr<-paste0('df1<-df_tb[,',var_caculation,'(nazero(',var_object,')),by=appl_no]')
    eval(parse(text = df_cal_expr))
  }
  df1<-data.frame(df1)
  names(df1)<-c('appl_no',paste0('crawl_mobile_',varlist))
  return(df1)
}
}