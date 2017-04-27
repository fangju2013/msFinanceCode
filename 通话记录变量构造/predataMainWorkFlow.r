library(data.table)
library(parallel)

raw.data <- data_basic.call_record_appl_moi_n1

Appl_To_Date_Call<-with(raw.data,Date_Convert_Func(appl_sbm_tm_date)) #return the string

time_contact_call <- with(raw.data,Date_Convert_Func(time_contact)) #return the string

Cond_Month_Call <- Cond_Mon_Func(Appl_To_Date_Call,time_contact_call) #return dataframe

Cond_Week_Call <- Cond_Week_Func(time_contact_call) #return the dataframe

Cond_Daytime_Call <- with(raw.data,Day_Time_Func(time_contact)) #return the dataframe

Type_Contact_Call <- with(raw.data,type_contact) #which the result is the string

ContactType_Call <- with(raw.data,Contact_Type_Func(Type_Contact_Call)) #return dataframe

Type_Call <- with(raw.data,type_call)  #which the result is the string 

CallType_Call <- with(raw.data,Call_Type_Func(Type_Call)) #return the dataframe

PhoneType_Call <- with(raw.data,Handle_Phone_Func(phone_contact)) #return dataframe

raw.data$num_contact <- as.numeric(as.character(raw.data$num_contact))

month <- c('OM','TM','SM','OW')
week <- c('WD','NWD','WW')
daytime <- c('MORN','NOON','NIGT','MIDNIGT','WW')
contacttype <- c('LOC','INTER','OTH','WW')
calltype <- c('ACT','NEG','WW')
object <- c('ContactDuration')
calculation <- c('SUM','MAX','MIN','SD','MEAN','MEDIAN','SKEWNESS','KURTOS','ENTROP')

First_List<-expand.grid(month,week,daytime,contacttype,calltype,object,calculation) #return dataframe

First_NameList<-with(First_List,paste0(Var7,'_',Var1,'_',Var2,'_',Var3,'_',Var4,'_',Var5,'_',Var6))#return the string

Call_Name1 <- paste("'",First_NameList[1:1500],"'",sep='',collapse=',') #return a string
Var_expr<-paste0('varlist<-list(',Call_Name1,')')  #return a string 
eval(parse(text=Var_expr))    #return the list of the varlist


First_offline_Func <- function(varlist){
  var_sep <- strsplit(varlist,'_')    #the function strsplit returns the list
  var_caculation <- tolower(unlist(lapply(var_sep,'[[',1)))
  var_month <- unlist(lapply(var_sep,'[[',2))
  var_week <- unlist(lapply(var_sep,'[[',3))
  var_daytime <- unlist(lapply(var_sep,'[[',4))
  var_contact_type <- unlist(lapply(var_sep,'[[',5))
  var_call_type <- unlist(lapply(var_sep,'[[',6))
  var_object <- unlist(lapply(var_sep,'[[',7))
  var_object[var_object == 'ContactDuration'] = 'num_contact'

  cond_month_expr <- paste0(var_month,'<-Cond_Month_Call$',var_month)
  cond_week_expr <- paste0(var_week,'<-Cond_Week_Call$',var_week)
  cond_daytime_expr <- paste0(var_daytime,'<-Cond_Daytime_Call$',var_daytime)
  cond_contactType_expr <- paste0(var_contact_type,'<-ContactType_Call$',var_contact_type)
  cond_callType_expr <- paste0(var_call_type,'<-CallType_Call$',var_call_type)
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
    df_cal_expr<-paste0('df1<-df_tb[,',var_caculation,'(NaToZero(',var_object,')),by=appl_no]')
    eval(parse(text = df_cal_expr))
  }
  df1<-data.frame(df1)
  names(df1)<-c('appl_no',paste0('crawl_mobile_',varlist))
  return(df1)
}
}

cl.cores <- detectCores(logical = F)

call1 <- mclapply(varlist,function(x)(First_offline_Func(x)),mc.cores=getOption('cl.cores',4))
call2 <- Reduce(function(x,y){merge(x,y,by='appl_no',all=T)},call1)

class(call1)


