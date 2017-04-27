library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
app_call_record <- sqlQuery(ch,"select * from data_basic.fj_app_call_record_all",
                            as.is=T)

raw.data <- app_call_record

raw.data <- get_alias_name(raw.data,'name')

get_class_number <- function(df,str){
  Mobile_strings <- c('134','135','136','137','138','139','147','150','151','152',
                      '157','158','159','178','182','183','184','187','188')
  Unicom_strings <- c('130','131','132','145','155','156','175','176','185','186')
  Telecom_strings <- c('133','153','177','180','181','189')
  CAR <- c('10086','10010','10000')
  MAIN <- c('95588','95599','95566','95533','95555')
  SEC <- c('95559','95528','95561','95595','95501','95568','95558','95508','95577')
  COMM <- c('4000271262','4009987101','4009987103')
  CASH <- c('02180203636','10101058','4008635151','01057140272','4000055002','4008902180','4008055855','4007910888','4006099400','4008323696','4006589966','4007777711','10100360','4008970288','4008821802','4009688821','4000805055','4008181868','4008230011','4008400500','4007771268')
  FINA <- c('95118','4006695526','4008295195','4008888172','4000012222','95561','4006999999','4008365365','11183','4008689966')
  tmp <- c(rep('Others',length(df[,str])))
  tmp[substr(df[,str],1,3)%in%Mobile_strings] <- 'Mobile'
  tmp[substr(df[,str],1,3)%in%Unicom_strings] <- 'Unicom'
  tmp[substr(df[,str],1,3)%in%Telecom_strings] <- 'Telecom'
  tmp[df[,str]%in%CAR] <- 'CAR'
  tmp[df[,str]%in%MAIN] <- 'MAIN'
  tmp[df[,str]%in%SEC] <- 'SEC'
  tmp[df[,str]%in%COMM] <- 'COMM'
  tmp[df[,str]%in%CASH] <- 'CASH'
  tmp[df[,str]%in%FINA] <- 'FINA'
  df[,str] <- tmp
  return(df)
}

raw.data <- get_class_number(raw.data,'number')

raw.data <- get_hours(raw.data,'call_time')

var_calculate <- function(df,newname=vars){
  result_df <- data.frame(union_id=unique(df$union_id[complete.cases(df$union_id)]))
  for(var in newname){
    matrx<-table(df[,'union_id'],df[,var])
    tmp<-colnames(matrx)
    colnames(matrx)<-paste(var,tmp,'count',sep='_')
    result_df<-data.frame(result_df,matrx[result_df[,'union_id'],])}
  return(result_df)
}

raw.data <- var_two_merge(raw.data,'name','type')

raw.data <- var_two_merge(raw.data,'number','type')

raw.data <- var_two_merge(raw.data,'call_hour','type')

raw.data <- var_two_merge(raw.data,'name','call_hour')

raw.data <- var_two_merge(raw.data,'number','call_hour')

raw.data <- var_three_merge(raw.data,'call_hour','name','type')

raw.data <- var_three_merge(raw.data,'call_hour','number','type')

vars <- c('name','number','type','call_hour','name_type','number_type','call_hour_type',
          'name_call_hour','number_call_hour','call_hour_name_type',
          'call_hour_number_type')

new.data <- var_calculate(raw.data,vars) 

##############################################################################

