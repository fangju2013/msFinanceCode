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
  CarMob <- c('10086')
  CarUni <- c('10010')
  CarTel <- c('10000')
  ICBC <- c('95588')
  ABC <- c('95599')
  BOC <- c('95566')
  CCB <- c('95533')
  CMB <- c('95555')
  BOCOM <- c('95559')
  SEC <- c('95528','95561','95595','95501','95568','95558','95508','95577','95561','11183')
  JX <- c('4000271262')
  BQ <- c('4009987101','4009987103')
  FQL <- c('01057140272')
  YDW <- c('4008902180')
  HDW <- c('4008055855')
  YND <- c('4006099400')
  WDZJ <- c('4008821802')
  YLD <- c('4000805055')
  CASH <- c('02180203636','10101058','4008635151','4000055002','4007910888','4008323696','4006589966','4007777711','10100360','4008970288','4009688821','4008181868','4008230011','4008400500','4007771268')
  JDJR <- c('95118')
  BY <- c('4006695526')
  ZY <- c('4008295195')
  JC <- c('4008888172')
  ZL <- c('4000012222')
  HE <- c('4006999999')
  SN <- c('4008365365')
  HY <- c('4008689966')
  tmp <- c(rep('Others',length(df[,str])))
  tmp[substr(df[,str],1,3)%in%Mobile_strings] <- 'Mobile'
  tmp[substr(df[,str],1,3)%in%Unicom_strings] <- 'Unicom'
  tmp[substr(df[,str],1,3)%in%Telecom_strings] <- 'Telecom'
  tmp[df[,str]%in%CarMob] <- 'CarMob'
  tmp[df[,str]%in%CarUni] <- 'CarUni'
  tmp[df[,str]%in%CarTel] <- 'CarTel'
  tmp[df[,str]%in%ICBC] <- 'ICBC'
  tmp[df[,str]%in%ABC] <- 'ABC'
  tmp[df[,str]%in%BOC] <- 'BOC'
  tmp[df[,str]%in%CCB] <- 'CCB'
  tmp[df[,str]%in%CMB] <- 'CMB'
  tmp[df[,str]%in%BOCOM] <- 'BOCOM'
  tmp[df[,str]%in%SEC] <- 'SEC'
  tmp[df[,str]%in%JX] <- 'JX'
  tmp[df[,str]%in%BQ] <- 'BQ'
  tmp[df[,str]%in%FQL] <- 'FQL'
  tmp[df[,str]%in%YDW] <- 'YDW'
  tmp[df[,str]%in%HDW] <- 'HDW'
  tmp[df[,str]%in%YND] <- 'YND'
  tmp[df[,str]%in%WDZJ] <- 'WDZJ'
  tmp[df[,str]%in%YLD] <- 'YLD'
  tmp[df[,str]%in%CASH] <- 'CASH'
  tmp[df[,str]%in%JDJR] <- 'JDJR'
  tmp[df[,str]%in%BY] <- 'BY'
  tmp[df[,str]%in%ZY] <- 'ZY'
  tmp[df[,str]%in%JC] <- 'JC'
  tmp[df[,str]%in%ZL] <- 'ZL'
  tmp[df[,str]%in%HE] <- 'HE'
  tmp[df[,str]%in%SN] <- 'SN'
  tmp[df[,str]%in%HY] <- 'HY'
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

quantDF <- data.frame(init = c(0,1/4,1/2,3/4,1,0.2,0.8))
for (var in names(new.data)[-1]){
  quantDF[,var] <- c(quantile(new.data[,var]),quantile(new.data[,var],c(0.2)),
                     quantile(new.data[,var],c(0.8)))
} 

write.csv(quantDF,file = '~/VarQuantileDf.csv',fileEncoding='gbk')





