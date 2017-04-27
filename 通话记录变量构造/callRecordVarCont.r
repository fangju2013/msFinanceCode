# find the strange sign in the dataframe
find_strange_sign<-function(df,n=50){
  sign<-c()
  for(i in names(df)){
    vec<-df[,i]
    vec<-as.character(vec)%>%as.factor()
    le<-levels(vec)[1:10]
    sign<-c(sign,le)
  }
  sign<-as.factor(sign)
  return(levels(sign)[1:n])
}

#set those strange values to NA as a missing value
stran_sign_handle<-function(df,strange.list,allchar=T){
  strange.list <- as.character(strange.list)
  for(i in names(df)){
    type<-class(df[,i])
    df[,i]<-as.character(df[,i])
    ssp<-df[,i]%in%strange.list%>%which()
    df[ssp,i]<-NA
    if(!allchar){
      if(type=='factor')
        df[,i]<-as.factor(df[,i])else if(type=='numeric')
          df[,i]<-as.numeric(df[,i])
    }
  }
  return(df)
}

# handle the name field
nameTranFunction <- function(analysisData){
  strings <- c('爸','妈','哥','姐','弟','妹','姨','舅','叔','伯','姑','家',
               '老婆','老公','媳妇','爷','奶')
  strings <- paste(strings,collapse = '|')
  analysisData <- within(analysisData,{name <- 
    if_else(is.na(str_match(analysisData$name,strings)),
            'Notfamily','Family')})
  return(analysisData)
}

# handle the number field
numberTranFunction <- function(analysisData){
  mainBank <- c('95588','95599','95566','95533','95555','95559')
  secBank <- c('95528','95561','95595','95501','95568','95558','95508','95577','95561',
               '11183')
  loanFinComp <- c('02180203636','10101058','4008635151','4000055002','4007910888',
                   '4006589966','4007777711','10100360','4008970288','4009688821',
                   '4008181868','4008230011','4008400500','4007771268','4000271262',
                   '4009987101','4009987103','01057140272','4008902180','4008055855',
                   '4006099400','4008821802','4000805055','95118','4006695526',
                   '4008295195','4008888172','4000012222','4006999999','4008365365',
                   '4008689966','4008323696')
  mainBank <- paste(mainBank,collapse = '|')
  secBank <- paste(secBank,collapse = '|')
  loanFinComp <- paste(loanFinComp,collapse = '|')
  analysisData <- within(analysisData,{
    number <- if_else(!is.na(str_match(analysisData$number,mainBank)),'MainBank',
                      if_else(!is.na(str_match(analysisData$number,secBank)),'SecBank',
                              if_else(!is.na(str_match(analysisData$number,loanFinComp)),
                                      'LoanFinComp','Others')))
  })
  return(analysisData)
}

# handle the type field
typeTranFunction <- function(analysisData){
  analysisData$type <- analysisData$type %>% as.character()
  analysisData <- within(analysisData,{
    type <- if_else(analysisData$type == '1','Caller',
                    if_else(analysisData$type == '2','Called','Miss'))
  })
  return(analysisData)
}

# handle the call_time field
callTimeTranFunction <- function(analysisData){
  library(lubridate)
  analysisData$call_time<-as.POSIXlt(analysisData$call_time,
                                       format = "%Y-%m-%d %H:%M:%S")
  analysisData$call_time<-hour(analysisData$call_time)%>%as.character()%>%as.numeric()
  analysisData <- within(analysisData,{
    call_time <- if_else(analysisData$call_time %in% c(0,1,2,3,4,5,6),'Midnight',
                         if_else(analysisData$call_time %in% c(7,8,9,10,11),'Morn',
                                 if_else(analysisData$call_time %in% c(12,13,14,15,16,17),'Noon',
                                         if_else(analysisData$call_time %in% c(18,19,20,21,22,23),
                                                 'Night',NULL))))
  })
  return(analysisData)
}

# calcuate the feature
var_calculate <- function(df,newname=names(df)[-1]){
  result_df <- data.frame(custorm_id=unique(df$custorm_id[complete.cases(df$custorm_id)]))
  for(var in newname){
    matrx<-table(df[,'custorm_id'],df[,var]) %>% as.data.frame.matrix()
    tmp<-colnames(matrx)
    colnames(matrx)<-paste(var,tmp,'count',sep='_')
    result_df<-data.frame(result_df,matrx[result_df[,'custorm_id'],],row.names=NULL)}
  return(result_df)
}
