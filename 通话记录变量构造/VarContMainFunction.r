library(dplyr)

#find the strange sign
find_strange_sign<-function(df,n){
  sign<-c()
  for(i in names(df)){
    vec<-df[,i]
    vec<-as.character(vec)%>%as.factor()
    le<-levels(vec)[1:150]
    sign<-c(sign,le)
  }
  sign<-as.factor(sign)
  return(levels(sign)[1:n])
}

#replace the strange sign to NA
stran_sign_handle<-function(df,strange.list){
  strange.list<-as.character(strange.list)
  for(i in names(df)){
    df[,i]<-as.character(df[,i])
    ssp<-df[,i]%in%strange.list%>%which()
    if(length(ssp)>0)
      df[ssp,i]<-NA
  }
  return(df)
}

#variable derivative for the name field
var_deri_name <-  function(df,str){
  df[,str] <- ifelse(is.na(df[,str]),'null','notnull')
  return(df)
}

#variable derivative for the number field
var_deri_number <- function(df,str){
  Mobile_strings=c('134','135','136','137','138','139','147','150','151','152',
                   '157','158','159','178','182','183','184','187','188')
  Unicom_strings=c('130','131','132','145','155','156','175','176','185','186')
  Telecom_strings=c('133','153','177','180','181','189')
  tmp <- c(rep('Others',length(df[,str])))
  tmp[substr(df[,str],1,3)%in%Mobile_strings] <- 'Mobile'
  tmp[substr(df[,str],1,3)%in%Unicom_strings] <- 'Unicom'
  tmp[substr(df[,str],1,3)%in%Telecom_strings] <- 'Telecom'
  df[,str] <- tmp
  return(df)
}

#variable derivative for the email field
var_deri_email <-  function(df,str){
  df[,str] <- ifelse(is.na(df[,str]),0,1)
  return(df)
}

#variable derivative for merge the fields
var_deri_merge <- function(df,str1,str2){
  str <- paste(str1,str2,sep = '_')
  tmp <- paste0(df[,str1],df[,str2])
  df[,str] <- tmp
  return(df)
}

#merge the two dataframe 
merge_data_frame <- function(raw_df,new_df,str){
  count_summary_groupby <- group_by(raw_df,raw_df[,str])
  count_summary <- summarise(count_summary_groupby,summary_count=n())
  names(count_summary)[1] <- str
  new_df <- left_join(count_summary,new_df,by= c(str))
  new_df <- data.frame(new_df)
  return(new_df)
}

#discriminate the name to family or notfamily
get_alias_name<-function(df,vec){
  strings <- c('爸','妈','哥','姐','弟','妹','姨','舅','叔','伯','姑','家',
               '老婆','老公','媳妇','爷','奶')
  l <- data.frame(initial=rep(1,length(df[,vec])))
  for(string in strings){
    string_vec <- ifelse(grepl(string,df[,vec]),1,0)
    l[,string] <- string_vec
  }
  l <- as.matrix(l)
  tmp <- ifelse(apply(l,1,sum)>1,'family','notfamily')
  df[,vec] <- tmp
  return(df)
}

#variable statistics function
var_statistics<-function(df,result_df,newname=vars){
  for(var in newname){
    matrx<-table(df[,'appl_no'],df[,var])
    tmp<-colnames(matrx)
    colnames(matrx)<-paste(var,tmp,'count',sep='_')
    #sum_matrx<-apply(matrx,1,sum)
    #sum_matrx<-as.matrix(sum_matrx)
    #colnames(sum_matrx)<-paste(var,'sum',sep='_')
    matrxprop<- matrx%>%prop.table(.,1)
    colnames(matrxprop)<-paste(var,tmp,'percent',sep='_')
    logmatrx<-log(matrxprop)*(-matrxprop)
    entropy<-apply(logmatrx,1,function(s)sum(s,na.rm=T))
    normatrx<-apply(matrxprop,2,sum) 
    normatrx<-t(apply(matrxprop,1,function(s)s/normatrx))
    norlogmatrx<-log(normatrx)*(-normatrx)
    norentropy<-apply(norlogmatrx,1,function(s)sum(s,na.rm=T))
    end<-cbind(entropy,norentropy)
    colnames(end)<-paste(var,c('entropy','normalization_entropy'),sep='_')
    tmp<-cbind(matrx,matrxprop,end)
    result_df<-data.frame(result_df,tmp[result_df[,'appl_no'],])}
  return(result_df)
}

#discriminate the call number of all 
get_class_number <- function(df,str){
  Mobile_strings = c('134','135','136','137','138','139','147','150','151','152',
                     '157','158','159','178','182','183','184','187','188')
  Unicom_strings = c('130','131','132','145','155','156','175','176','185','186')
  Telecom_strings = c('133','153','177','180','181','189')
  ICBC <- c('95588')   #工行
  ABC <- c('95599')    #农行
  CCB <- c('95533')    #建行
  BOC <- c('95566')    #中行
  BOCOM <- c('95559')  #交行
  CMB <- c('95555')    #招行
  CEB <- c('95595')    #光大
  CNCB <- c('95558')   #中信
  SPDB <- c('95528')   #浦发
  CGB <- c('95508')    #广发
  PAB <- c('95511')    #平安
  OtherBank <- c('95580','95561','95568','95577')
  tmp <- c(rep('Others',length(df[,str])))
  tmp[substr(df[,str],1,3)%in%Mobile_strings] <- 'Mobile'
  tmp[substr(df[,str],1,3)%in%Unicom_strings] <- 'Unicom'
  tmp[substr(df[,str],1,3)%in%Telecom_strings] <- 'Telecom'
  tmp[substr(df[,str],1,5)%in%ICBC] <- 'ICBC'
  tmp[substr(df[,str],1,5)%in%ABC] <- 'ABC'
  tmp[substr(df[,str],1,5)%in%CCB] <- 'CCB'
  tmp[substr(df[,str],1,5)%in%BOC] <- 'BOC'
  tmp[substr(df[,str],1,5)%in%BOCOM] <- 'BOCOM'
  tmp[substr(df[,str],1,5)%in%CMB] <- 'CMB'
  tmp[substr(df[,str],1,5)%in%CEB] <- 'CEB'
  tmp[substr(df[,str],1,5)%in%CNCB] <- 'CNCB'
  tmp[substr(df[,str],1,5)%in%SPDB] <- 'SPDB'
  tmp[substr(df[,str],1,5)%in%CGB] <- 'CGB'
  tmp[substr(df[,str],1,5)%in%PAB] <- 'PAB'
  tmp[substr(df[,str],1,5)%in%OtherBank] <- 'OtherBank'
  df[,str] <- tmp
  return(df)
}

#calculate the datediff from the appl_sbm_tm to the call_time
get_datediff <- function(df,date1,date2){
  str <- paste(substr(date1,1,4),substr(date2,1,4),sep = '_')
  tmp1 <- difftime(strptime(df[,date1],'%Y-%m-%d %H:%M:%S'),
                   strptime(df[,date2],'%Y-%m-%d %H:%M:%S'),
                   units = 'days')
  tmp2 <- cut(tmp1%>%as.numeric(),breaks = c(-Inf,15,30,60,90,Inf),
              labels = c('HalfMonth','OneMonth','TwoMonth','ThreeMonth',
                         'ThreeMonthPlus'))
  tmp2 <- tmp2%>%as.character()
  df[,str] <- tmp2
  return(df)
}

#get the hours from the call_time 
get_hours <- function(df,date){
  str <- paste(substr(date,1,4),'hour',sep = '_')
  tmp1 <- substr(df[,date],12,13)
  tmp2 <- cut(tmp1%>%as.numeric(),breaks = c(0,7,13,15,19,24),
              labels = c('Midnight','Morning','Noon','Afternoon','Evening'),
              right = FALSE)
  tmp2 <- tmp2%>%as.character()
  df[,str] <- tmp2
  return(df)
}

#merge the two,three,four or more fields
var_two_merge <- function(df,str1,str2){
  str <- paste(str1,str2,sep = '_')
  tmp <- paste0(df[,str1],df[,str2])
  df[,str] <- tmp
  return(df)
}

var_three_merge <- function(df,str1,str2,str3){
  str <- paste(str1,str2,str3,sep = '_')
  tmp <- paste0(df[,str1],df[,str2],df[,str3])
  df[,str] <- tmp
  return(df)
}

var_four_merge <- function(df,str1,str2,str3,str4){
  str <- paste(str1,str2,str3,str4,sep = '_')
  tmp <- paste0(df[,str1],df[,str2],df[,str3],df[,str4])
  df[,str] <- tmp
  return(df)
}

#statistics the frequently used variables   
num_statistics<-function(df,wide.df,numer_var,id.var){
  gp.df<-data.frame(idx=df[,id.var],value=as.numeric(df[,numer_var]))
  gp.df <- group_by(gp.df,idx)
  gped.df <- summarise(gp.df,
                       q1 = quantile(value,c(1/4)),
                       q2 = quantile(value,c(1/2)),
                       q3 = quantile(value,c(3/4)),
                       ske = skewness(value),
                       kur = kurtosis(value),
                       meann=mean(value,na.rm=T),
                       sumn=sum(value,na.rm=T),
                       maxn=max(value,na.rm=T),
                       minn=min(value,na.rm=T),
                       std=sd(value,na.rm = T))
  
  tmp<-as.data.frame(gped.df)
  tmp[,1]<-as.character(tmp[,1])
  tmp.name<-names(tmp)
  tmp.name<-paste(numer_var,tmp.name,sep='_')
  tmp.name[1]<-id.var
  names(tmp)<-tmp.name
  tmp<-as.matrix(tmp)
  rownames(tmp)<-tmp[,id.var]
  tmp<-tmp[,-1]
  wide.df<-cbind(wide.df,tmp[rownames(wide.df),])
  return(wide.df)
}
