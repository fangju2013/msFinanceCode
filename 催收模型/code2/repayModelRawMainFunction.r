#get the variables and the target by every user 
repayVarFunc <- function(vec){
  library(moments)
  repay_count <- length(vec)
  repay_timely <- sum(if_else(vec<=0,1,0),na.rm = T)
  repay_overdue <- sum(if_else(vec>0,1,0),na.rm = T)
  repay_timely_max <- max(vec[vec<=0],na.rm = T)
  repay_timely_min <- min(vec[vec<=0],na.rm = T)
  repay_overdue_max <- max(vec[vec>0],na.rm = T)
  repay_overdue_min <- min(vec[vec>0],na.rm = T)
  repay_timely_mean <- mean(vec[vec<=0],na.rm = T)
  repay_overdue_mean <- mean(vec[vec>0],na.rm = T)
  repay_mean <- mean(vec,na.rm = T)
  repay_timely_var <- var(vec[vec<=0],na.rm = T)
  repay_overdue_var <- var(vec[vec>0],na.rm = T)
  repay_var <- var(vec,na.rm = T)
  repay_skewness <- skewness(vec,na.rm = T)
  repay_kurtosis <- kurtosis(vec,na.rm = T)
  repay_result <- cbind(repay_count,repay_timely,repay_overdue,repay_timely_max,
                        repay_timely_min,repay_overdue_max,repay_overdue_min,
                        repay_timely_mean,repay_overdue_mean,repay_mean,
                        repay_timely_var,repay_overdue_var,repay_var,
                        repay_skewness,repay_kurtosis)
  varNames <<- colnames(repay_result)
  result <- paste(repay_result,collapse = '#')
  return(result)
}


getVarTarg <- function(loan_pmt_dt,last_pmt_dt,last_rpy_dt){
  tmp <- which.max(as.Date(loan_pmt_dt))
  targ <- as.numeric(as.Date(last_rpy_dt[tmp]) - as.Date(last_pmt_dt[tmp]))
  if(length(last_rpy_dt[-tmp])>0){
    Var <- as.numeric(as.Date(last_rpy_dt[-tmp]) - as.Date(last_pmt_dt[-tmp])) 
  }else{
    Var <- NA 
  }
  vars <- repayVarFunc(Var)
  result <- paste(targ,vars,sep = '/')
  return(result)
}

# calculate the missing values ratio
getNaRatio <- function(df,targ){
  samp.l <- nrow(df)
  miss.ratio <- c()
  for(i in names(df)){
    if(i!=targ){
      miss.l <- sum(is.na(df[,i]))
      tmp <- round(miss.l/samp.l,2)
      names(tmp) <- i
      miss.ratio <- c(miss.ratio,tmp)
    }
  }
  return(miss.ratio)
}

#handle the missing values 
miss_handle<-function(df,targ,dis.val=NULL,num.val=NULL){
  df<-data.frame(df)
  df[,targ] <- as.numeric(df[,targ])
  for(i in names(df)){
    if(is.factor(df[,i])&i!=targ&!is.null(dis.val)){
      df[,i]<-as.character(df[,i])
      mp<-which(is.na(df[,i]) | is.infinite(df[,i]) | is.null(df[,i]) | df[,i] == '')
      df[mp,i]<-dis.val
      df[,i]<-as.factor(df[,i])
    }else if(is.numeric(df[,i])&i!=targ&!is.null(num.val)){
      temp<-df[,i]
      mp<-which(is.na(df[,i]) | is.infinite(df[,i]) | is.null(df[,i]) | df[,i] == '')
      df[mp,i]<-num.val
    }
  }
  if(length(df)==1)
    df<-df[,1]
  return(df)
}
