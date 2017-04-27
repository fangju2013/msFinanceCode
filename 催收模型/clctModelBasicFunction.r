#find the strange sign in the dataframe 
find_strange_sign<-function(df,n){
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
  strange.list<-as.character(strange.list)
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

#add flag of NA and NULL 
addFlagNaFunction <- function(inputData,target){
  valueNon <- function(x) ifelse(x=='' | is.na(x) | is.null(x), 1, 0)
  tmp <- which(names(inputData) == target)
  tmpData <- inputData[,-tmp]
  valueNonData <- data.frame(apply(tmpData, 2, valueNon))
  newname <- paste0('NA_', names(valueNonData))
  names(valueNonData) <- newname
  outputData <- cbind(inputData, valueNonData, row.names=NULL)
  return(outputData)
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

#cut the dataframe into training and testing parts when using xgb.cv
balance_cut <- function(df,prob = 0.7,targ){
  g.num <- which(df[,targ] == 0)
  b.num <- which(df[,targ] == 1)
  g.l <- length(g.num) * prob
  b.l <- length(b.num) * prob
  g.sam <- sample(g.num,g.l,replace = F)
  b.sam <- sample(b.num,b.l,replace = F)
  samples <- c(g.sam,b.sam)
  for(i in 1:5){
    samples <- sample(samples,length(samples),replace = F)
  }
  trainDf <- df[samples,]
  testDf <- df[-samples,]
  return(list(train = trainDf,test = testDf))
}

# cut the dataframe into train,validation and test when using the xgb.train
balance_cut <- function(df,prob1=0.6,prob2=0.2){
  df$int <- sample(3,nrow(df),replace = T,prob = c(prob1,prob2,1-prob1-prob2))
  df$flagSplit <- if_else(df$int==1,'train',
                          if_else(df$int==2,'validation','test'))
  df$int <- NULL
  df$no1 <- 1
  df$no <- cumsum(df$no1)
  df$no1 <- NULL
  train_no <- df[df$flagSplit=='train','no']
  vali_no <- df[df$flagSplit=='validation','no']
  test_no <- df[df$flagSplit=='test','no']
  train <- df[train_no,]
  validation <- df[vali_no,]
  test <- df[test_no,]
  train$flagSplit <- NULL
  validation$flagSplit <- NULL
  test$flagSplit <- NULL
  return(list(train=train,validation=validation,test=test))
}

#define the data type of each column
define_types <- function(df,type_fun,vars = NULL){
  if(is.null(vars))
    vars <- names(df)
  vars <- vars[vars%in%names(df)]
  for(i in vars){
    df[,i] <- as.character(df[,i]) %>% type_fun
  }
}

# transform the category variables into one-hot 
one_hot_transform<-function(df,targ,min.rat=0.05){
  for(i in names(df)){
    if(is.factor(df[,i])&i!=targ){
      vec<-as.character(df[,i])
      vec[is.na(vec)]<-'missing'
      all.num<-prop.table(table(vec))
      values<-names(all.num)[all.num>min.rat]
      if(length(values)<1)
        values<-names(all.num[order(all.num,decreasing = T)])[1]
      for(j in values){
        var.na<-paste(i,j,sep='_')
        df[,var.na]<-as.numeric(vec==j)
      }
    }
  }
  return(df)
}
