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
