raw_data <- app_contacts

strange.list <- find_strange_sign(raw_data,200)
strange.list <- c(strange.list[1:113],'null','NULL')

raw_data <- stran_sign_handle(raw_data,strange.list)
raw_data <- var_deri_name(raw_data,'name')
raw_data <- var_deri_number(raw_data,'number')
raw_data <- var_deri_merge(raw_data,'name','number')

vars<-c('name','number','name_number')

var_calculate <- function(df,newname=vars){
  result_df <- data.frame(union_id=unique(df$union_id[complete.cases(df$union_id)]))
  for(var in newname){
    matrx<-table(df[,'union_id'],df[,var])
    tmp<-colnames(matrx)
    colnames(matrx)<-paste(var,tmp,'count',sep='_')
    matrx <- unclass(matrx)
    result_df<-data.frame(result_df,matrx[result_df[,'union_id'],])}
  return(result_df)
}

new_data <- var_calculate(raw_data,vars)

quantDF1 <- data.frame(init = c(0,1/4,1/2,3/4,1,0.2,0.8))
for (var in names(new_data)[-1]){
  quantDF1[,var] <- c(quantile(new_data[,var]),quantile(new_data[,var],c(0.2)),
                     quantile(new_data[,var],c(0.8)))
} 

write.csv(quantDF1,file = '~/VarQuantileDf.csv',fileEncoding='gbk')



