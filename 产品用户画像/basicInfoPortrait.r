library(dplyr)
library(stringr)
library(ggplot2)
library(RODBC)

#get the data from the impala 
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from data_basic.fj_basicInfo_prod", as.is=T)

#map the two dataframe 
mapTwoDf <- function(df1,df2){
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

dictionary <- data.frame(dictionary)
dictionary[,1] <- tolower(dictionary[,1])
new.data <- mapTwoDf(raw.data,dictionary)

#plot the basic information using the ggplot
 #plot the gender bar 
new.table <- with(new.data,table(gender_cd,prod_cd))
with(new.data,prop.table(new.table,2))

p <- ggplot(data=new.data[!is.na(new.data$gender_cd),],aes(x=prod_cd,fill=gender_cd))
p <- p + geom_bar(position='dodge',width = 0.6)
p

 #plot the age bar
age <- cut(as.numeric(new.data$age),breaks=c(Inf,-Inf,22,30),
           labels=c('在校青年','奋斗青年','稳定青年'))
age <- as.character(age) 
new.data$age <- age
p <- ggplot(data=new.data[!is.na(new.data$age),],aes(x=prod_cd,fill=age))
p <- p + geom_bar(position='fill',width=0.6)
p

 #plot the marg_status 
with(new.data,table(marg_status,prod_cd))

 #plot the edu_degree 
edu_degree <- new.data$edu_degree
edu_degree[which(edu_degree == 'A')] <- '博士及以上'
edu_degree[which(edu_degree == 'B')] <- '硕士'
edu_degree[which(edu_degree == 'C')] <- '本科'
edu_degree[which(edu_degree == 'D')] <- '大专	'
edu_degree[which(edu_degree == 'E')] <- '中专及技校'
edu_degree[which(edu_degree == 'F')] <- '高中'
edu_degree[which(edu_degree == 'G')] <- '初中及以下'

edu_degree[which(edu_degree%in%c('初中','初中以下','初中及以下'))] <- '小学初中'
edu_degree[which(edu_degree%in%c('中专及技校','高中\\中专\\技校','高中'))] <- '高中'
edu_degree[which(edu_degree%in%c('大学专科\\专科学校','大专','大学本科',
                                 '本科','硕士','博士及以上'))] <- '大学'
edu_degree[which(edu_degree%in%c('大专\t','大学'))] <- '大学及以上'

new.data$edu_degree <- edu_degree
table(new.data$edu_degree)

p <- ggplot(data=new.data[!is.na(new.data$edu_degree),],aes(x=prod_cd,fill=edu_degree))
p <- p + geom_bar(position='fill',width=0.6)
p

p <- ggplot(data=raw.data,aes(x=average,fill=average))
p <- p + geom_bar(position='dodge',width = 0.4)
p <- p + theme(axis.text.x = element_blank())
p

