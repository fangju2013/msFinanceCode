library(dplyr)
#define variable type 
id_vars<-c('union_id','appl_no_last','prod_cd','contra_no','cust_id','loan_cd')
category_vars<-c('GENDER_cd','state_name','soc_id',"unit_inds_cat"
                 ,"unit_prop","marg_status","HOUSE_COND","loan_purp","edu_degree")

numeric_vars<-c('mo_earn','age','appl_lim','other_loan_num','other_amt','other_credit_num'
                ,'houseFund_amt','consume_amt','commercialHouse_amt','acc_lim','work_life','datediff'
                ,"crdt_lim","principal","interest","penalty","limit_used_rate","fee","all_day_ratio"
                ,"roi","M1","M2","M3","M3_plus")
target_var<-c('aprv_status')

#get the data which is needed
mp<-which(plt.data$aprv_status=='Success')
candidate_data<-plt.data[mp,c(category_vars,numeric_vars)]

#handle the strange sign
strange.list<-find_strange_sign(candidate_data,10)

#handle the numeric data
for (i in numeric_vars){
  candidate_data[,i]<-round(as.numeric(candidate_data[,i]),2)
  mp<-which(is.na(candidate_data[,i])|is.infinite(candidate_data[,i]))
  candidate_data[mp,i]<-mean(candidate_data[-mp,i],na.rm=T)
  candidate_data[,i]<-as.character(candidate_data[,i])%>%as.numeric()
}

#handle the category data
for (i in category_vars){
  mp<-which(is.na(candidate_data[,i])|candidate_data[,i]=='NULL')
  candidate_data[mp,i]<-'missing###'
  candidate_data[,i]<-as.character(candidate_data[,i])%>%as.factor()
}

#construct the modeling  by clustering 
library(clustMixType)
a<-lambdaest(candidate_data)
cluster<-kproto(candidate_data,10,lambda = a)
cluster$cluster
write.csv(cluster$centers)
