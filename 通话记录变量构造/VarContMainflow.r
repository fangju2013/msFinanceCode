library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
app_call_record <- sqlQuery(ch,"select * from data_basic.fj_app_call_record_all",
                            as.is=T)

app_contacts <- sqlQuery(ch,"select * from data_basic.fj_app_contacts_all",as.is=T)
label <- sqlQuery(ch,"select * from data_basic.scl_label_tmp1",as.is = T)
############################################################################
######################################app_contacts
source('VarContMainFunction.r')
raw_data <- app_contacts

#find the strange sign
strange.list <- find_strange_sign(raw_data,200)
strange.list <- c(strange.list[1:113],'null','NULL')
#handle the strange sign
raw_data <- stran_sign_handle(raw_data,strange.list)

#variable derivative for the name field
raw_data <- var_deri_name(raw_data,'name')

#variable derivative for the number field
raw_data <- var_deri_number(raw_data,'number')

#variable derivative for the email field
raw_data <- var_deri_email(raw_data,'email')

#variable derivative for merge the fields
raw_data <- var_deri_merge(raw_data,'name','number')

#variable derivative for the given fields
vars<-c('name','number','email','name_number')
new_data<-data.frame(appl_no=unique(raw_data$appl_no[complete.cases(raw_data$appl_no)]))
new_data<-var_statistics(raw_data,new_data,vars)

#merge the two dataframe 
new_data <- merge_data_frame(raw_data,new_data,'appl_no')

#########################################################app_call_record_hive
raw_data1<-app_call_record

#discriminate the name to family or notfamily
raw_data1 <- get_alias_name(raw_data1,'name')

#discriminate the call number of all 
raw_data1 <- get_class_number(raw_data1,'number')

#calculate the datediff from the appl_sbm_tm to the call_time 
raw_data1 <- get_datediff(raw_data1,'appl_sbm_tm','call_time')

#get the hours from the call_time 
raw_data1 <- get_hours(raw_data1,'call_time')

#merge the two,three,four or more fields
raw_data1 <- var_two_merge(raw_data1,'name','type')
raw_data1 <- var_two_merge(raw_data1,'number','type')
raw_data1 <- var_two_merge(raw_data1,'appl_call','type')
raw_data1 <- var_two_merge(raw_data1,'call_hour','type')
raw_data1 <- var_two_merge(raw_data1,'name','appl_call')
raw_data1 <- var_two_merge(raw_data1,'name','call_hour')
raw_data1 <- var_two_merge(raw_data1,'number','appl_call')
raw_data1 <- var_two_merge(raw_data1,'number','call_hour')
raw_data1 <- var_three_merge(raw_data1,'appl_call','name','type')
raw_data1 <- var_three_merge(raw_data1,'call_hour','name','type')
raw_data1 <- var_three_merge(raw_data1,'appl_call','number','type')
raw_data1 <- var_three_merge(raw_data1,'call_hour','number','type')
raw_data1 <- var_four_merge(raw_data1,'appl_call','call_hour','name','type')
raw_data1 <- var_four_merge(raw_data1,'appl_call','call_hour','number','type')

#variable statistics
vars<-c('type','name','number','appl_call','call_hour',
        'name_type','number_type','appl_call_type','call_hour_type',
        'name_appl_call','name_call_hour','number_appl_call','number_call_hour',
        'appl_call_name_type','call_hour_name_type','appl_call_number_type',
        'call_hour_number_type','appl_call_call_hour_name_type',
        'appl_call_call_hour_number_type')
new_data1<-data.frame(appl_no=unique(raw_data1$appl_no[complete.cases(raw_data1$appl_no)]))
new_data1<-var_statistics(raw_data1,new_data1,vars)

#merge the two dataframe 
new_data1 <- merge_data_frame(raw_data1,new_data1,'appl_no')

#save the result in the given directory
save(new_data,file = '~/app_contact.RData',compress=T)
save(new_data1,file = '~/app_call_record.RData',compress=T)

#write the data to the given directory
write.csv(new_data,file = 'C:\\Users\\Administrator\\Desktop\\new_data.csv')

data1 = names(new_data)
data2 = names(new_data1)
data3 = names(APP_Devices)
write.csv(data1,file ='~/contact.csv')
write.csv(data2,file = '~/call_record.csv')
write.csv(data3,file = '~/app_device.csv',fileEncoding='gbk')
