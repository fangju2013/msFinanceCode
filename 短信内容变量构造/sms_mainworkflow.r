library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
data <- sqlQuery(ch,"select* from data_basic.app_sms_1116 s where s.appl_no='20160801150005546759'",
                 stringsAsFactors=F,as.is=T)

dfGps <- sqlQuery(ch,"select * from data_basic.app_gps_1121 where appl_no='441013139421532424'",
                  stringsAsFactors=F,as.is=T)

odbcClose(ch)

#handle the data about the sms_time and appl_sbm_tm
data <- time_process(data,t1='sms_time',t2='appl_sbm_tm')
dfGps <- time_process(dfGps,t1='create_time',t2='appl_sbm_tm')
df <- data[,c('appl_no','content','datediff','sms_hour','sms_moment')]
df_null <- df[1,]
df_null$content=NA

dfGps <- dfGps[,c('appl_no','longitude','latitude','datediff','sms_hour','sms_moment')]
dfGps_null <- dfGps[1,]
dfGps_null[,c('longitude','latitude')] <- NA
##############################################
###########第一个函数
t0 <- Sys.time()
r <- sms_length_gen_res(data = df)
t1 <- Sys.time()
print(t1-t0)


t0 <- Sys.time()
r <- sms_length_gen_res(data = df_null)
t1 <- Sys.time()
print(t1-t0)

names(r)[is.infinite(unlist(r[1,]))]

##############################################
##############第二个函数
t0 <- Sys.time()
r <- sms_length_breaks_res(data = df)
t1 <- Sys.time()
print(t1-t0)

t0 <- Sys.time()
r <- sms_length_breaks_res(data = df_null)
t1 <- Sys.time()
print(t1-t0)

##############################################
##############第三个函数
t0 <- Sys.time()
r <- sms_key_word_res(data = df)
t1 <- Sys.time()
print(t1-t0)

t0 <- Sys.time()
r <- sms_key_word_res(data = df_null)
t1 <- Sys.time()
print(t1-t0)
