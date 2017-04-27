library(rjson)
library(RMySQL)
library(DBI)
#################################################
dbname   = "data_analysis" 
username = "fangju"
password = "QY3[pVFUFiCO(woo)e2O5<9na9O7ir"
host     = "192.168.2.83"
port     = 3359
#######################################
config <- data.frame(dbname,username,password,host,port,stringsAsFactors=FALSE)

conn <- dbConnect(MySQL(), 
                  dbname   = config$dbname, 
                  username = config$username, 
                  password = config$password,
                  host     = config$host,
                  port     = as.integer(config$port))
dbSendQuery(conn,'SET NAMES GBK')

##SQL Query##
raw_data<- dbGetQuery(conn, "SELECT * FROM dss_market_all_0928")
plt.data<-raw_data
