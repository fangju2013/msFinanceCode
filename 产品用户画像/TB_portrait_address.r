library(dplyr)
library(fBasics)
require(bitops)
library(RCurl)
library(RODBC)
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from crawler.crawl_taobao_order_detail_hive limit 10000",as.is=T)

#get the data information of the user
id = '3767000' 
UserInfo <- dplyr::filter(raw.data,custorm_id == id,sts_order == '交易成功')

#check the data types
str(UserInfo) 
#find the strange sign
strange.list <- find_strange_sign(UserInfo,10) # no strange sign

#get the province and city dataframe
provinceCity <- data.frame(Province,stringsAsFactors = F)
names(provinceCity) <- c('province','city')
#get the address information to the user
maxReceiveAddress <- function(df1,df2){
  province_city <- paste(df1[,1],df1[,2],sep = ' ')
  province_city <- paste(province_city,collapse = '|')
  addressProvinceCity <- str_extract_all(df2$add_receiver,province_city) %>% unlist()
  maxProvinceCity <- str_replace(max(names(table(addressProvinceCity))),' ','')
  province <- paste(df1[,1] %>% unique(),collapse = '|')
  addressProvince <- str_extract_all(df2$add_receiver,province) %>% unlist()
  maxProvince <- max(names(table(addressProvince)))
  maxProvCity <- c(maxProvinceCity,maxProvince)
  names(maxProvCity) <- c('maxProvinceCity','maxProvince')
  return(maxProvCity)
}

maxReceiveAddress(provinceCity,UserInfo)
