library(dplyr)
library(stringr)
library(RODBC)

#get the data from the impala 
ch <- odbcConnect("impalaodbc", uid = "ju.fang", pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select * from crawler.crawl_jd_order_product_hive
                     where custorm_id = '5805000' ", as.is=T)
odbcClose(ch)
###############################################################
#clean the JDBrands data
JDbrands <- JDbrands
cleanBrand <- function(df,str){
  df <- data.frame(df)
  tmp <- df[,str]
  tmp <- str_replace_all(tmp, "[(（][\\S\\/\\s\\S]+[）)]", "")
  tmp <- str_replace_all(tmp,"\r\r\n","")
  df[,str] <- tmp
  return(df)
}
JDbrands <- cleanBrand(JDbrands,'Brand')

#preprocessing the JDbrands
brandCandidate<-paste(JDbrands$Brand%>%unique(),collapse = '|')
itemsCandidate<-paste(str_extract_all(JDbrands$classThree,'[\u4e00-\u9fa5]+')%>%unlist()%>%unique(),collapse = '|')
allBrandItems<-paste(JDbrands$Brand,JDbrands$classThree,sep='#')

#get the data of one user
orderProduct <- raw.data
custormID <- "3841100"
UserOrderProduct <- filter(orderProduct,custorm_id == custormID)

#delete the brand which is gift 
UserOrderProduct <- UserOrderProduct[str_match(UserOrderProduct$name,"赠品|快充|充值卡") %>% is.na(),]

#delete the "【京东超市】" from the name field
UserOrderProduct$name <- str_replace_all(UserOrderProduct$name,"（\\S+[\\s\\S+]）|【\\S+】","") 

#get the name brands 
nameBrands<-str_split(UserOrderProduct$name,c(" "),simplify = TRUE,n=2)[,1] # return the matrix

#match the name brands
matchedBrands <- str_extract_all(nameBrands,brandCandidate)  # return the list
matchedItem <- str_extract_all(UserOrderProduct$name,itemsCandidate) # return the list

#get the brandsItem
brandItems <- mapply(function(x,y) paste(x,y,sep = "#"),matchedBrands,matchedItem) #return the list

#get the location from the allBrandItems
getBrandItem <- function(vec){
  idx <- which(!is.na(str_extract(allBrandItems,paste(vec,collapse = '|'))))
  if (length(idx) > 0)
    return(idx[1])
}
df <- JDbrands[lapply(brandItems,getBrandItem) %>% unlist(),]
df
