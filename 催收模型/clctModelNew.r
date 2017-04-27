library(dplyr)
library(RODBC)
ch <- odbcConnect("impalaodbc",uid="ju.fang",pwd = "5ca8b1ff1de54242")
sqlTables(ch)
raw.data <- sqlQuery(ch,"select* from data_basic.scl_basicInfo_0315",
                 stringsAsFactors = F,as.is = T)
odbcClose(ch)

# join the jdVariables , the basic info and the target variable  
basicInfoData <- raw.data
targetLabel <- unique(targetLabel)
analysisData <- left_join(basicInfoData,targetLabel,by=c('union_id'))
ALLDF <- filter(ALLDF,id != '男')
analysisData <- left_join(analysisData,ALLDF,by = c('id'))

# distinguish the variables which are useless,numeric and factor variables

varsTypeFunc <- function(df){
  df <- data.frame(df)
  types <- unique(df[,2])
  typeList <- list()
  for(i in 1:length(types)){
    tmp <- which(df[,2] == types[i])
    typeList[[i]] <- df[,1][tmp]
  }
  names(typeList) <- types
  return(typeList)
}
typesList <- varsTypeFunc(clctVarMap)

#handle the strange sign 
find_strange_sign(analysisData,50)


