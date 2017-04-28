############################# addFlagNaFunction.R
############# deal with dataframe to input in random forest
## add flag of NA and null
valueNon <- function(x) ifelse(x=='' | is.na(x) | is.null(x), 1, 0)#missing value

addFlagNaFunction <- function(inputData){

	##增加NA标签
	valueNonData <- data.frame(apply(inputData, 2, valueNon))
	newname <- paste0('NA_', names(valueNonData))
	names(valueNonData) <- newname

	outputData <- cbind(inputData, valueNonData, row.names=NULL)
	outputData[is.na(outputData) | is.null(outputData) | outputData == ''] <- 0

	return(outputData)
}

############################# addFlagDefaultFunction.R
#############  add flag of special missing data
## give out defaultNum before using this function
#defaultNum <- c(9990, 9991, 9992, 9993)#special data flag
defaultVector <- function(x, a) ifelse(x==a, 1, 0)

addFlagDefaultFunction <- function(inputData, defaultNum){
	outputData <- inputData

	##增加特殊数据的标签
	for(i in 1:length(defaultNum)){
	  if(nrow(inputData) != 1){
	    v <- data.frame(apply(inputData, 2, function(x)(defaultVector(x, defaultNum[i]))))
	    newname <- paste0(defaultNum[i], '_', names(v))
	    names(v) <- newname
	    outputData <- cbind(outputData, v, row.names=NULL)
	  } else {
	    v <- data.frame(t(apply(inputData,2,function(x)(defaultVector(x,defaultNum[i])))),row.names=NULL )
	    newname <- paste0(names(v), '_', defaultNum[i])
	    names(v) <- newname
	    outputData <- cbind(outputData, v, row.names=NULL)
	  }
	}

	return(outputData)
}



############################# varDeleteFunction.R

## 统计各变量取值为NA的个数
varNaCountFunction <- function(x) sum(is.na(x)|is.null(x)|x == '')

## 各变量非NA取值的种类数
varNonNaTypeFunction <- function(x) {
  if (varNaCountFunction(x) == length(x)) {#全部为空
    varLength <- 0
  } else {
    if (varNaCountFunction(x) == 0){#不存在空值
      varLength <- length(unique(x))
    }else {
      varLength <- length(unique(x)) - 1
    }
  }
  return(varLength)  #unique 不包括NA
}

## data.frame为入参，统计变量取值中NA个数及非NA取值的种类数
# 调用函数 varNaCountFunction 、varNonNaTypeFunction
varCountFunction <- function(dataframe) {
  naCount <- as.data.frame(apply(dataframe, 2, varNaCountFunction))
  names(naCount) <- c("varNaCount")
 
  valueType <- as.data.frame(apply(dataframe, 2, varNonNaTypeFunction))#非NA种类数
  names(valueType) <- c("varNonNaType")
  
  varClass <- as.data.frame(unlist(lapply(dataframe, class)))
  colnames(varClass) <- 'varClass'
  
  varSummary <- cbind(names(dataframe), naCount, valueType, varClass)
  names(varSummary)[1] <- c("varName")

  return(varSummary)
}


## 删除缺失率过高或者只有一个非空取值的变量
varDeleteFunction <- function(dataframe, missThrd){
	#missThrd  变量缺失率阈值
	varCount <- varCountFunction(dataframe) #count NA and non-na value type
	varDelete <- varCount[varCount$varNaCount >= missThrd*nrow(dataframe) | varCount$varNonNaType == 1, ]$varName
	if (length(varDelete) != 0) dataframe <- dataframe[, -which(colnames(dataframe) %in% varDelete)]

	return(dataframe)
}



############################# varGroupFunction.R
##入参：两列的df - tempData，列名分别为target、varName
##数值型分组数n （字符型以原值标记）
varGroupFunction <- function(tempData, target, varName, n){
  if (class(tempData[, varName]) == 'integer' | class(tempData[, varName]) == 'numeric'){#数值型 用quantile分组
    cutPoint = unique(quantile(as.numeric(tempData[, varName]),
                               probs = seq(from = 0, to = 1, length.out = n),
                               na.rm = TRUE))
    
    if (length(cutPoint) >= 3 |
        (length(cutPoint) == 2 & !(cutPoint[1]==min(tempData[, varName]) & cutPoint[2] == max(tempData[, varName])))){
      
      tempData$varGroup <- cut(as.numeric(tempData[, varName]), breaks = cutPoint)
      ##取值最低的手动赋值（cut左开右闭的区间，不能覆盖最小值）
      tempData[, 'varGroup'][!is.na(tempData[, varName]) & is.na(tempData[, c('varGroup')])] <- levels(tempData$varGroup)[1]
      
      tempData[, varName] <- tempData$varGroup
      tempData <- tempData[, -which(colnames(tempData) == 'varGroup')]
    }
  }
  #tempData[, varName] <- tempData[, varName]

  return(tempData)
}


############################# numCountFunction.R
## numCount of sum, target, non target
## input `varGroup` - df of two columns - target, varName(after varGroup)
numCountFunction <- function(varGroup, target, varName){
  ## sum num
  numSum <- data.frame(table(varGroup[, varName]))
  names(numSum) <- c(varName, "numSum")
  
  ## Target num
  numTarget <- data.frame(table(varGroup[varGroup[, target] == 1, varName]))
  names(numTarget) <- c(varName, "numTarget")
  
  ## NonTarget num
  numNonTarget <- data.frame(table(varGroup[varGroup[, target] == 0, varName]))
  names(numNonTarget) <- c(varName, "numNonTarget")

  numCount <- merge(merge(numSum, numTarget, all.x = TRUE), numNonTarget, all.x = TRUE)
  numCount[is.na(numCount)] <- 0
  
  numCount <- numCount[order(numCount[, varName]), ]#order by levels
  
  return(numCount)
}



############################# ratioCalFunction.R
## inputData - dataframe after numCountFunction
## output - dataframe adding ratio of target in each group
ratioCalFunction <- function(numCount){
  ratioCal <- numCount
  ratioCal$ratioInGroup <- round(ratioCal$numTarget / ratioCal$numSum, 4)
  return(ratioCal)
}



############################# varIvCalFunction.R

## 单变量的取值及客户好坏的标签
## input - numCount (dataframe) - output of function `numCountFunction`
## output - iv of single variable
varIvCalFunction <- function(numCount){
  numCount$ratioTarget <- round(numCount$numTarget / sum(numCount$numTarget), 4)
  numCount$ratioNonTarget <- round(numCount$numNonTarget / sum(numCount$numNonTarget), 4)
  
  numCount$woeValue <- round(log(numCount$ratioTarget / numCount$ratioNonTarget), 4)
  numCount$ivValue <- round((numCount$ratioTarget - numCount$ratioNonTarget) * numCount$woeValue, 5)
  
  numCount$ivValue[numCount$ivValue==Inf] <- 0
  
  return(sum(numCount$ivValue)) #iv value
}

############################# ivCalReusltFunction.R
## based on self defined function - varGroupFunction, numCountFunction, varIvCalFunction
## iv calculation for variables in a dataframe
## output - dataframe including varName, ivValue
ivCalReusltFunction <- function(inputData, target, nVarGroup){
  ivVarName <- colnames(inputData) #存储IV计算对应的变量名
  ivValue <- vector(mode="numeric", length=0) #空向量 用于存储计算的IV值
  
  for (i in 1:ncol(inputData)){
    if (colnames(inputData)[i] == target){
      next
    } else {
      varName <- colnames(inputData)[i]
      
      if (sum(is.na(inputData)) == nrow(inputData) | #全部为空的列
          (sum(is.na(inputData)) != nrow(inputData) & length(unique(inputData[, i])) == 1)) {
        #部分缺失，但变量仅有一类取值
        ivValue[i] <- 0
        next
      } else {
        tempData <- inputData[, c(varName, target)] #target和variable列
        tempData <- tempData[!is.na(tempData[, varName]) & !is.null(tempData[, varName]) & tempData[, varName] != '', ]
        varGroup <- varGroupFunction(tempData, target, varName, nVarGroup)
        numCount <- numCountFunction(varGroup, target, varName)    
        ivValue[i] <- varIvCalFunction(numCount)
        ##self defined function for variable iv calculation
      }
    }
  }
  
  ivResult <- as.data.frame(cbind(ivVarName, ivValue))
  ivResult <- ivResult[ivVarName != target ,]
  
  ivResult$ivValue <- as.numeric(as.character(ivResult$ivValue))
  ivResult <- ivResult[order(-ivResult$ivValue), ]#order by IV
  return(ivResult)
}




############################# vifCalFunction.R
## vif calculation of dataframe
#input - dataframe with target in first column

vifCalFunction <- function(vifData, target){
  library(car)
  
  #install.packages("dummies")
  library(dummies)
  vifData <- cbind(vifData[, target], dummy.data.frame(vifData[, colnames(vifData) != target]))
    #add dummy variables
  colnames(vifData)[1] <- target
  
  vifData[is.na(vifData) | is.null(vifData) | vifData == ''] <- 0
  vifData[, target] <- as.character(vifData[, target])
  
  ## vif calculation
  lmFunction <- lm(as.formula(paste('lm(', target, '~., data = vifData)', sep = '')),
                   data = vifData)
  
  #summary(lmFunction)
  varCoefficients <- as.data.frame(cbind(names(vifData)[names(vifData)!=target], 
                                         as.data.frame(lmFunction$coefficients[2:length(lmFunction$coefficients)])))
  
  names(varCoefficients) <- c('varName', 'varCoe')
  varCoefficients$varCoe <- round(as.numeric(varCoefficients$varCoe), 5)
  
  #delete variables with zero coefficients
  varSelect <- varCoefficients[!is.na(varCoefficients$varCoe), ]
  
  #lm with selected variables
  lmData <- as.data.frame(cbind(vifData[, target],
                                vifData[, colnames(vifData) %in% varSelect[, 1]]))
  colnames(lmData)[1] <- target
  lmData[, target] <- as.character(lmData[, target])
  
  lmFunction <- lm(as.formula(paste('lm(', target, '~., data = lmData)', sep = '')),
                   data = lmData)
  
  vifValue <- as.data.frame(round(vif(lmFunction), 3))
  names(vifValue) <- 'vifValue'
  
  vifResult <- as.data.frame(cbind(varSelect$varCoe, vifValue))
  names(vifResult) <- c('varCoeOfLm', 'vifValue')
  
  return(vifResult)
}


############################# lorenzDataFunction.R
# highTrue - flag of variable type (the higher the better)
# input - dataframe after varGroup and numCount Function
lorenzDataFunction <- function(numCount, varName, target, highTrue){
  lorenzData <- numCount
  if (highTrue == FALSE){#取值越小越好，累积数量及比例从小到大的组别依次计算
    lorenzData$cumNumSum <- cumsum(lorenzData$numSum)
    lorenzData$cumNumTarget <- cumsum(lorenzData$numTarget)
    lorenzData$cumRatioTarget <- round(lorenzData$cumNumTarget / lorenzData$cumNumSum, 4)
  }else {#取值越高越好，累积数量及比例 从大到小的组别依次计算
    ncolNum <- ncol(lorenzData)
    for (i in 1:nrow(lorenzData)){
      lorenzData[i, (ncolNum+1)] <- sum(lorenzData$numSum[i:nrow(lorenzData)])
      lorenzData[i, (ncolNum+2)] <- sum(lorenzData$numTarget[i:nrow(lorenzData)])
      lorenzData[i, (ncolNum+3)] <- round(lorenzData[i, (ncolNum+2)] / lorenzData[i, (ncolNum+1)], 4)
    }
    names(lorenzData)[c((ncolNum+1):(ncolNum+3))] <- c('cumNumSum', 'cumNumTarget', 'cumRatioTarget')
  }
  
  lorenzData$ratioInGroup <- round(lorenzData$numTarget / lorenzData$numSum, 4)
  return(lorenzData)
}




############################# barLinePlotFunction.R
## plot of bar and line in the same page
## input- x, yBar, yLine, varName
barLinePlotFunction <- function(x, yBar, yLine, varName, target, addLineTrue){
  bar <- barplot(yBar, ylim=c(0, max(yBar)*1.2), 
                 main = paste(target, 'of', varName),
                 ylab="groupNum", 
                 col="grey", col.axis="black", col.lab="black")
  
  mtext(levels(x), side=1, line=1, at=bar, col="black")
  mtext(paste('varGroup of ', varName, sep = ''), side=1, line=3, col="black")
  
  par(new=T)
  plot(bar, yLine, axes=F,
       ylim=c(0, max(yLine)), xlab="", ylab="", col="red", type="b")
  axis(4, col="red", col.ticks="red", col.axis="red")
  
  if (addLineTrue == 'TRUE'){
    # 增加平均值水平线
    par(new=T)
    abline(h = mean(yLine), col = c("blue"))
  }
  
} 




############################# strSplitFunction.R
### R语言对测试结果明细数据做整理
### 支持一个字符拆分后存储为多行数据
require(dplyr)

txtSplitFunc <- function(.data, colName, resNames = NULL){
  text = .data[, colName]
  textMdy <- gsub('\\|', '\n', text)
  
  dfTmp <- read.csv(text = textMdy, sep = '#', header = FALSE,
                    stringsAsFactors = FALSE, colClasses = "character")
  if(!is.null(resNames)) names(dfTmp) <- resNames
  dfTmp
}

### example
#函数中'|' 和'#'均为固定的分隔符，需要根据情况修订
#dft <- data.frame(id = c(1,2), detail = c('2016-08-14#线上微额快速贷|2016-08-13#线下消费分期|2016-08-12#线下消费分期|2016-08-12#线上微额快速贷|'
#                                          , '2016-08-26#其他|2016-08-22#其他|2016-08-22#其他|2016-08-15#其他|2016-08-08#线上微额快速贷|2016-08-05#线上消费分期|2016-08-04#线上微额快速贷|2016-08-04#线上信用现金贷|2016-08-04#线下信用现金贷|')
#                  ,stringsAsFactors = FALSE)

#dfres <- dft %>% group_by(id) %>% do(txtSplitFunc(.,  colName = 'detail'))



############################# randomRorestFunction.R
## random forest based on input - rfData with target and variables

#outputSpace <- '~/dataEvaluation/compareBrYinlianJuxinli/rfResult'#评估结果输出位置
#target <- 'flagOverdueD30'
#outputName <- 'dataResource'
#numTree <- 200

randomForestFunction <- function(rfData, target, outputSpace, outputName, numTree) {
  library(randomForest)
  set.seed(2016)
  
  X <- rfData[, -which(colnames(rfData) == target)]
  y <- rfData[, target]
  
  X.RF = randomForest(x=X,
                      y=as.factor(as.character(y)),
                      ntree=numTree,
                      replace=TRUE)  ## OOB
  
  #print(X.RF)
  varImportance = as.data.frame(cbind(colnames(rfData)[c(2:ncol(rfData))]
                                      , importance(X.RF, type=2)))
  colnames(varImportance)[1] <- 'varName'
  
  varImportance[, 2] <- as.numeric(as.character(varImportance[, 2]))
  varImportance <- varImportance[order(-varImportance[, 2]), ]
  
  save(varImportance, file = paste(outputSpace, '/', target, '_',
                                   outputName, '_varImportance.RData', sep = ''))
  
  write.csv(varImportance, paste(outputSpace, '/', target, '_',
                                 outputName, '_varImportance.csv', sep = ''))
  
  pred = predict(X.RF, type="prob")
  
  return(pred)#预测值两列 以逾期为目标则取第二列的概率值，否则取第一列
}


##################################################################
############################# pcaRelatedFunction.R

## count variable numbers needed for certain cumulative importance from pca result
# 统计PCA输出的结果中 给定阈值时所需的成分数
varCount <- function(varImpact, cumProp){
  varNum <- numeric(0)
  for (i in 1:length(cumProp)){
    varNum[i] <- min(which(varImpact$varCumProp >= cumProp[i]))
  }
  return(varNum)
}


####### output the linear relations between pc and variables
outputVar <- function(orderedVar, numVar){#numVar - 每个成分中输出的相关变量个数
  descFunction <- character(0) #成分与变量的线性关系获取
  for (i in 1:numVar){ #前numVar个变量分别连接系数和变量名
    if (i == 1){ 
      # i <- 1
      descFunction[i] <- paste(names(orderedVar)[2], ' = ',
                               round(orderedVar[i, 2], 2),
                               '*',
                               orderedVar[i, 1],
                               sep = '')
    } else{
      if (i > 1 & orderedVar[i, 2] >= 0){
        descFunction[i] <- paste(' + ',
                                 round(orderedVar[i, 2], 2),
                                 '*',
                                 orderedVar[i, 1],
                                 sep = '')
      } else{
        if (i > 1 & orderedVar[i, 2] < 0){
          descFunction[i] <- paste(' - ',
                                   round(orderedVar[i, 3], 2),
                                   '*',
                                   orderedVar[i, 1],
                                   sep = '')
          
        }
        
      }
    }
    
  }
  
  outName <- character(0)
  for (i in 1:numVar){ #numVar个变量的线性关系拼接在一起
    outName <- paste(outName, descFunction[i], sep = '')
  } 
  
  return(outName)
}


############################# pcaOutputFunction.R
## PCA function
#input -dataframe 'analysisData' of target, variables
#output -single variable result of target and PCs

pcaOutputFunction <- function(analysisData, target, numVarGroup, numVar){
  #numVarGroup - num of varGroups
  #numVar - 各成分的单变量分析中 成分与变量的线性关系输出时的变量个数
  
  ### variables used for pca - delete target
  pcaData <- analysisData[, -which(colnames(analysisData) == target)]
  
  #install.packages("dummies")
  library(dummies)
  pcaData <- dummy.data.frame(pcaData)
  pcaData[is.na(pcaData)] <- 0
  
  ### remove var=0 将方差为0的列去掉
  pcaData <- pcaData[sapply(pcaData, var) !=0] 
  ### dedup variable 将相同的列去掉
  pcaData <- pcaData[!duplicated(lapply(pcaData, function(x)(x)))]
  
  ### 标准化
  scaleData <- as.data.frame(scale(pcaData))
  
  ################################### pca 
  #method 1: use princomp
  #pcaResult <- princomp(scaleData, cor = TRUE)
  
  #method 2: use prcomp
  pcaResult <- prcomp(scaleData, retx = TRUE, center = TRUE, scale. = TRUE)
  #pcaVar <- rownames(pcaResult$rotation)
  
  # impact of components
  varImpact <- summary(pcaResult)$importance
  varImpact <- as.data.frame(t(varImpact))
  names(varImpact) <- c("sd", "varProp", "varCumProp")
  
  # variables needed for certain cumulative inpact
  cumProp = seq(0.5, 1, 0.1)
  varNum <- varCount(varImpact, cumProp)#self defined function
  varNumSelect <- as.data.frame(cbind(cumProp, varNum))
  
  #选择的成分数量（解释90%以上）
  varNumChosen <- varNumSelect[varNumSelect$cumProp == 0.9, ]$varNum
  
  ## linear relations of pc and original variables
  pcaRotation <- as.data.frame(pcaResult$rotation)
  pcaRotation <- pcaRotation[, 1:varNumChosen]

  varSelect <- data.frame(cbind(colnames(pcaData), pcaRotation))
  colnames(varSelect)[1] <- 'varName'
  
  ####################### pca result analysis
  ### 计算各观测在成分上的取值
  pcaValues <- data.frame(as.matrix(scaleData) %*% as.matrix(pcaRotation))
  
  pcaAnalysis <- data.frame(cbind(analysisData[, target], pcaValues))
  names(pcaAnalysis)[1] <- target
  
  ### 成分与target的单变量分析
  for (i in c(2:ncol(pcaAnalysis))){
    ### 成分与变量的线性关系
    tempVar <- varSelect[, c(1, i)]
    absName <- paste("abs", names(varSelect)[2], sep = '')
    tempVar[, absName] <- abs(tempVar[, 2])
    
    tempVar <- tempVar[order(-tempVar[, absName]), ]
    orderedVar <- tempVar[order(-tempVar[, 3]), ]
    
    ### 成分与target的单变量分析
    varName <- colnames(pcaAnalysis)[i]
    tempData <- pcaAnalysis[, c(target, varName)]
    
    varGroup <- varGroupFunction(tempData, target, varName, numVarGroup)
    numCount <- numCountFunction(varGroup, target, varName)
    numCount <- ratioCalFunction(numCount)#add ratio
    numCount[is.na(numCount)] <- 0
    
    x <- numCount[, 1]
    yBar <- numCount$numSum
    yLine <- numCount$ratioInGroup
    
    barLinePlotFunction(x, yBar, yLine, varName, target, addLineTrue = 'TRUE')
    
    legend("topleft",
           legend=outputVar(orderedVar, numVar),
           #self defined function to output linear relation between PC and variables
           lwd=0.65, cex=0.75, col=c("red"), bty="n")
  }
}

