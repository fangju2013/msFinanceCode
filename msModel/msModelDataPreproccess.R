# package dependency: smbinning
# missing value regroup
# 数值型变量，缺失值处理标记函数，现在所有的缺失值全部标成一类
msDataPreproccessNumMiss <- function(input, nObs, missSet, tagPct, missTagMethod, missCnt = length(input)){
  list(FM1 = missSet)
}

# numeric variable data Preproccess function
# 数值型变量处理函数，用来统计变量给定分位点的取值，缺失率，缺失填充方法/取值，众数取值及比率
# 根据条件确定要增加的哑变量，上下届标记，缺失标记，众数标记
# 参数含义：trimPct，用于floor&cap的分位点；tagPct,在何种比例下增加哑变量；missSet，特定缺失集合；missTagMethod，缺失标记方法，暂不起作用；
#			missImpMethod，缺失填充方法
msDataPreproccessNum <- function(data, trimPct = 0.01, tagPct = 0.01
                             , missSet = c(999999972, 999999989)
                             , missTagMethod = c('each', 'group', 'all')
                             , missImpMethod = c('mean', 'median', 0)
                             , nObs = length(data)){
  missPart <- data[data %in% missSet]
  unMissPart <- data[!(data %in% missSet)]    
  flagList <- list()
  # quantile
  if(length(unMissPart) == 0){
    valueList <- list(missRate = 1)
  } else{
    cfValue <- quantile(unMissPart, probs = c(trimPct, 1 - trimPct), names = FALSE)#给定分位点
    # mode part
    freqTbl <- table(unMissPart)
    modePos <- which.max(freqTbl)[1]
    # floor & cap
    unMissPart <- pmin(pmax(unMissPart, cfValue[1]), cfValue[2])
    missImpValue <- if(missImpMethod == 0) 0 else do.call(missImpMethod, list(unMissPart))
    valueList <- list(LBValue = cfValue[1], UBValue = cfValue[2], missImp = missImpValue
                      , modeValue = as.numeric(names(modePos))
                      , missRate = length(missPart)/nObs
                      , modeRate = as.numeric(freqTbl[modePos]/nObs))

    if(sum(unMissPart <= cfValue[1])/nObs > tagPct) flagList$FLB <- cfValue[1]
    if(sum(unMissPart >= cfValue[2])/nObs > tagPct) flagList$FUB <- cfValue[2]
    if(freqTbl[modePos]/nObs > tagPct & !valueList$modeValue %in% cfValue) flagList$FSM <- valueList$modeValue
    
  }
  
  flagListMiss <- msDataPreproccessNumMiss(missPart, missSet = missSet, tagPct = tagPct, missTagMethod= missTagMethod)
  list(valueList = valueList, flagList = c(flagList, flagListMiss))
}



# code generator for numeric variable
# numeric variable flag prefix : FSM FLB FUB FM*
# variable front AMT/NUM/RAT/RTO/PB
# 根据数值型变量的处理结果，生成数据处理代码，包括缺失值、众数、上下届满足条件的标记，以及floor&cap代码和缺失值填充代码
# 参数：varName:需要生成代码的变量名；tagLists，所有数据型变量处理的list；missSet，缺失值集合；thrdIgnore，变量排除阈值，在缺失超过一定程度时不处理变量；
msDataGenCodeNum <- function(varName, tagLists, missSet, thrdIgnore = 0.99){
  tagList <- tagLists[[varName]]
  if(tagList$valueList$missRate > thrdIgnore) return(NULL)
  if(tagList$valueList$modeRate > thrdIgnore) return(NULL)
  
  # floor & cap code generate
  lowerBound <- tagList$valueList$LBValue
  upperBound <- tagList$valueList$UBValue
  missImput <- tagList$valueList$missImp
  missCode <- sprintf('%s[%s %%in%% c(%s)] <- %s', varName, varName, paste(missSet, collapse = ','), missImput)
  fAndCCode <- sprintf('%s <- pmax(%s, pmin(%s, %s))', varName, lowerBound, varName, upperBound)
  
  
  flagTagCodes <- sapply(names(tagList$flagList), function(flagPrefix, flagList, varName){
    flagValue <- flagList[[flagPrefix]]
    sprintf('%s_%s <- %s', flagPrefix, varName, if(length(flagValue) > 1) sprintf('(%s %%in%% c(%s)) + 0', varName, paste(flagValue, collapse = ','))
      else sprintf('(%s == %s) + 0', varName, flagValue))
  }, flagList = tagList$flagList, varName = varName)
  names(flagTagCodes) <- paste(names(flagTagCodes), varName, sep = '_')
  varDependency <- data.frame(varName = names(flagTagCodes), varDepend = varName, stringsAsFactors = FALSE)
  flagTagCodes[varName] <- paste(missCode, fAndCCode, sep = ';')
  
  list(dealCode = flagTagCodes, varDependency = varDependency)
}
# code excute sequence -- missing, cap&floor, other tag, transformation
# 合并分组函数，用于类别型变量，将多个类别合为更少的几个，并使用相应标识
# 参数：dfGrpStat，分组统计后的数据框，包括每组个数，目标均值，方差等；minPct，每组最小百分比，即每组量占总数的最小百分比；minCnt，每组最少数量
# maxGroup，最大组个数，即重新合并后组的最大个数；groupMethod，重分组方法，现在只支持t检验；thrdIdx， 分组合并阈值，现在根据t检验设置的置信度
# flagMiss，用于标识统计数据是否为缺失数据；missSet，缺失值集合， labelSeq，用于标记的字符序列
msDataRegroupNomGrp <- function(dfGrpStat, minPct, minCnt, maxGroup, groupMethod, thrdIdx
                    , flagMiss = FALSE, missSet, nObs, labelSeq = c(1:9, LETTERS)){
  # c('data', 'cnt', 'mn', 'var', 'targetLabel')
  if(nrow(dfGrpStat) == 0){
    if(flagMiss) return(data.frame(data = missSet, cnt = 0, mn = 0, var = 0, targetLabel = 'FM1', stringsAsFactors = FALSE))
    return(data.frame(data = character(0), cnt = integer(0), mn = numeric(0), var = numeric(0)
                      , targetLabel = character(0), stringsAsFactors = FALSE))
  }
  # 均值降序排
  dfGrpStat <- dfGrpStat[order(dfGrpStat$mn, decreasing = TRUE), ]
  dfGrpStat$pct <- dfGrpStat$cnt/nObs
  dfGrpStat$remainPct <- sum(dfGrpStat$pct) - cumsum(dfGrpStat$pct)
  # proccess small volumn category
  # isLast cntGroupNo cntGroupCnt cntGroupMn cntGroupVar
  numCategory <- nrow(dfGrpStat)
  # 增加临时列，用于后面分组
  dfCntGrp <- within(dfGrpStat, {
    var[is.na(var)] <- 0
    cntIsLast <- FALSE
    cntGroupNo <- 0
    cntGroupCnt <- 0
    cntGroupMn <- 0
    cntGroupVar <- 0
    rawGroupSum <- cnt*mn
    rawGroupVarSum <- var*(cnt - 1)
  })
  
  # cntGroupNoTmp cntGroupCntTmp cntGroupMnTmp cntGroupVarTmp
  cntGroupNoTmp <- 1; cntGroupCntTmp <- 0; cntGroupPctTmp <- 0; cntGroupMnTmp <- 0; cntGroupVarTmp <- 0; cntGroupPctTmp <- 0
	#根据每组数量级百分比合并分组
  for( i in 1:numCategory){
    dfCntGrp[i, 'cntGroupNo'] <- cntGroupNoTmp
    
    cntGroupMnTmp <- (cntGroupMnTmp*cntGroupCntTmp + dfCntGrp[i, 'rawGroupSum'])/(cntGroupCntTmp + dfCntGrp[i, 'cnt'])
    cntGroupVarTmp <- (cntGroupVarTmp*(cntGroupCntTmp - 1) + dfCntGrp[i, 'rawGroupVarSum'])/(max(cntGroupCntTmp + dfCntGrp[i, 'cnt'] - 2, 1))
    cntGroupCntTmp <- cntGroupCntTmp + dfCntGrp[i, 'cnt']
    cntGroupPctTmp <- cntGroupPctTmp + dfCntGrp[i, 'pct']
    
    if((cntGroupCntTmp > minCnt & cntGroupPctTmp > minPct & dfCntGrp[i, 'remainPct'] > minPct)| i == numCategory){
      dfCntGrp[i, 'cntIsLast'] <- TRUE
      dfCntGrp[i, 'cntGroupCnt'] <- cntGroupCntTmp
      dfCntGrp[i, 'cntGroupMn'] <- cntGroupMnTmp
      dfCntGrp[i, 'cntGroupVar'] <- cntGroupVarTmp
      
      cntGroupNoTmp <- cntGroupNoTmp + 1
      cntGroupCntTmp <- 0
      cntGroupPctTmp <- 0
      cntGroupMnTmp <- 0
      cntGroupVarTmp <- 0
    }
  }
  
  # combine group by certain method, only t-test for now
  dfGrpedTBC <- dfCntGrp[dfCntGrp$cntIsLast, c('cntGroupNo', 'cntGroupCnt', 'cntGroupMn', 'cntGroupVar')]
  dfGrpedTBC[dfGrpedTBC$cntGroupMn==0,'cntGroupMn']=0.2  #我手工修改，预防为0，然后后面报错，调试中发现0.2-0.5都能正确区分没有逾期的分类，0.1以下总有区分不合理的，这个以后观察调试
  dfGrpedTBC[dfGrpedTBC$cntGroupVar==0,'cntGroupVar']=0.2 #同上
  
  numGrpMid <- nrow(dfGrpedTBC)
  if(numGrpMid == 1){
    dfGrpedTBC <- data.frame(cntGroupNo = 1, mtdGrpNo = 1)
  }else{
    # mtdGroupNoTmp mtdGroupCntTmp mtdGroupMnTmp mtdGroupVarTmp
    dfGrpedTBC$mtdIsLast <- TRUE
    dfGrpedTBC$mtdGrpNo <- 1
    mtdGroupNoTmp <- 1
    mtdGroupCntTmp <- dfGrpedTBC[1, 'cntGroupCnt']
    mtdGroupMnTmp <- dfGrpedTBC[1, 'cntGroupMn']
    mtdGroupVarTmp <- dfGrpedTBC[1, 'cntGroupVar']
	# t检验合并分组
    for(i in 2:numGrpMid){
      dfGrp <- mtdGroupCntTmp + dfGrpedTBC[i, 'cntGroupCnt'] - 2
      combineVar <- (mtdGroupVarTmp*(mtdGroupCntTmp - 1) + dfGrpedTBC[i, 'cntGroupVar']*(dfGrpedTBC[i, 'cntGroupCnt'] - 1))/dfGrp
      diffT <- (mtdGroupMnTmp - dfGrpedTBC[i, 'cntGroupMn'])/sqrt(combineVar*(1/mtdGroupCntTmp + 1/dfGrpedTBC[i, 'cntGroupCnt']))
      
      if(pt(abs(diffT), dfGrp) < 1 - thrdIdx){
        dfGrpedTBC[i - 1, 'mtdIsLast'] <- FALSE
        mtdGroupMnTmp <- (mtdGroupMnTmp*mtdGroupCntTmp + dfGrpedTBC[i, 'cntGroupMn']*dfGrpedTBC[i, 'cntGroupCnt'])/(dfGrp + 2)
        mtdGroupVarTmp <- combineVar
        mtdGroupCntTmp <- dfGrp + 2
      } else {
        mtdGroupNoTmp <- mtdGroupNoTmp + 1
        
        mtdGroupCntTmp <- dfGrpedTBC[i, 'cntGroupCnt']
        mtdGroupMnTmp <- dfGrpedTBC[i, 'cntGroupMn']
        mtdGroupVarTmp <- dfGrpedTBC[i, 'cntGroupVar']
      }
      dfGrpedTBC[i, 'mtdGrpNo'] <- mtdGroupNoTmp
    }
  }
  
  labelPrefix <- if(flagMiss) 'FM' else 'FX'
  dfGrpedTBC$targetLabel <- paste0(labelPrefix, labelSeq[dfGrpedTBC$mtdGrpNo])
  
  dfLabelTag <- merge(dfCntGrp, dfGrpedTBC[, c('cntGroupNo', 'targetLabel')])
  dfLabelTag[, c('data', 'cnt', 'mn', 'var', 'targetLabel')]
}

# categorical variable regroup by using IV/chi-square/t-test: need levels?
# 类别型，变量分组函数，输入为变量及目标取值
# 参数：data，变量列；target，目标列；minPct，最小百分比；minCnt，样本最小数量；maxGroup，最大重分组个数；missSet，缺失值集合
#     groupMethod，重分组方法；thrdIdx，分组方法对应阈值； missGrpPrin，针对缺失值采取的重分组策略；
msDataRegroupNominal <- function(data, target, minPct = 0.05, minCnt = 30, maxGroup = 20
                                 ,missSet = c('999999972', '999999988', '999999989')
                                 ,groupMethod = c('ttest', 'chi', 'iv'), thrdIdx = 0.025
                                 ,missGrpPrin = c('normal', 'each', 'group', 'all')
                                 ,nObs = length(data)){
  # group count, mean, variance
  dfGrpCntMn <- aggregate(target ~ data, FUN = function(x) c(cnt = length(x), mn = mean(x), var = var(x)))
  dfGrpCntMn <- cbind(dfGrpCntMn, dfGrpCntMn[, 'target'])
  isMiss <- dfGrpCntMn$data %in% missSet
  listValue <- list(missRate = sum(dfGrpCntMn[isMiss, 'cnt'])/nObs)
  if(missGrpPrin == 'normal') return(list(mapDict = msDataRegroupNomGrp(dfGrpCntMn, minPct, minCnt, maxGroup, groupMethod, thrdIdx, flagMiss = FALSE, missSet, nObs)
                                          ,listValue = listValue))
  # seperated regroup for miss and unmiss part
  dfAggMissPart <- dfGrpCntMn[isMiss, ]
  missMapDict <- msDataRegroupNomGrp(dfAggMissPart, minPct, minCnt, maxGroup, groupMethod, thrdIdx, flagMiss = TRUE, missSet, nObs)
  
  unMissMapDict <- msDataRegroupNomGrp(dfGrpCntMn[!isMiss, ], minPct, minCnt, maxGroup, groupMethod, thrdIdx, flagMiss = FALSE, missSet, nObs)
  # merge result
  list(mapDict = rbind(unMissMapDict, missMapDict), listValue = listValue)

}

# code generator for categorical variable
# numeric variable flag prefix : FX* FM*
# variable front AMT/NUM/RAT/RTO/PB
# 'data', 'cnt', 'mn', 'var', 'targetLabel'
# 字符型变量处理代码生成函数，针对单个变量，生成此变量的哑变量标记代码
# 参数： varName，变量名；tagLists，所有类别型变量重分组后的标记；thrdIgnore，确实比例忽略阈值；
msDataGenCodeCat <- function(varName, tagLists, thrdIgnore = 0.99){
  mapDict <- tagLists[[varName]]$mapDict
  if(max(mapDict$cnt)/sum(mapDict$cnt) > thrdIgnore) return(NULL)
  
  mapPartCode <- sprintf('%s <- dictRGMap%s[%s]', varName, varName, varName)
  newComeCode <- sprintf('%s[is.na(%s)] <- ""', varName, varName)
  
  tagVarPrefix <- unique(mapDict$targetLabel)
  flagTagCodes <- sapply(tagVarPrefix, function(prefix, varName) sprintf('%s_%s <- (%s == "%s") + 0', prefix
                                                                        , varName, varName, prefix)
                        , varName = varName)
  names(flagTagCodes) <- paste(tagVarPrefix, varName, sep = '_')
  varDependency <- data.frame(varName = names(flagTagCodes), varDepend = varName, stringsAsFactors = FALSE)
  flagTagCodes[varName] <- paste(mapPartCode, newComeCode, sep = ';')
  
  list(dealCode = flagTagCodes, varDependency = varDependency)
}

# extract generated code from the list/ return the vector
# cat(paste(res[1:4], collapse = '\n'))
# 从生成的代码list中提取并合为named vector
msDataCodeExtract <- function(codeVecLst){
  codeLstPure <- lapply(codeVecLst, '[[', 'dealCode')
  do.call('c', codeLstPure)
}

# extract variable dependency data frame 
# 提取处理后变量依赖关系的函数
msDataVarDependExtract <- function(dependVarLst){
  if(!is.null(dependVarLst$varDependency)) return(dependVarLst$varDependency)
  dependDfLst <- lapply(dependVarLst, '[[', 'varDependency')
  do.call('rbind', dependDfLst)
}

# map dict generator for categorical variable
# 用于生成类别型变量的重分组映射字典
# 参数: 类别型变量处理后的标记列表
msDataMapDict <- function(valueLst){
  mapDictLst <- lapply(valueLst, '[[', 'mapDict')
  
  mapDictVectors <- lapply(mapDictLst, function(statDesc){
    targetValue <- statDesc$targetLabel
    names(targetValue) <- statDesc$data
    targetValue
  })
  
  for(varName in names(mapDictVectors)){
    assign(paste0('dictRGMap', varName), mapDictVectors[[varName]], envir = parent.frame(1))
  }
  NULL
}

# variable transformation
# atomic transformation function
# 变量变换的原子函数
msModelTransfInverseAtom <- function(x) 1/(1+x)
msModelTransfSquareAtom <- function(x) x^2
msModelTransfLogAtom <- function(x) log(1+x)
msModelTransfSqrtAtom <- function(x) sqrt(x)

# return a dataframe by giving certain dataframe and specific transformation method
# 针对给定变换的所有变量统一变换
# 参数： df，输入自变量数据框；transfType，变量类型
msModelDataTansform <- function(df, transfType = c('IV', 'SR', 'SQ', 'LN')){
  res <- switch(transfType,
                IV = lapply(df, msModelTransfInverseAtom),
                SR = lapply(df, msModelTransfSquareAtom),
                SQ = lapply(df, msModelTransfSqrtAtom),
                LN = lapply(df, msModelTransfLogAtom))
  names(res) <- paste(transfType, names(res), sep = '_')
  as.data.frame(res)
}
# for data input , return all transformed data specified by transform type
# 针对给定自变量数据集，做四种常规变换
# 参数：df，自变量数据集；transfType，变量变换向量
msModelDataTansformPack <- function(df, transfType = c('IV', 'SR', 'SQ', 'LN')){
  transformedDf <- list()
  varDependLst <- list()
  for(type in transfType){
    transformedDf[[type]] <- msModelDataTansform(df, type)
    varDependLst[[type]] <- data.frame(varName = names(transformedDf[[type]]), varDepend = names(df), stringsAsFactors = FALSE)
  }
  names(transformedDf) <- NULL
  list(data = do.call('cbind', transformedDf), varDependency = do.call('rbind', varDependLst))
}

# variable evluate part: for each index
# only for numeric variable Jensen–Shannon divergence
# 针对数值型变量以及指定分组情况下返回对称KL距离的函数
# 参数： x，自变量；y，因变量；method，分组方式；maxcat，最大分组个数；minCnt，每组最少个数；minPct，每组最小比例
msModelGetJSDNum <- function(x, y, method = c('even', 'smbinning'), maxcat = 10, minCnt = 30, minPct = 0.05
                          ,nobs = length(x), missSet = c(999999971:999999972, 999999980:999999989)){
  missLabel <- x %in% missSet
  nMiss <- sum(missLabel)
  if(nMiss == nobs) return(NULL)
  
  subMinPct <- max(minPct*nobs/(nobs - nMiss), minCnt/(nobs - nMiss))
  if(subMinPct < 0.5){
    if(method == 'smbinning'){
      # if(length(x[!missLabel]) == 0) return(NULL)
      resTmp <- smbinning(data.frame(x=x[!missLabel], y=y[!missLabel]), x = 'x', y = 'y', p = subMinPct)
      cuts <- if(is.list(resTmp)) resTmp$cuts else min(x[!missLabel])
    }else{
      maxGroup <- floor(1/subMinPct)
      cuts <- unique(quantile(x[!missLabel], seq(0, 1, 1/maxGroup)[-c(1, maxGroup + 1)]))
    }
  }else{
    cuts <- min(x[!missLabel])
  }
  # cat(deparse(substitute(x)), '\n')
  # return(length(cuts))
  cutted <- if(length(unique(x[!missLabel])) == 1) as.factor(x[!missLabel]) else cut2(x[!missLabel], cuts = cuts)
  levelCut <- c(levels(cutted), unique(x[missLabel]))
  
  x[!missLabel] <- as.character(cutted)
  res <- msModelGetJSDCat(x,y)
  c(res, cuts = cuts, levels = levelCut)
}
# Jensen–Shannon divergence for categoric variable
# 类别型变量对称KL距离计算函数
msModelGetJSDCat <- function(x, y){
  res <- data.table(x=x,y=y, key = 'x')[, list(grpCnt = length(y), posCnt = sum(y), negCnt = sum(1-y)), by = 'x']
  names(res) <- c('disInterval', 'grpCnt', 'posCnt', 'negCnt')
  totPos <- sum(res$posCnt); totNeg <- sum(res$negCnt)
  res <- within(res, {
    posRate <- posCnt/(posCnt + negCnt)
    posPct <- posCnt/totPos
    negPct <- negCnt/totNeg
    WOE <- log(negPct/posPct)
    IV <- (negPct - posPct) * WOE
  })
  list(IV = sum(res$IV[!is.infinite(res$IV)]), detail = res)
}



