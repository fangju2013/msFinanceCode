
################################# 数据预处理
#将'~/edsEvaluation/modelCode/2 dataProcess.R'中的处理定义为函数
  #函数内层不能直接source 否则报错 - 要么跑原代码 要么定义为函数（dataDealFunction正是为了代替 dataProcess 的功能）

#入参analysisData，返回 dfModelAllX - 后面需要的建模数据
#其中return的数据可以根据实际需要选择 dfModelAllX, train_data, test_data等

dataDealFunction <- function(analysisData){
  
  #############################################################################
  ### 流程化代码 - 数据预处理 - 传入analysisData（确保变量类别正确即可）
  #############################################################################
  
  require(Hmisc)
  require(data.table)
  require(car)
  require(parallel)
  require(ROCR)
  require(gbm)
  require(caret)
  require(smbinning)
  require(ggplot2)
  require(tools)
  require(dplyr)
  
  
  ########################################### 数据预处理-流程化部分
  ###数据处理
  dfModelInputData <- analysisData
  dfModelInputData[,targetVar]=sapply(dfModelInputData[,targetVar],as.numeric)
  
  #区分训练集和验证集
  a=0.8 #训练集的比例
  dfModelInputData$int=sample(2, nrow(dfModelInputData), replace = T, prob = c(a,1-a))
  dfModelInputData$flagSplit=ifelse(dfModelInputData$int==1,'train','test')
  dfModelInputData$int=NULL
  
  #每个客户打标签
  dfModelInputData$no1=1
  dfModelInputData$no=cumsum(dfModelInputData$no1)
  dfModelInputData$no1=NULL
  train_no=dfModelInputData[dfModelInputData$flagSplit=='train','no']
  test_no=dfModelInputData[dfModelInputData$flagSplit=='test','no']
  
  #缺失值处理
  dfModelInputData[is.na(dfModelInputData) | is.null(dfModelInputData) | dfModelInputData=='']=999999971
  
  #区分训练集和验证集
  trainLabel <- (dfModelInputData[, 'flagSplit'] == 'train')
  testLabel <- (dfModelInputData[, 'flagSplit'] == 'test')
  
  #挑选进入模型的x和y
  dfModelInputData[,targetVar]=as.integer(dfModelInputData[,targetVar])
  names=colnames(dfModelInputData)
  names=setdiff(names,c(targetVar,'flagSplit'))
  
  dfModelAllX=dfModelInputData[,names] ###全部样本数据的X变量部分
  targetLabelAll <- dfModelInputData[, targetVar] ###全部样本数据的y变量部分
  
  # variable/model selection threshold 变量和模型的各种阈值
  psiThrd <- 0.125; missSetGeneral <- c(999999971:999999972, 999999980:999999989);
  regroupThrd <- 0.05; regroupMinPct <- 0.03;
  maxRRpt <- 10; maxRNumPerLoop <- 3; maxRVif <- 4; maxREvalIdx <- 'ks';
  
  # variable partition 分出分类和数字型变量
  varsChr <- names(dfModelAllX)[sapply(dfModelAllX, class) == 'character']
  varsFactor <- names(dfModelAllX)[sapply(dfModelAllX, class) == 'factor']
  varsNum <- names(dfModelAllX)[sapply(dfModelAllX, class) %in% c('integer', 'numeric')]
  #把全部是缺失值的暂不用进入下面的psi
  aaa=sapply(dfModelAllX[,varsNum],min)
  varsNum_chr=names(aaa[aaa>999999900]) #这种情况全是缺失值，我改为分类变量
  #varsNum_max<- names(dfModelAllX)[sapply(dfModelAllX[,varsNum],min) >9999999]
  varsNum=setdiff(varsNum,varsNum_chr)
  dfModelAllX[,varsNum]=sapply(dfModelAllX[,varsNum],as.numeric)
  
  dfModelAllX[, varsFactor] <- lapply(dfModelAllX[, varsFactor], as.character)
  dfModelAllX[, varsNum_chr] <- lapply(dfModelAllX[, varsNum_chr], as.character)
  varsChr <- c(varsChr, varsFactor,varsNum_chr)
  
  
  # variable selection by PSI 去除psi高的
  #options(sqldf.driver= 'SQLite') #smbinning会调用一个包，里面有一个数据库引擎，如果y=as.integer()话就要调用这个包
  library(smbinning)
  psiNumVar <- lapply(dfModelAllX[trainLabel|testLabel, varsNum], msModelGetJSDNum
                      , y = (testLabel[trainLabel|testLabel]) ,method='smbinning')
  psiNumVarValue <- sapply(psiNumVar, '[[', 'IV')
  varsNum_temp=names(psiNumVarValue[psiNumVarValue > psiThrd])
  varsNum <- setdiff(varsNum, varsNum_temp)
  varsNum <- setdiff(varsNum, 'no')
  
  psiCatVar <- lapply(dfModelAllX[trainLabel|testLabel, varsChr], msModelGetJSDCat
                      , y = testLabel[trainLabel|testLabel])
  psiCatVarValue <- sapply(psiCatVar, '[[', 'IV')
  varsChr_temp=names(psiCatVarValue[psiCatVarValue > psiThrd])
  varsChr <- setdiff(varsChr,varsChr_temp)
  dfModelAllX=dfModelAllX[,c(varsNum,varsChr)]
  
  
  #最后合并为两个varsChr和varsNum
  # data preprocess     # variable tag
  #数据源\转换，只是记录下各个节点的数据，包括最大值最小值缺失值，转换在后面
  varTagLstNum <- lapply(dfModelAllX[trainLabel, varsNum], msDataPreproccessNum, missSet = missSetGeneral
                         , missTagMethod = 'all' , missImpMethod = 'median')
  
  varTagLstChr=NULL
  if (length(varsChr)>1) {
    varTagLstChr <- lapply(dfModelAllX[trainLabel, varsChr], msDataRegroupNominal, target = targetLabelAll[trainLabel], minPct = regroupMinPct
                           , missSet = missSetGeneral, groupMethod = 'ttest', thrdIdx = regroupThrd, missGrpPrin = 'all')
  }
  if (length(varsChr)==1){
    aaa=dfModelAllX[trainLabel, varsChr]
    aaa=data.frame(aaa)
    names(aaa)=varsChr
    aaa[,1]=as.character(aaa[,1])
    varTagLstChr <- lapply(aaa, msDataRegroupNominal, target = targetLabelAll[trainLabel], minPct = regroupMinPct
                           , missSet = missSetGeneral, groupMethod = 'ttest', thrdIdx = regroupThrd, missGrpPrin = 'all') 
  } 
  if (length(varsChr)==0){varTagLstChr=NULL}
  
  
  #记录下,后面程序不需要
  varDescStatsTagLst <- list(num = varTagLstNum, chr = varTagLstChr)
  #save(varDescStatsTagLst, file = paste(workSpace, '/outputData/', tempName, '_dataVariableDescStatsAndTag.RData', sep = ''))
  
  
  # tag list/varTagLstNum varTagLstChr
  # generate code  #生成跑批代码
  # 根据数值型变量的处理结果，生成数据处理代码，包括缺失值、众数、上下届满足条件的标记，以及floor&cap代码和缺失值填充代码
  varCodeLstNum <- lapply(names(varTagLstNum), msDataGenCodeNum, tagLists = varTagLstNum, missSet = missSetGeneral)
  varCodeLstChr <- lapply(names(varTagLstChr), msDataGenCodeCat, tagLists = varTagLstChr)
  #把dealcode里面的内容，拼成一个大向量# 从生成的代码list中提取并合为named vector
  varCodeVecNum <- msDataCodeExtract(varCodeLstNum)
  varCodeVecChr <- msDataCodeExtract(varCodeLstChr)
  
  # categorical variable recode generate
  #分别找出缺失值所在的位置
  varCodeNumMissPos <- grep('^FM[1-9A-Z]_', names(varCodeVecNum))
  varCodeNumOtagPos <- which(substr(names(varCodeVecNum), 1, 4) %in% c('FLB_', 'FUB_', 'FSM_'))
  varCodeNumMiss <- varCodeVecNum[varCodeNumMissPos]
  varCodeNumOtag <- varCodeVecNum[varCodeNumOtagPos]
  varCodeNumFCap <- varCodeVecNum[-c(varCodeNumMissPos, varCodeNumOtagPos)]
  
  # numerical variable recode generate
  varCodeCatTagPos <- grep('^F[MX][1-9A-Z]_', names(varCodeVecChr))
  varCodeCatTag <- varCodeVecChr[varCodeCatTagPos]
  varCodeCatMap <- varCodeVecChr[-varCodeCatTagPos]
  msDataMapDict(varTagLstChr)
  
  #记录下，未来模型上线用
  recodeLst <- list(varCodeNumMiss = varCodeNumMiss, varCodeNumFCap = varCodeNumFCap, varCodeNumOtag = varCodeNumOtag
                    , varCodeCatMap = varCodeCatMap, varCodeCatTag = varCodeCatTag)
  #save(recodeLst, file = paste(workSpace, '/outputData/', tempName, '_recodeLst.RData', sep = ''))
  #save(varTagLstChr, file = paste(workSpace, '/outputData/', tempName, '_varTagLstChr.RData', sep = ''))
  
  #利用上面的代码做数据处理#在后面数据里面执行eval的代码
  dfModelAllX <- within(dfModelAllX, {
    eval(parse(text = paste0(varCodeNumMiss, collapse = '\n')))   #修改缺失值为0
    eval(parse(text = paste0(varCodeNumFCap, collapse = '\n')))   #增加flb,fub,fsm的哑变量
    eval(parse(text = paste0(varCodeNumOtag, collapse = '\n')))   #把极大、极小值修改
    
    eval(parse(text = paste0(varCodeCatMap, collapse = '\n')))    #缺失值修改为0
    eval(parse(text = paste0(varCodeCatTag, collapse = '\n')))    #增加分类的哑变量
  })   
  
  # remove categorcal variables去掉分类变量,数值型变量不用去
  dfModelAllX <- dfModelAllX[, setdiff(names(dfModelAllX), c(varsChr))]
  
  #去掉变异系数小、严重不平衡的数据、相关性高、共线性高的
  dfModelAllX=var_wipe(dfModelAllX,label=trainLabel,cv_number=0.05) 
  dfModelAllX=ZeroVar_wipe(dfModelAllX,label=trainLabel,ZeroVar_number=98/2) 
  dfModelAllX=cor_wipe(dfModelAllX,label=trainLabel,cor_number=0.9) 
  dfModelAllX=coll_wipe(dfModelAllX,label=trainLabel) 
  
  dfModelAllX$no=dfModelInputData$no #no加进去
  
  #做数值型的变量转换，然后跟其他合并
  # grep('^FX[1-9A-Z]_', names(dfModelProcessedX))
  # single variable transformation    #做开方等转换
  varTransformSingle <- intersect(varsNum, names(dfModelAllX))
  varsMin <- sapply(dfModelAllX[, varTransformSingle], min)
  varTransformSingle <- setdiff(varTransformSingle, names(varsMin[varsMin < 0])) #小于0的不做各种转换
  if (length(varTransformSingle)>1) {varTransformed <- msModelDataTansformPack(dfModelAllX[, varTransformSingle])} 
  if (length(varTransformSingle)==1){bbb=as.data.frame(dfModelAllX[, varTransformSingle]);names(bbb)=varTransformSingle;varTransformed <- msModelDataTansformPack(bbb)}
  if (length(varTransformSingle)==0){varTransformed=NULL}
  aaa=varTransformed$data
  # correlation & colinear treatment/ caret  去掉变异系数小、严重不平衡的数据、相关性高、共线性高的
  aaa=var_wipe(aaa,label=trainLabel,cv_number=0.05) 
  aaa=ZeroVar_wipe(aaa,label=trainLabel,ZeroVar_number=98/2) 
  aaa=cor_wipe(aaa,label=trainLabel,cor_number=0.9) 
  aaa=coll_wipe(aaa,label=trainLabel)
  if (!is.null(aaa)) {dfModelAllX <- cbind(dfModelAllX,aaa)}
  
  #记录下，未来模型上线用
  #save(varTransformSingle, file = paste(workSpace, '/outputData/', tempName, '_varTransformSingle.RData', sep = ''))
  
  # interact variable treatment/ need to complete              #做标记
  # variable dependency list: varTransformed$varDependency varCodeLstChr[[i]]$varDependency varCodeLstNum[[i]]$varDependency
  varDependencyLst <- lapply(list(numDepend = varCodeLstNum, chrDepend = varCodeLstChr, singleTrans = varTransformed)
                             , msDataVarDependExtract)
  #save(varDependencyLst, file = '~/outputData/dataVariableDependency.RData', compress = TRUE)
  varPreprocessInfo <- list(recodeLst = recodeLst, varDependency = varDependencyLst, varTransformSingle = varTransformSingle, varTransformInteract = NULL) 
  #save(varPreprocessInfo, file = sprintf('~/outputData/dataVariablePreprocessInfo.RData', workSpace), compress = TRUE)
  
  dfModelAllX[, targetVar] <- targetLabelAll     #把y加进去
  train_data <- dfModelAllX[trainLabel,]  #抽出训练集
  test_data <- if(sum(testLabel) > 0) dfModelAllX[testLabel,] else NULL #抽出验证集
  
  return(dfModelAllX) ###输出后面建模需要的数据
  
  # save(dfModelInputData, file = paste(workSpace, '/outputData/', tempName, '_dfModelInputData.RData', sep = ''))
  # save(dfModelAllX, file = paste(workSpace, '/outputData/', tempName, '_dfModelAllX.RData', sep = ''))
  # save(train_data, file = paste(workSpace, '/outputData/', tempName, '_train_data.RData', sep = ''))
  # save(test_data, file = paste(workSpace, '/outputData/', tempName, '_test_data.RData', sep = ''))
  # save(targetVar, file = paste(workSpace, '/outputData/', tempName, '_targetVar.RData', sep = ''))
  # save(trainLabel, file = paste(workSpace, '/outputData/', tempName, '_trainLabel.RData', sep = ''))
  # save(testLabel, file = paste(workSpace, '/outputData/', tempName, '_testLabel.RData', sep = ''))
  # 
}