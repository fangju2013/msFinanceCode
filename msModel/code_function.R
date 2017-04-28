#变量转换，按照相应的区间
blzh <- function(qh_score_model_woe, qh_model_woe,trans_bz='WoE',name='name'){
  for ( i in 1:ncol(qh_score_model_woe)) {
    dftemp1 <- unique(data.frame(qh_score_model_woe[names(qh_score_model_woe)[i]]))
    names(dftemp1) <- 'aaa'
    dftemp2 <- qh_model_woe[qh_model_woe[,name] == names(qh_score_model_woe)[i], ]
    if(nrow(dftemp2) == 0){
      qh_score_model_woe <- qh_score_model_woe
    }else{
      fun <- function(x){
        max(dftemp2[x >= dftemp2$begin, 'begin'])
              }
      dftemp11 <- as.data.frame(apply(dftemp1, 1, FUN = fun))
      names(dftemp11) <- 'begin'
      dftemp11 <- cbind.data.frame(dftemp1, dftemp11)
      dftemp11 <- merge(dftemp11, dftemp2, all.x =TRUE, by = 'begin')
      dftemp22 <- dftemp11[c('aaa',trans_bz)]
      names(dftemp22) <- c(names(qh_score_model_woe)[i], paste(names(qh_score_model_woe)[i], trans_bz, sep = '_'))
      qh_score_model_woe <- merge(qh_score_model_woe, dftemp22,all.x =T)
    }
  }
  #qh_score_model_woe1=qh_score_model_woe[,names(qh_score_model_woe)]
  qh_score_model_woe1=qh_score_model_woe[,which(names(qh_score_model_woe)==targetVar):length(names(qh_score_model_woe))]
  qh_score_model_woe1$targetVar=qh_score_model_woe$targetVar
  if (length(qh_score_model_woe$no)>0) {qh_score_model_woe1$no=qh_score_model_woe$no}
  name=names(qh_score_model_woe1)
  for ( i in 1:length(name)) {
    name[i]=sub(paste0('_',trans_bz),'',name[i])
    names(qh_score_model_woe1)[i]=name[i]
    i=i+1
  }
  return(qh_score_model_woe1)
}   

#计算woe和iv
compute_woe_iv<- function(dfModelAllX1, varnames_score,end=9999999999){
  model_woe =data.frame(name = 0,Cutpoint=0,CntRec=0,CntGood=0, CntBad=0,GoodRate=0, BadRate=0,WoE = 0,IV=0,begin = 0,end=0,no=0,no1=0)
  model_woe = model_woe[-1,] 
  no_use=c()                    
  less_bl=c()
  library(smbinning)
  options(sqldf.driver= 'SQLite')
  for  (i in 1:(length(varnames_score)-1))
  { 
    if(length(table(dfModelAllX1[,varnames_score[i]]))==2 & min(dfModelAllX1[,varnames_score[i]])==0
       & max(dfModelAllX1[,varnames_score[i]])==1)  #如果是只有0和1的分类变量
    {
      dfModelAllX1[,varnames_score[i]]<-as.factor(as.character(dfModelAllX1[,varnames_score[i]]))
      result=smbinning.factor(df=dfModelAllX1,y=targetVar,x=varnames_score[i])
      dfModelAllX1[,varnames_score[i]]<-as.numeric(as.character(dfModelAllX1[,varnames_score[i]]))
    }  else if (length(table(dfModelAllX1[,varnames_score[i]]))<10) #如果是数值变量，但类别少于10，要增加随机值 
    {
      suiji=abs(rnorm(nrow(dfModelAllX1),0,mean(dfModelAllX1[,varnames_score[i]])/1000000))
      dfModelAllX1[,varnames_score[i]]=dfModelAllX1[,varnames_score[i]]+suiji
      result=smbinning(df=dfModelAllX1,y=targetVar,x=varnames_score[i],p=0.05)
      # dfModelAllX1[,varnames_score[i]]=as.factor(dfModelAllX1[,varnames_score[i]])
      #  result=smbinning.factor(df=dfModelAllX1,y=targetVar,x=varnames_score[i])
      less_bl=append(less_bl, varnames_score[i])
    } else  #以上两种都不满足，即是正常的数值
    {result=smbinning(df=dfModelAllX1,y=targetVar,x=varnames_score[i],p=0.05)}
    
    if(is.list(result)){
      aaa=result$ivtable
      aaa$end=aaa$Cutpoint
      aaa$begin[1]=-10000000
      for (a in 1:(nrow(aaa)-3)) {
        aaa$begin[a+1]=aaa$Cutpoint[a]
        a=a+1
      }
      name=setdiff(names(model_woe),c('name','no','no1'))
      aaa=cbind(name=rep(varnames_score[i],(nrow(aaa)-2)),aaa[1:(nrow(aaa)-2),name])
      aaa$begin=chartr(old="<=",new='  ', aaa$begin)
      aaa$begin=chartr(old="'",new='  ', aaa$begin)
      aaa$end=chartr(old="<=",new='  ', aaa$end)
      aaa$end=chartr(old=">",new='  ', aaa$end) #是最近发现cutpoint格式变了新加的，多了>
      aaa$end=chartr(old="'",new='  ', aaa$end)
      aaa$no=i
      aaa$no1=cumsum(aaa[, 'no'])/i
      aaa[, 'end'] <-as.numeric(aaa[, 'end'])
      aaa[, 'begin'] <-as.numeric(aaa[, 'begin'])
      aaa$begin=aaa$begin+0.000001
      # aaa=cbind(name=rep(varnames_score[i],(nrow(aaa)-2)),aaa[1:(nrow(aaa)-2),names(model_woe)[-1]])
      #以下两行是最近发现cutpoint格式变了新加的，多了>
      dfModelAllX1[,varnames_score[i]]=as.numeric(dfModelAllX1[,varnames_score[i]])
      max=max(dfModelAllX1[,varnames_score[i]])
      if (max>end)  {aaa[aaa$end<aaa$begin,'end']=max} else {aaa[aaa$end<aaa$begin,'end']=end}
      model_woe=rbind(model_woe,aaa)
    } else{ no_use=append(no_use, varnames_score[i])}#记录下哪些变量没有被选中
    i=i+1
  }
  return(model_woe)
}  

# 看每个变量的影响程度
relweights=function(model_name,...){
  R=cor(model_name$model)
  nvar=ncol(R)
  rxx=R[2:nvar,2:nvar]
  rxy=R[2:nvar,1]
  svd=eigen(rxx)
  evec=svd$vectors
  ev=svd$values
  delta=diag(sqrt(ev))
  lambda=evec %*% delta %*% t(evec)
  lambdasq=lambda^2
  beta=solve(lambda) %*% rxy
  rsquare=colSums(beta^2)
  rawwgt=lambdasq %*% beta ^2
  import=(rawwgt/rsquare)*100
  lbls=names(model_name$model[2:nvar])
  rownames(import)=lbls
  colnames(import)='weights'
  barplot(t(import),names.arg=lbls,
          ylab="% of r_squre",
          xlab='predictor variables',
          main='relative importance of predictor variables',
          sub=paste('r_squre=',round(rsquare,digits=3)),
          ...)
  return(import)
}

#去掉变异系数小、严重不平衡的数据、相关性高、共线性高的

differentWay_wipe<-function(data=data)
{
  require(caret)
  data=var_wipe(data) 
  data=ZeroVar_wipe(data) 
  data=cor_wipe(data) 
  data=coll_wipe(data) 
  return(data)
}

#去除变异系数小(标准差/平均值)
var_wipe<-function(data=data,label=trainLabel,cv_number=0.05)
{
  if (length(names(data))>1) {
    name_begin=names(data)
    name=names(data[label, ])
    sd=sapply(data[label, name], sd)
    mean=sapply(data[label, name], mean)
    byxs=sd/mean
    cv_name1=names(byxs[byxs<cv_number&(!is.na(byxs))])  
    cv_name2=names(byxs[(is.na(byxs))])
    cv_name=unique(c(cv_name1,cv_name2))
    if(length(cv_name)>=1){data<-data[,setdiff(names(data),cv_name)]}
    if(is.numeric(data)) {data=as.data.frame(data);names(data)=setdiff(name_begin,ccc)}
  }
  return(data)
}

#去除严重不平衡的数据
ZeroVar_wipe<-function(data=data,label=trainLabel,ZeroVar_number=98/2)
{
  if (length(names(data))>1) {
    name_begin=names(data)
    vars2RmLowVariance=nearZeroVar(data[label, ], ZeroVar_number,names=TRUE)
    if(length(vars2RmLowVariance)>=1){data<-data[,setdiff(names(data),vars2RmLowVariance)]}
    if(is.numeric(data)) {data=as.data.frame(data);names(data)=setdiff(name_begin,vars2RmLowVariance)}
  }
  return(data)
}

#去除相关性correlation
cor_wipe<-function(data=data,label=trainLabel,cor_number=0.9)
{
  if (length(names(data))>1) {
    name_begin=names(data)
    corMatrix <- cor(data[label, ])
    vars2RmHighCorr <- findCorrelation(corMatrix,cor_number,names=TRUE)
    if(length(vars2RmHighCorr)>=1){data<-data[,setdiff(names(data),vars2RmHighCorr)]}
    if(is.numeric(data)) {data=as.data.frame(data);names(data)=setdiff(name_begin,vars2RmHighCorr)}
  }
  return(data)
}

#去除共线性
coll_wipe<-function(data=data,label=trainLabel)
{
  if (length(names(data))>1) {
    name_begin=names(data)
    collinearInfo <- findLinearCombos(data[label, ])
    vars2RmCollinear <- names(data)[collinearInfo$remove]
    if(length(vars2RmCollinear)>=1){data<-data[,setdiff(names(data),vars2RmCollinear)]}
    if(is.numeric(data)) {data=as.data.frame(data);names(data)=setdiff(name_begin,vars2RmCollinear)}
  }
  return(data)
}

##########把x和y呈u型关系的做变量转换
uuu_cut_transform<-function(data,uuu_cut,data_uuu)
{
  name_all=as.character(uuu_cut$name) 
  cut_all=as.numeric(uuu_cut$cut)
  name_u=c()
  for (i in 1:length(name_all)){
    name=name_all[i]
    newname <- paste0('Fuu_',cut_all[i],'_',name)
    u1=abs(data[,name_all[i]]-cut_all[i])
    data[,newname]=u1
    name_u=c(name_u,newname)
  }
  data_uuu=data[,name_u]
  return(data_uuu)
}

##########把x和y呈L型关系的做变量转换
# UpLow_cut_transform<-function(data,UpLow_cut)
# {
#   name_all=as.character(UpLow_cut$name) 
#   cut_all=as.numeric(UpLow_cut$cut)
#   type_all=as.character(UpLow_cut$type)
#   name_UpLow=c()
#   for (i in 1:length(name_all)){
#     name=name_all[i]
#     cut=as.numeric(cut_all[i])
#     type=type_all[i]
#     if (type=='up'){ newname <- paste0('Fup_',cut_all[i],'_',name);
#     newvalues=ifelse(data[,name]>=cut&data[,name]<99999999,cut,data[,name]);
#     data[,newname]=newvalues;
#     newname_type <- paste0('FupCut_',cut_all[i],'_',name);
#     newvalues_type=ifelse(data[,name]>=cut&data[,name]<99999999,1,0);
#     data[,newname_type]=newvalues_type;
#     name_UpLow=c(name_UpLow,newname,newname_type);  }
#     if (type=='low'){newname <- paste0('Flow_',cut_all[i],'_',name);
#     newvalues=ifelse(data[,name]<=cut&data[,name]>-99999999,cut,data[,name]);
#     data[,newname]=newvalues;
#     newname_type <- paste0('FlowCut_',cut_all[i],'_',name);
#     newvalues_type=ifelse(data[,name]<=cut&data[,name]>-99999999,1,0);
#     data[,newname_type]=newvalues_type;
#     name_UpLow=c(name_UpLow,newname,newname_type); }
#   }
#   data_uuu=data[,name_UpLow]
#   return(data_uuu)
# }

# if (length(varTransformSingle)>=1&(exists("uuu_transform", envir = new.env())) ){
# name_u=c()
# for (i in 1:length(varTransformSingle)){
# cfValue=quantile(dfModelAllX[,varTransformSingle[i]], probs = c(0,0.1,0.3,0.5,0.7,0.9))
# aaa=c(cfValue[2],cfValue[3],cfValue[4],cfValue[5])
# name=varTransformSingle[i]
# u1=abs(dfModelAllX[,varTransformSingle[i]]-aaa[1])
# u2=abs(dfModelAllX[,varTransformSingle[i]]-aaa[2])
# u3=abs(dfModelAllX[,varTransformSingle[i]]-aaa[3])
# u4=abs(dfModelAllX[,varTransformSingle[i]]-aaa[4])
# newname1 <- paste0(name,'_u1')
# newname2 <- paste0(name,'_u2')
# newname3 <- paste0(name,'_u3')
# newname4 <- paste0(name,'_u4')
# dfModelAllX[,newname1]=u1
# dfModelAllX[,newname2]=u2
# dfModelAllX[,newname3]=u3
# dfModelAllX[,newname4]=u4
# name_u=c(name_u,newname1,newname2,newname3,newname4)
# }
# varTransformSingle=c(varTransformSingle,name_u)
# }

name_original_trans<-function(name=name)
{
  name1=c()
  for (i in 1:length(name)) {
    a=name[i]
    a=sub("SQ_",'',a)
    a=sub("SR_",'',a)
    a=sub("IV_",'',a)
    a=sub("LN_",'',a)
    a=sub("FLB_",'',a)
    a=sub("FUB_",'',a)
    a=sub("FSM_",'',a)
    a=sub("FX1_",'',a)
    a=sub("FX2_",'',a)
    a=sub("FX3_",'',a)
    a=sub("FX4_",'',a)
    a=sub("FX5_",'',a)
    a=sub("FX6_",'',a)
    a=sub("FX7_",'',a)
    a=sub("FX8_",'',a)
    a=sub("FX9_",'',a)
    a=sub("FX10_",'',a)
    a=sub("FX11_",'',a)
    a=sub("FM1_",'',a)
    a=sub("FM2_",'',a)
    a=sub("Flow_",'',a)
    a=sub("FlowCut_",'',a)
    a=sub("Fup_",'',a)
    a=sub("FupCut_",'',a)
    name1=append(name1,a)
  }
  return(name1)
}


##########计算ks和auc
ks_auc_compute<-function(data=data,p='p',targetVar=targetVar){
#p=p;targetVar=targetVar;
pred <- prediction(data[,p], data[,targetVar])
perf <- performance(pred,"tpr","fpr")
ks=round(max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]),4)
perf2 <- performance(pred, 'auc')
auc=round(as.numeric(perf2@y.values),4)
return(aaa=list(ks=ks,auc=auc))
}
