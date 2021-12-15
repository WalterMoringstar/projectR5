#-------R语言实例
#加载包
install.packages("jiebaR")
library("jiebaR")

#加载分词环境
wk<-worker()
wk['爸妈第一次出国，很放心，他们告诉我会很开心，我就心满意足了']

#查看分词引擎配置
wk

#查看默认词典位置
show_dictpath()
#查看目录
dir(show_dictpath())

#打开系统词典文件jieba.dict.utf8，并打印前10行
scan(file="C:/Program Files/R/R3.2.5/library/jiebaRD/dict
/jieba.dict.utf8",what=character(),nlines=10,sep='\n'
,encoding='utf-8',fileEncoding='utf-8')


#打开用户自定义词典文件user.dict.utf8，并打印前10行
scan(file="C:/Program Files/R/R3.2.5/library/jiebaRD/dict
/user.dict.utf8",what=character(),nlines=10,sep='\n'
,encoding='utf-8',fileEncoding='utf-8')


#增加自定义词典
wk["我喜欢量子号的邮轮"]

#设定空间默认路径
setwd("C:\\Users\\用户路径")
#用户自定义词典名称
userdic<-'trip_dic.txt'
#加载分词引擎，导入自定义词典
wk = worker(user=userdic,bylines=TRUE,lines=5000000)
#分词
wk["我喜欢量子号的邮轮"]

#增加停用词词典
#用户自定义词典和停用词词典名称
userdic<-'trip_dic.txt'
stopword<-'stopword_adj.txt'
#加载分词引擎，导入自定义词典
wk = worker(user=userdic,stop_word=stopword,bylines=TRUE,lines=5000000)
#分词
wk["我喜欢量子号的邮轮"]

#关键词提取TF-IDF
 #用户自定义词典名称
 userdic<-'trip_dic.txt'
 stopword<-'stopword_adj.txt'
 #加载分词引擎，导入自定义词典
 wk = worker(user=userdic,stop_word=stopword,lines=5000000)
 #分词
 segment<-wk["R的极客理想系列文章，涵盖了R的思想，使用，工具，创新等的一系列要点，以我个人的学习和体验去诠释R的强大。"]
 segment
 freq(segment)    #计算词频
keys<-worker("keywords",topn=5)   #设置关键词数量
vector_keywords(segment,keys)     #计算关键词分值



#词性标注
#用户自定义词典名称
 userdic<-'trip_dic.txt'
 stopword<-'stopword_adj.txt'
 #加载分词引擎，导入自定义词典
 wk = worker(user=userdic,stop_word=stopword,"tag",lines=5000000)
 #分词
 segment<-wk["爸妈第一次出国，很放心，他们告诉我会很开心，我就心满意足了"]
segment


#-----项目实战
######################################################################
#函数功能：清理文本数据
#参数说明：
#        text：文本向量
######################################################################
dataclean<-function(text){
     #去除url
     text<-gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",text) 
     #gsub是字符替换函数，去空格
     text <- gsub(pattern = " ", replacement ="", text) 
     #有时需要使用\\\t     
     text <- gsub("\t|\r|\v|\f|\n|\\\t", "", text) 
     text<-gsub(pattern="([0-9]{4}年)?([0-9]*月)?[0-9]{1,}日","",text)
     text<-gsub(pattern="([0-9]{4}年)","",text)
     text<-gsub(pattern="([0-9]{1,}月)","",text)
     text<-gsub(pattern="[0-9]{1,}","",text)
     #清除英文字符
     text <- gsub("[a-zA-Z]", "", text)
     #清除对应sentence里面的空值（文本内容），要先执行文本名  
     text <- text[!is.na(text)]    
     #文本长度过小
     text <- text[!nchar(text) < 2] 
     return(text)
}


######################################################################
#函数功能：分片段并打上标识
#参数说明：
#        text：文本向量
######################################################################
splitsentence<-function(text){
   commentdata<-data.frame(id=seq(1,length(text),1),term=text)
   commentdata$term<-as.character(commentdata$term)

   #以标点符号作为分隔符把句子分成片段
   subcon<-strsplit(text,",|\\.|!|\\?|;|~|，|。|！|\\？|；|～|…|﹏﹏|。。。。。。|\\.\\.\\.\\.\\.\\.")

   #计算每条评论片段数
   temp<-unlist(lapply(subcon,length))
   #生成每条评论标号，标号数量和片段数相同
   id<-rep(commentdata$id,temp)
   #把片段结果对象变成向量
   term<-unlist(subcon)

   #打上分句id
   groupid<-function(x){
     subid<-seq(1:x)
     return(subid)
   }

   #生成片段标识
   subid<-paste(id,"-",unlist(lapply(temp,groupid)),seq="")
   subcondata<-data.frame(id=id,term=term,subid=subid)
   subcondata$term<-as.character(subcondata$term)
   subcondata$subid<-as.character(subcondata$subid)

   return(subcondata)
}




######################################################################
#函数功能：分词
#参数说明：
#         useridc：用户自定义词典文件名
#         stopword：停用词词典文件名
#         subdf：数据框，需要分词的数据，每一行为一条文本片段
######################################################################
#导入jiebaR包
library("jiebaR")
segword_trn<-function(userdic,stopword,subdf){
  #载入分词空间
  wk = worker(user=userdic,stop_word=stopword,'tag',bylines=TRUE,lines=5000000)
  #分词函数
  tt<-wk[subdf$term]

  #给每个分词标号
  temp_fc<-unlist(lapply(tt,length))
  id_fc<-rep(subdf[,"subid"],temp_fc)
  term_fc<-unlist(tt)
  segterm_fc<-data.frame(id=id_fc,term=term_fc,cx=names(unlist(tt)))
  segterm_fc$id<-as.character(segterm_fc$id)
  segterm_fc$term<-as.character(segterm_fc$term)
  segterm_fc$cx<-as.character(segterm_fc$cx)
  segterm_fc$id_tot<-as.numeric(unlist(lapply(strsplit(segterm_fc$id,'-'),function(x) x[1])))
  return(segterm_fc)
}






#--- 文本质量量化指标模型
library("jiebaR")
library(plyr)
library(dplyr)

userdic<-'trip_dic.txt'        #用户字典
stopword<-'stopword_adj.txt'   #停止词
qualitydic<-'质量标准.csv'     #质量标准


#导入质量指标相关词词典
qualityword<-read.csv(qualitydic,header=TRUE,stringsAsFactors=FALSE)

#导入文本
content<-read.csv("评论数据.csv",header=TRUE,stringsAsFactors=FALSE)
commenttext<-content$term


#数据清理
commenttext<-dataclean(commenttext)
#分句并转换成数据框并且表上subid
subcondata<-splitsentence(commenttext)
#分词
segworddata<-segword_trn(userdic,stopword,subcondata)




#文本质量评分
#1 主题覆盖量
qualitterm<-join(segworddata,qualityword)
qualitynum<-as.data.frame(qualitterm %>% group_by(id_tot) %>% summarise(n_distinct(class,na.rm=TRUE)))
names(qualitynum)[2]<-"quality_num"
qualitynum$qualitynum_flag<-qualitynum$quality_num
attach(qualitynum)
qualitynum[which(quality_num == 1), ]$qualitynum_flag<-0.2
qualitynum[which(quality_num == 2), ]$qualitynum_flag<-0.4
qualitynum[which(quality_num == 3), ]$qualitynum_flag<-0.6
qualitynum[which(quality_num == 4), ]$qualitynum_flag<-0.8
qualitynum[which(quality_num == 5), ]$qualitynum_flag<-1
detach(qualitynum)


#2 文本分词数量
segwordnum<-as.data.frame(segworddata %>% group_by(id_tot) %>% summarise(n_distinct(term,na.rm=TRUE)))
names(segwordnum)[2]<-"segword_num"

segword_num<-segwordnum$segword_num
segword_num_q2<-quantile(segword_num,0.2)
segword_num_q4<-quantile(segword_num,0.4)
segword_num_q6<-quantile(segword_num,0.6)
segword_num_q8<-quantile(segword_num,0.8)
segword_num_q10<-quantile(segword_num,1)

segwordnum$segwordnum_flag<-segwordnum$segword_num

attach(segwordnum)
segwordnum[which(segword_num >=0 & segword_num <=segword_num_q2), ]$segwordnum_flag<-0.2
segwordnum[which(segword_num > segword_num_q2 & segword_num <= segword_num_q4), ]$segwordnum_flag<-0.4
segwordnum[which(segword_num > segword_num_q4 & segword_num <= segword_num_q6), ]$segwordnum_flag<-0.6
segwordnum[which(segword_num > segword_num_q6 & segword_num <= segword_num_q8), ]$segwordnum_flag<-0.8
segwordnum[which(segword_num > segword_num_q8 & segword_num <= segword_num_q10), ]$segwordnum_flag<-1
detach(segwordnum)



#3 评论点赞数
positive_num<-content$positivenum
positive_num_q2<-quantile(positive_num,0.2)+0.001
positive_num_q4<-quantile(positive_num,0.4)+0.001
positive_num_q6<-quantile(positive_num,0.6)+0.001
positive_num_q8<-quantile(positive_num,0.8)+0.001
positive_num_q10<-quantile(positive_num,1)+0.001

positivenum<-data.frame(id_tot=content$id,positive_num=content$positivenum,positivenum_flag=positive_num)

attach(positivenum)
positivenum[which(positive_num >=0 & positive_num <=positive_num_q2), ]$positivenum_flag<-0.2
positivenum[which(positive_num > positive_num_q2 & positive_num <= positive_num_q4), ]$positivenum_flag<-0.4
positivenum[which(positive_num > positive_num_q4 & positive_num <= positive_num_q6), ]$positivenum_flag<-0.6
positivenum[which(positive_num > positive_num_q6 & positive_num <= positive_num_q8), ]$positivenum_flag<-0.8
positivenum[which(positive_num > positive_num_q8 & positive_num <= positive_num_q10), ]$positivenum_flag<-1
detach(positivenum)


#4 评论中照片数量
photonum<-data.frame(id_tot=content$id,isphoto=content$isphoto,photo_flag=content$isphoto)

attach(photonum)
photonum[which(isphoto ==0), ]$photo_flag<-0
photonum[which(isphoto ==1), ]$photo_flag<-1
detach(photonum)


#5评论分值偏移
score_num<-data.frame(id_tot=content$id,score=content$score,score_flag=0)
score<-content$score
median_score<-median(score)
diffscore<-abs(score-median_score)

diffscore_q2<-quantile(diffscore,0.2)+0.001
diffscore_q4<-quantile(diffscore,0.4)+0.001
diffscore_q6<-quantile(diffscore,0.6)+0.001
diffscore_q8<-quantile(diffscore,0.8)+0.001
diffscore_q10<-quantile(diffscore,1)+0.001

attach(score_num)
score_num[which(score>median_score-diffscore_q2 & score<=median_score+diffscore_q2), ]$score_flag<-1
score_num[which((score>median_score-diffscore_q4 & score<=median_score-diffscore_q2)
                |(score>median_score+diffscore_q2 & score<=median_score+diffscore_q4)
               ), ]$score_flag<-0.8
score_num[which((score>median_score-diffscore_q6 & score<=median_score-diffscore_q4)
                |(score>median_score+diffscore_q4 & score<=median_score+diffscore_q6)
               ), ]$score_flag<-0.6
score_num[which((score>median_score-diffscore_q8 & score<=median_score-diffscore_q6)
                |(score>median_score+diffscore_q6 & score<=median_score+diffscore_q8)
               ), ]$score_flag<-0.4
score_num[which((score>median_score-diffscore_q10 & score<=median_score-diffscore_q8)
                |(score>median_score+diffscore_q8 & score<=median_score+diffscore_q10)
               ), ]$score_flag<-0.2
detach(score_num)



#6 整合评论分
qualityscore<-join(qualitynum,segwordnum)
qualityscore<-join(qualityscore,positivenum)
qualityscore<-join(qualityscore,photonum)
qualityscore<-join(qualityscore,score_num)
qualityscore<-qualityscore[,c("id_tot","qualitynum_flag","segwordnum_flag","positivenum_flag","photo_flag","score_flag")]

attach(qualityscore)
qualityscore$score_tot<-qualitynum_flag*0.3+segwordnum_flag*0.2+positivenum_flag*0.2+photo_flag*0.2+score_flag*0.1
detach(qualityscore)

qualityscore[order(qualityscore$score_tot),]




#--- 用户相似度模型
#导入用户特征数据
uiddesc<-read.csv("用户数据.csv",header=TRUE,stringsAsFactors=FALSE)

#计算欧式距离
eu_dist<-function(a,b){
  dist<-sqrt(sum((a-b)^2))
  return (dist)
}

#新用户
sample_uid<-c(4,3,6,4)

#建立相似度初始向量
simindex_chain<-c()

#计算新用户与每个评论用户相似度
for (i in 1:nrow(uiddesc)){
    eudist<-eu_dist(sample_uid,unlist(uiddesc[i,-1]))
    simindex<-1/(1+eudist)
    simindex_chain<-c(simindex_chain,simindex)
}

#相似度结果
simiindex_df<-data.frame(id=c(1:nrow(uiddesc)),simindex=simindex_chain)
simiindex_df[order(-simiindex_df$simindex),]





#------情感词分析
library("jiebaR")
library(plyr)
library(dplyr)

userdic<-'trip_dic.txt'        #用户字典
stopword<-'stopword_adj.txt'   #停止词
postivedic<-"postive.txt"      #正向情感词
negtivedic<-"nagative.txt"     #负向情感词
advworddic<-"程度副词.csv"     #程度副词字典
denyworddic<-"否定词.csv"      #否定词字典


#导入情感词并附上权重
postive=readLines(postivedic,encoding='UTF-8')
nagtive=readLines(negtivedic,encoding='UTF-8')
pos<-data.frame(term=postive,weight=rep(1,length(postive)))
neg<-data.frame(term=nagtive,weight=rep(-1,length(nagtive)))
posneg_tot<-rbind(pos,neg)

#导入程度副词、否定词
advword<-read.csv(advworddic,header=TRUE,stringsAsFactors=FALSE)
denyword<-read.csv(denyworddic,header=TRUE,stringsAsFactors=FALSE)
#导入文本
content<-read.csv("评论数据.csv",header=TRUE,stringsAsFactors=FALSE)
commenttext<-content$term
#数据清理
commenttext<-dataclean(commenttext)
#分句并转换成数据框并且表上subid
subcondata<-splitsentence(commenttext)
#分词
segworddata<-segword_trn(userdic,stopword,subcondata)

#关联情感词、程度副词和否定词
tstterm<-join(segworddata,posneg_tot)
tstterm<-join(tstterm,advword)
names(tstterm)[length(names(tstterm))]<-"adv_score"    
tstterm<-join(tstterm,denyword,by='term')
names(tstterm)[length(names(tstterm))]<-"deny_score"
tstterm$adv_score[!complete.cases(tstterm$adv_score)]<--999
tstterm$deny_score[!complete.cases(tstterm$deny_score)]<--999
tstterm$id_tot<-as.numeric(gsub(" ","",tstterm$id_tot))



#####################################################################
#函数功能：对片段进行情感性打分
#参数说明：
#         idname：片段标号
#         fliename：带有否定词、副词和正负情感词的文本
#####################################################################
word_segment <- function(idname,filename){

    #-- 打行号
    #抽取片段
    filepart = subset(filename,id==idname)
    #对片段中每个分词打上id
    wordfile = data.frame(
       filepart
      ,idx=1:nrow(filepart) )

    #找出正负情感词在片段中的位置
    wordindex = wordfile$idx[!is.na(wordfile$weight)]
    
    #-- 上下限表
    citeration = data.frame(
                            wordindex
                           ,left  = wordindex-3
                           ,right = wordindex+3
                           ,leftidx = c(wordindex[1]-4,head(wordindex,-1))
                           ,rightidx = c(tail(wordindex,-1),wordindex[length(wordindex)]+4)
                           ,left_up=c(tail(wordindex-3,-1),wordindex[length(wordindex-3)]+3)
                           )

   #窗口期判定函数
   computevalue <- function(i,citeration,wordindex,filepart){
         left = ifelse(citeration$left[wordindex==i]<0,0,citeration$left[wordindex==i])
         right= citeration$right[wordindex==i]
         leftidx= ifelse(citeration$leftidx[wordindex==i]<0,0,citeration$leftidx[wordindex==i])
         rightidx= citeration$rightidx[wordindex==i]
         left_up=citeration$left_up[wordindex==i]
         wdidx=citeration$wordindex[wordindex==i]
  
         result = cbind(
                        ifelse(right<rightidx
                              ,max((filepart$adv_score[max(left,leftidx+1):max(wdidx,left_up-1)]),na.rm=T)
                              ,max(filepart$adv_score[max(left,leftidx+1):wdidx],na.rm=T)
                              )
                       ,ifelse(right<rightidx
                              ,max(filepart$deny_score[max(left,leftidx+1):max(wdidx,left_up-1)],na.rm=T)
                              ,max(filepart$deny_score[max(left,leftidx+1):wdidx],na.rm=T))
                              )
        return(result)
    }
                           
    #--计算值
    result = data.frame(t(sapply(wordindex,computevalue,citeration,wordindex,filepart)))
    names(result) = c('adv','deny')

    final_result = data.frame(
       id=idname 
      ,posneg=filepart$weight[wordindex]
      ,result
      )

    return(final_result)
}



#####################################################################
#函数功能：综合计算每条评论总得分
#参数说明：
#         texttb：评论文本（打上情感词、否定词和副词标签后的）
#####################################################################
#情感词综合打分
valuefun<-function(texttb){
  #抽取正负情感词所在的片段
  idnotnull<-data.frame(id=unique(texttb$id[complete.cases(texttb$weight)]))
  idnotnull$id<-as.character(idnotnull$id)
  tstterm_nnid<-join(texttb,idnotnull,type="inner")

  word_index<-unique(tstterm_nnid$id)
  
  system.time(score_combine<-lapply(word_index,word_segment,tstterm_nnid))
  score_combine_tb<-do.call("rbind", score_combine) 
  score_combine_tb$id<-as.character(score_combine_tb$id)
  score_combine_tb$adv[score_combine_tb$adv==-999]<-1
  score_combine_tb$deny[score_combine_tb$deny==-999]<-1
  score_combine_tb$value<-score_combine_tb$posneg*score_combine_tb$adv*score_combine_tb$deny
  subconvalue<-aggregate(score_combine_tb$value,by=list(score_combine_tb$id),sum)
  subconvalue$idtot<-as.numeric(unlist(lapply(strsplit(subconvalue$Group.1,'-'),function(x) x[1])))
  commentvalue<-aggregate(subconvalue$x,by=list(subconvalue$idtot),sum)
  names(commentvalue)[1]<-'id'
  commentvalue$x<-round(commentvalue$x,2)
  return(commentvalue)
}



system.time(valuetb<-valuefun(tstterm))


