setwd("C:\\Users\\用户路径")
age_uid<-read.csv("age.csv",header=TRUE,stringsAsFactors=FALSE)


#####################################################################
#函数功能：画出数据分布函数及QQ图
#参数说明：
#        input.data：连续型指标数据
#####################################################################
index_dispic<-function(input.data){
    opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,1))
    qqnorm(input.data,main="QQ图")
    qqline(input.data)
    hist(input.data,freq=F,main="直方图和密度估计曲线")
    lines(density(input.data),col="blue")
    x<-c(round(min(input.data)):round(max(input.data)))
    lines(x,dnorm(x,mean(input.data),sd(input.data)),col="red")   
    par(opar)
}



#####################################################################
#函数功能：对连续型指标数据做描述性统计
#参数说明：
#        x：连续型指标数据
#####################################################################
mystats <- function(x, na.omit = FALSE) {
  if (na.omit) 
    x <- x[!is.na(x)]
  m <- round(mean(x),2)
  n <- round(length(x),2)
  s <- sd(x)
  var<-round(var(x),2)
  range<-round(range(x),2)
  min<-min(x)
  max<-max(x)
  q_25<-quantile(x,0.25)
  median<-round(median(x),2)
  q_75<-quantile(x,0.75)
  #计算偏度
  skew <- round(sum((x - m)^3/s^3)/n,2)
  #计算峰度
  kurt <- round(sum((x - m)^4/s^4)/n - 3,2)
  return(data.frame(n = n,mean = m,stdev = s
           ,var=var,min=min,q_25=q_25,median=median,q_75=q_75,max=max
           , skew = skew, kurtosis = kurt))
}


index_dispic(age_uid$age)
mystats(age_uid$age)


#寻找聚类结果最好的分类
result_index<-c()
#考察类别数为2至8
k_set<-c(2:8)
for (i in k_set)
{
result<-kmeans(age_uid$age,i)
#计算伪F值
result_index<-c(result_index,result$betweens/(result$tot.withinss+result$betweens))
}

#画出类别与伪F值折线图
plot(k_set,result_index,type='o')

#将原始变量分成5类
i<-5
result<-kmeans(age_uid$age,i)
result_cluster_per<-data.frame(clusterper=paste(round(result$size*100/length(result$cluster),2),'%',sep=''))
result_data<-data.frame(center=round(result$centers,2),cnt=result$size,result_cluster_per)
#对每类数据进行描述性统计
cluster<-data.frame(value=age_uid$age,cluster=result$cluster)
cluster_result<-aggregate(value~cluster,data=cluster ,FUN=mystats)
#整合所有数据
total_result<-data.frame(as.data.frame(cluster_result[[2]]),result_data)

