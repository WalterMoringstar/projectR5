#计算最小样本量
minsample<-function(alpha=0.05,power=0.9,lift=0.02,mu,sd_num){
    z<-(qnorm(1-alpha/2)+qnorm(power))^2
    d<-(lift*mu/sd_num)^2
    minsample<-2*z/d
    return(minsample)
}
minsample(,,,0.1,1.29)

minsample(0.05,0.8,0.02,0.09,0.488)



#固定置信度和power
minsample_fun<-function(lift,mu,sd_num){
    fm1<-(sd_num*(qnorm(0.975)+qnorm(0.8)))/mu
    fm<-2*(fm1^2)
    minsample<-fm/(lift^2)
    return(minsample)
}

#提升度从0.01到0.05
lift<-seq(0.01,0.05,0.001)
nlift<-length(lift)
samplen<-c()

#生成不同提升度下的最小样本量
for (i in 1:nlift){
   minsample<-minsample_fun(lift[i],0.09,0.488)
   samplen[i]<-ceiling(minsample)
}

#绘制曲线图
plot(samplen,lift,type='o')
abline(h=0.02)
