# Q5.1
unlucky_class<-rnorm(26,40,8)

class_mean<-rnorm(10000,50,5)
class_size<-sample(5:40,10000,replace = T)
sH7Gtotal_grade<-c()
t=list()
class_grade=list()
class_list<-c()
for (i in 1:10000){
  class_grade<-rnorm(class_size[i],class_mean[i],10)
  total_grade<-c(total_grade,class_grade)
  class_list<-c(class_list,replicate(class_size[i],i))
}

mean(total_grade)
hist(total_grade)
normative<-c()
for (i in 1:26){
  normative<-c(normative,sum(unlucky_class[i]>total_grade)/length(total_grade)*100)
}
um=mean(normative)
hist(normative,breaks = 0:100)

#Q5.2
total_gradelist<-c()

for (i in 1:10000){
  class_grade<-rnorm(class_size[i],class_mean[i],10)
  total_gradelist<-c(total_gradelist,list(class_grade))
}
me<-c()
for (i in 1:10000){
  n<-c()
  for (j in 1:length(total_gradelist[[i]])){
    n<-c(n,sum(total_gradelist[[i]][j]>total_grade)/length(total_grade)*100) 
  }
  me<-c(me,mean(n))
}
sum(um>me)
#//
data=data.frame(total_grade,class_list)
data1<-data[order(data$total_grade),]
data2<-data.frame(data1,c(1:length(total_grade)))
npg<-c()
for (i in 204775:length(total_grade)){
  npg<-c(npg,data2$c.1.length.total_grade..[i]/length(total_grade)*100)
}
data3<-data.frame(data2,npg)
mean_pop<-c()
for (i in 1:10000){
  mean_pop<-c(mean_pop,mean(data3$npg[which(data3$class_list==i)]))
}
sum(um>mean_pop)
# 1.58%
#Q5.3 l's team is higher than s'
l<-c(64,63,62,59)
s<-c(70,63,61,56)
a<-c()
b<-c()
a<-(sum(data3$total_grade<l[1])/length(total_grade)*100+sum(data3$total_grade<l[2])/length(total_grade)*100+sum(data3$total_grade<l[3])/length(total_grade)*100+sum(data3$total_grade<l[4])/length(total_grade)*100)/4
b<-(sum(data3$total_grade<s[1])/length(total_grade)*100+sum(data3$total_grade<s[2])/length(total_grade)*100+sum(data3$total_grade<s[3])/length(total_grade)*100+sum(data3$total_grade<s[4])/length(total_grade)*100)/4

