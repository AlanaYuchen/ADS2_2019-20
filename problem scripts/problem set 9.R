# Guinness Quality Control
barley<-read.table("/Users/chenghui/Documents/ADS2/material/ADS2 week 9/barley.txt",header = F)
t.test(data,mu=50)$p.value
# p.value=4.94e-07, there is significant difference between sample and 50
hist(data$V1,breaks = 35:60)
s<-c()
for (i in 2:50){
  s<-c(s,sum(replicate(100,t.test(sample(barley1$V1,i,replace = F),mu=50)$p.value<0.05)))
}
min(which(s>=95))
51-sum(s>=95)
plot(s,type='l')
abline(h=95,col='red')
# minimun number to screen is 18.
#=================================
barley1<-read.table("/Users/chenghui/Documents/ADS2/material/ADS2 week 9/barley.txt",header = F)
#barley<-scan("/Users/chenghui/Documents/ADS2/material/ADS2 week 9/barley.txt") 
sampling_means<-vector() 
for (replicate in 1:1000){
  barley_sample<-sample(barley1$V1, size = length(barley1$V1), replace = TRUE)
  sampling_means<-c(sampling_means, mean(barley_sample)) }
hist(sampling_means, xlab = "Sample means", main = "")
shapiro.test(sampling_means) #Shapiro-Wilk normality test, if p-value>0.05, follows normal distribution



