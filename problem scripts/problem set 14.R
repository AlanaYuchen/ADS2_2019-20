# Testing assumptions for ANOVA
# normally distributed residual
d1<-rnorm(1000,0,1)+3
d2<-rnorm(1000,0,1)+5
d3<-rnorm(1000,0,5)+7
data1<-data.frame(d1,d2,d3)
sd1<-aov(d1~d2+d3,data = data1)
plot(sd1,2)

# non-normal distribution of residual
d4<-sample(c(0.1,0.2,0.3),1000,replace = T)+3
d5<-sample(c(0.1,0.2,0.3),1000,replace = T)+5
d6<-sample(c(0.1,0.2,0.3),1000,replace = T)+7
data2<-data.frame(d4,d5,d6)
sd2<-aov(d4~d5+d6,data = data2)
plot(sd2,1)
plot(sd2,2)

#equal variance
d7<-rnorm(1000,1,1)
d8<-rnorm(1000,3,1)
d9<-rnorm(1000,5,1)
data3<-data.frame(d7,d8,d9)
sd3<-aov(d8~d7+d9,data = data3)
plot(sd3,1)

# unequal variance
d10<-rnorm(1000,1,1)
d11<-rnorm(1000,3,2)
d12<-rnorm(1000,5,3)
data4<-data.frame(d10,d11,d12)
sd4<-aov(d10~d11+d12,data = data4)
plot(sd4,1)
plot(sd4,2)
