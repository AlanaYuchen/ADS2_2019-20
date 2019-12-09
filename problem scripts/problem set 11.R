# 2. one sample sample size estimation
n=((1.96*385)/100)^2
m=n/0.95
# recruit 57/60 patients
# 3. two sample sample size estimation
n = 2* (((1.96+0.85)/(0.25/0.42))^2)
# n=44.255, so we need 45 patients
power.t.test(delta = 0.25, sd=0.42, sig.level = 0.05, power = 0.8)$n #n = 45.286
qt(1.96,4)
pnorm(1.96,mean = 0,sd=1)

# qnorm(p,mean,sd) output value on x axis
# pnorm(q,mean,sd) output density on the left side of q

# 3. relationship between statistical power, sample size, significance leve and effect size.
# 3.1
power.t.test(sd=0.5,sig.level = 0.05, alternative = 'two.sided',type = 'two.sample',n=20, delta = 0.4)$power
# power = 0.693

# 3.2
power.t.test(sd=0.5,sig.level = 0.1, alternative = 'two.sided',type = 'two.sample',n=20, delta = 0.4)$power
# power = 0.799, power increase as significant level increase

# 3.3
power.t.test(sd=0.5,sig.level = 0.05, alternative = 'two.sided',type = 'two.sample',n=10, delta = 0.4)$power
# power = 0.395, power decrease as sample size decrease

# 3.4
power.t.test(sd=0.5,sig.level = 0.05, alternative = 'two.sided',type = 'two.sample',n=20, delta = 0.8)$power
# power = 0.999, power increase as effect size increase

# 3.5
slsp<-c()
sl<-seq(0.01,0.1,length.out = 100)
for (i in sl){
 slsp<-c(slsp,power.t.test(sd=0.5,sig.level = i, alternative = 'two.sided',type = 'two.sample',n=20, delta = 0.8)$power) 
}
sssp<-c()
ss<-seq(5,104,length.out = 100)
for (i in ss){
  sssp<-c(sssp,power.t.test(sd=0.5,sig.level = 0.05, alternative = 'two.sided',type = 'two.sample',n=i, delta = 0.8)$power) 
}
essp<-c()
es<-seq(0.1,1,length.out = 100)
for (i in es){
  essp<-c(essp,power.t.test(sd=0.5,sig.level = 0.05, alternative = 'two.sided',type = 'two.sample',n=20, delta = i)$power) 
}
plot(sl,slsp,ylim = c(0.9,1))
par(new=T)
plot(ss,sssp,col='red')
par(new=T)
plot(es,essp,col='blue')

# 4. relationship between sample size vs p-value
# 4.1
set.seed(10)
a<-rnorm(5,10,5)
b<-rnorm(5,11,5)
t.test(a,b,paired = F,alternative = 'two.sided',var.equal = T)
# p>0.05, no significant difference between a and b

# 4.2
a<-rnorm(500,10,5)
b<-rnorm(500,11,5)
t.test(a,b,paired = F,alternative = 'two.sided',var.equal = T)
# p<0.05, significant difference

# 4.3
size<-seq.int(5,500,length.out = 100)
ppvalue<-c()
for (i in size){
  pvalue<-c()
  for (j in 1:100){
  a<-rnorm(i,10,5)
  b<-rnorm(i,11,5)
  pvalue<-c(pvalue,t.test(a,b,paired = F,alternative = 'two.sided',var.equal = T)$p.value)
  }
  ppvalue<-c(ppvalue,mean(pvalue))
}
plot(size,ppvalue,xlim=c(5,500), ylim=c(0,0.5),pch='.')
plot(size,ppvalue,pch=16)
