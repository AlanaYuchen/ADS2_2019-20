#sampling from a population
population<-rnorm(1e6,100,5)
popmean<-round(mean(population),2) #round(number,digit) the digit refers to the numbers after "."
popmean

popsd<-round(sd(population),1)
popsd
smean=c()
ssd=c()
ssmean=c()
sssd=c()
for (i in 1:1000){
  popsample<-sample(population,100,replace = FALSE)
  smean<-c(smean,mean(popsample))
  ssd<-c(ssd,sd(popsample))
}
for (i in 1:1000){
  popsample<-sample(population,100,replace = FALSE)
  ssmean<-c(ssmean,mean(popsample))
  sssd<-c(sssd,sd(popsample))
}
hist(ssd)
sssd<-sum(ssd<5)

hist(smean)
mean(ssd)
hist(smean,col="red",ylim = c(0,400),xlim=c(95,105))
par(new=T)
hist(ssmean,col="blue",ylim = c(0,400),xlim = c(95,105))


#getting information about sensitive topics
coin<-sample(0:1,300,replace=T)
yes = 0
no = 0
for (i in 1:300){
  if (coin[i]==0){
    print("Were you born between 1 Januray and 30 June? (T/F)")
    answer<-readline()
  }else{
    print("Have you ever shoplifted?")
    answer<-readline()
  }
  if (answer=="T"){
    yes = yes+1
  }
  if (answer =="F"){
    no = no+1
  }
}
yes 
no
