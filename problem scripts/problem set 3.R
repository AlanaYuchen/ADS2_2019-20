## checked
# Rolling dice
result<-c()
for(i in 1:1000){
  dots=0
for(i in 1:5){
  dot<-sample(1:6,1)
  dots = dots+dot
}
 result <- c(result,dots) 
}

hist(result,freq = F)
lines(density(result))


#Bean machine
list<-c()
for (i in 1:1000){
  sum=0
  for (i in 1:8){
    posi<-sample(0:1,1)
    sum = sum + posi
  }
  list <- c(list,sum)
}
hist(list,col='pink',main = 'Bean Machine')

  #differnt probability
list<-c()
for (i in 1:100){
  sum=0
  # incresing the amount of independent random variable
  for (i in 1:50){
    posi<-sample(0:1,1,prob = c(0.8,0.2))
    sum = sum + posi
  }
  list <- c(list,sum)
}
list<-c()
#Mela
for (i in 1:100){
  s<-sum(sample(c(0,1),50,replace = T, prob = c(0.2,0.8)))
  list<-c(list,s)
}

hist(list,col='pink',main = 'Bean Machine',breaks = 0:80,axes = F)
summary(list)
sum(list==1)

# Class grade
score<-c()
for (i in 1:30){
  s<-sample(0:1,10,prob = c(0.75,0.25),replace = T)
  score<-c(score,sum(s))
}
av<-c()
for (i in 1:1000){
  st<-sample(score,10,replace = F)
  av<-c(av,sum(st)/10)
}
hist(av)

# question number maximization
score<-c()
for (i in 1:300){
  s<-sample(0:1,1000,prob = c(0.75,0.25),replace = T)
  score<-c(score,sum(s))
}

hist(score,breaks = 0:1000)
