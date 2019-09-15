#Looking at students grade
marks<-rnorm(100,86,5.0)
hist(marks,pch=16,xlab = "marks distribution",ylab = "the number of students",main = "Distribution of students' mark in an introductory BMI class at UoA",col = "lightgrey")
cnt1 = 0
cnt2 = 0
for (i in 1:100){
  if(marks[i]>91||marks[i]<81){
    cnt1 = cnt1+1
  }
  if(marks[i]>96||marks[1]<76){
    cnt2=cnt2+1
  }
}
print(paste("The number of students whose score higher than 91 or lower than 81 is:",cnt1))
print(paste("The number of students whose score higher than 96 or lower than 76 is:",cnt2))

#=================================
grade_81_91 <- sum(marks<81|marks>91)
grade_81_91

other_class = c(36, rep(86,49), rep(87,50)) 
mean(other_class)
round(sd(other_class),digits = 2)
hist(other_class, xlab="grade", breaks=100, col="lavender", main="Other class")

#getting good grades
set3=c()
set4=c()
for (i in 1:1000){
  cnt3 = 0
  cnt4 = 0
  correct<-sample(1:4,20,replace = TRUE)
  answer<-sample(1:4,20,replace =TRUE)
  for (i in 1:20){
    if (correct[i]==answer[i]){
      cnt3 = cnt3+1
    }
    if (correct[i]==1){
      cnt4 = cnt4+1
    }
  }
  set3<-c(set3,cnt3/20)
  set4<-c(set4,cnt4/20)
}
cnt3>=10
cnt4>=10
t.test(set3,set4)
plot(set3,col='red',pch=".",ylab = "possibility of correct answers")
par(new=TRUE)
plot(set4,col='blue',pch=".",ylab='')
#=======
0.25^10*0.75^10*choose(20,10)
p_passing=0
for (s in 10:20){
  a<-0.25^s*0.75^(20-s)*choose(20,s)
  p_passing = p_passing+a
}
p_passing




