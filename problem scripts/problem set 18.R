# 1
# 1.1
# H0: There is no difference between flavors.
fl<-c(40,32,48,57,23)
css<-c()
for (i in 1:1000){
si<-rnorm(5,40,3)
if (sum(si>0)==5){
cs<-sum((si-40)^2/40)
css<-c(css,cs)
}}
chisq.test(fl)
mean(17.65>css)
library(ggplot2)
css<-data.frame(css)
ggplot(css,aes(x=css))+
  geom_density()
for (i in 1:1000){
chi_distribution<-rchisq(5,df=4)
chi-square<-sum((chi_distribution-40)^2/40)
}
#===========================================
popu2 <- c(rep("A",500000),rep("B",500000),rep("C",500000),rep("D",500000), rep("E",500000))
chi2 <-function(){
  sam <- sample(popu2,200,replace = F) 
  n.A <- length(which(sam =="A"))
  n.B <- length(which(sam =="B"))
  n.C <- length(which(sam =="C"))
  n.D <- length(which(sam =="D"))
  n.E <- 200-n.A-n.B-n.C-n.D
  X2 <- (n.A-40)^2/40 + (n.B-40)^2/40 + (n.C-40)^2/40 + (n.D-40)^2/40 +
    (n.E-40)^2/40 
  return (X2)}
chi.sti<- replicate(10000, chi2()) #calculate the p-value
Obs <- c(40,32,48,57,23)
chi.survey <- sum((Obs-40)^2/40) 
length(which(chi.sti >= chi.survey))/10000
chisq.test(Obs, p=rep(0.2,5))
chi.df4 <- rchisq(10000,df=4) #compare the curves library(tidyr) library(ggplot2)
chidat <- data.frame(stimu=chi.sti, chi.df4)
library(dplyr)
library(tidyr)
chidat <- gather(chidat, key="method",value="Chi.Square") 
ggplot(data=chidat, aes(x=Chi.Square, color=method)) + geom_density()

#input the data into array
mouse.data <- array(c(40,9,34,7,20,15,25,20), dim = c(2,2,2))
dname <- list(status=c("Alive","Dead"),sex=c("Male","Female"),Genotype= c("WT","KO"))
dimnames(mouse.data) <- dname
mouse.data
mouse.data<-as.table(mouse.data)
summary(mouse.data)
mdf<-as.data.frame(mouse.data)
g <- ggplot(data=mdf, aes(x=Genotype,y=sex, color=status)) + geom_point (aes(size=Freq)) + facet_grid(.~status)
g+theme(axis.text = element_text(size = 15), legend.text =element_text( size = 15))
