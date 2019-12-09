## 1. one sample ttest
#1. embryonic stem cell’s SOX17 expression different from others 
#2. T distribution
#3. critical value
cv<-qt(0.975,2)
  ## qt(density,df) generate critical value
  ## dt(c_v, df) gives density 
  ## pt(critical_value, degree of freedom), 2*pt(-abs(c_v),df) gives the p_value of two_tailed t_test
sample<-c(15,30,50)
mu=8.9
#4.
t.test(sample,mu=mu)
  #p.value=0.1538
#5. cannot reject null hypothesis
#6. no significant difference

## 2. two sample ttest
#2.1 paired
#1. Na:the new treatment significantly reduce the AML1 expression level in acute myeloid leukemia paitents
#2. T distribution
#3. 
p<-qt(0.975,4)
x<-c(102,340,234,332,129)
y<-c(74,56,70,104,11)
t_value<-mean(x-y)/(sd(x-y)/sqrt(5))
t.test(x,y,paired=T,alternative = 'greater')
  #p.value=0.01026
# reject null hypothesis
#6. significant reduction

#2.2 unpaired
geneexp<-read.csv('/Users/chenghui/Documents/ADS2/material/ADS2 week 10/week10_t_test_problemset_testdata.csv')
head(geneexp)
tail(geneexp)
dim(geneexp)
pvalue<-c()
for (i in 1:length(geneexp$gname)){
  pvalue<-c(pvalue,t.test(geneexp[i,2:5],geneexp[i,6:9])$p.value,paired=F)
}
geneexp<-data.frame(geneexp,pvalue)
sig<-geneexp[which(pvalue<=0.05),]
dim(sig)



