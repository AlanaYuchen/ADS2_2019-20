jb<-read.csv("/Users/chenghui/Documents/ADS2/material/ADS2 week 13/jellybeans.csv")
head(jb)
library(ggplot2)
ggplot(jb,aes(y=score))+
  geom_boxplot(aes(fill=colour),show.legend = NA)+
  labs(title='Jellybean',x='colour')

# Madness of multiple ttest
mauve<-jb[which(jb$colour=='mauve'),]
control<-jb[which(jb$colour=='control'),]
t.test(mauve$score,control$score,paired = F)
# mauve jellybean has significant difference from control
