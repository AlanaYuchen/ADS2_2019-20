library("tidyr")
library("dplyr")
library("ggplot2")
library("MAP")

GDP0<- read.csv("/Users/chenghui/Documents/ADS2/material/ADS2 week 6/GDP.csv",check.names = F,na.strings = "") 
#na.strings makes the NA can be read by computer, seems not necessary here (self-recognized), check.names=F can change X2018 to 2018
GDP1<-gather(GDP0,key = "Year",value = "GDP",-CountryName,factor_key = T,convert = T)  # '-CountryName' refers to exclude the data in "CountryName" Column
GDP2<-drop_na(GDP1)
GDP3<-filter(GDP2,CountryName=='Germany'|CountryName=='France'|CountryName=='Italy'|CountryName=='Greece')
GDP3<-filter(GDP3,Year=='1960'|Year=='1970'|Year=='1980'|Year=='1990'|Year=='2000'|Year=='2010'|Year=='2018') #
plot1.1<-ggplot(GDP3,aes(x=Year,y=GDP,color=CountryName))+
  geom_point()+
  geom_line(aes(group=CountryName))+
  geom_text(aes(label=GDP),hjust = 0.1, nudge_x = 0.05,size=2)+
  labs(title = "Trend of GDP Growth in Four Countries")
  
plot1.2<-plot1.1+
  scale_y_continuous(trans='log2')+
  facet_grid(.~CountryName)

plot1.3<-ggplot(GDP3,aes(x=Year,y=GDP,color=CountryName))+
  geom_point()+
  geom_smooth(aes(group=CountryName),method = 'loess')
  

plot1.4<-ggplot(GDP3,aes(x=Year,y=GDP,color=CountryName))+
  geom_area(aes(fill=CountryName),position='fill')          # cannot reach satisfaction
  
plot1.5<-ggplot(GDP3,aes(x=Year,y=GDP))+
  geom_bar(aes(x=Year,fill=CountryName),position = "dodge",stat='identity',width = 0.8)

GDP6<-read.csv("/Users/chenghui/Documents/ADS2/material/ADS2 week 6/GDP.csv",na.strings = "",header = T,check.names = F)
GDP7<-gather(GDP6,key = "Year",value = "GDP",-CountryName,factor_key = T,convert = T)  # '-CountryName' refers to exclude the data in "CountryName" Column
GDP8<-drop_na(GDP7)
GDP9<-dplyr::filter(GDP8, CountryName %in% c("France","Germany","Italy","Greece"),Year %in% c( 1960,1970,1980,1990,2000,2010,2018))
ggplot(GDP9,aes(x=Year,y=GDP,color=CountryName))+
  geom_point()+
  geom_smooth(aes(group=CountryName),method = 'loess')
ggplot(GDP9,aes(x=Year,y=GDP,color=CountryName))+
  geom_area(aes(fill=CountryName),position='fill')
# what is the difference between them???
gdp <- read.csv("/Users/chenghui/Documents/ADS2/material/ADS2 week 6/GDP.csv", na.strings = "",header = T, check.names = F) #reshape the dataset
gdp <- gather(gdp,key = "Year", value = "GDP", factor_key = T,-CountryName, convert = T)
#remove incomplete records
gdp.noNA <- drop_na(gdp)
#extract subset of data
gdp.sub <- dplyr::filter(gdp.noNA, CountryName %in% c("France","Germany","Italy","Greece"),Year %in% c( 1960,1970,1980,1990,2000,2010,2018) ) 
summary(gdp.sub)
ggplot(gdp.sub,aes(x=Year,y=GDP,color=CountryName))+
  geom_area(aes(fill=CountryName),position='fill')
#=================================================

# Map
#eu_GDP<- dplyr::filter(gdp.noNA, CountryName %in% c("France","Germany","Italy","Greece"),Year %in% c(2018) ) #gdp.noNA do not have Germany
map1<-filter(gdp.sub,Year == '2018')
summary(eu_map)
library(maps)
eu_map<-map_data("world",region = c("France","Germany","Italy","Greece"))
ggplot(eu_map,aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=region))
data<-dplyr::left_join(eu_map,map1,by=c("region"="CountryName"))       
ggplot(eu_map,aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=data$GDP))
gmap <- ggplot(eu_map,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=data$GDP)) + 
  labs(title = "2018 GDP in eu countries",cex = 8)+ 
  scale_fill_gradient(low="dark blue",high="red",breaks=c(1e+12,2e+12,3e+12),labels=c(" 1 Trillion","2 Trillion","3 Trillion"))
setwd("/Users/chenghui/Documents/ADS2/material/ADS2 week 6/")
png("GDP.png",width = 800,height = 600)
gmap
dev.off()
