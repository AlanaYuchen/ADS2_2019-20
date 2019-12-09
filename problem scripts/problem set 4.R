#checked  
# Generate synthetic data for linear model in R
set.seed(32)
x <- rnorm(500,0,1) 
y <- x*10.8+0.6
plot(x,y,pch='.',col='yellowgreen')
d <- data.frame(x,y)
ds <- d[sample(1:length(d$y),0.5*length(d$y),replace = F),]
ny <- ds$y+rnorm(500,0,1)
plot(ds$x,ny,pch='.',col='yellowgreen')

x <- rnorm(500,0,1) 
y <- x*10.8+0.6

a <- rnorm(500,0,1) 
b <- a*10.8+0.6 + rnorm(500,0,1)

x<-c(x,a)
y<-c(y,b)
d <- data.frame(x,y)

plot(d$x,d$y,pch='.',col='yellowgreen')

# sample code
idx=sample(1:500,250)
y[idx]=y[idx]+rnorm(length(idx))
plot(x,y,col='yellowgreen',pch='.')

# DNA sequence
DNA <-sample(c('C','G','T','A'),100,replace=T,prob = c(0.25,0.3,0.2,0.25))
paste(DNA,collapse = '')
paste0(DNA,collapse = '')
as.character(DNA,collapse = '')
DNA<-paste(DNA,collapse = '') # sep = can be used to separate the character in DNA, 
                              #while collapse can connect all the characters together and insert the charcter in ''in the whole string
  
install.packages('BiocManager')
library('RBioinf')
randDNA(100)
