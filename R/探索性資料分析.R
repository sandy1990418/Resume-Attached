setwd("D:\\1082\\商業分析\\Lecture1_data")


##1.2
1+2+3
3*7*2
6+7*3/2
(4*6)+5
4*(6+5)
17/5

17%%5 
17%/%5

round(98.562,1)
round(1778,-2)
round(1234,-3)

help(round)

x<-2
x
y=5
rm(x)
rm(list=ls())


##1.3 vectors
x=c(1,2,3,4)
X

xChar= c("foo", "goo", "boo", "far") 
xMix	= c(1, TRUE, "3", 4)
xMix	
x=1:4
x=seq(1, 4, by=1)

unique(x)
length(x)
sort(x,decreasing=FALSE)
sort(x,decreasing=TRUE)

x<=2
x>2
any(x<2)
all(x>2)
which(x==2)
x==2
which(x!=2)
x%in%c(4, 5, 6, 7, 8)
x=4:8
x[1]
x[1:2]
x[c(1,4)]
x=1:4
x[which(x>=2 & x<=3)]
x[which(x>=2 | x<=3)]

x=rep(0,4)
x
x=x+(1:4)
x

x**2
x^2
sqrt(x)
log(x) 
exp(x)
help(log)
#----


#----

min(x)
max(x)
sum(x)
cumsum(x)

sum(x<4)


scores=c(91,NA,NA)
sum(scores)
is.na(scores)
sum(na.omit(scores))

z=c(1,2,NULL,3,4)
z
d=NULL
is.null(z)
is.null(d)
which
which(is.NULL)

log(c(-1,0))
x=-Inf/Inf
x
is.nan(x)

##1.4 matrix
A=matrix(1:15, nrow=5, dimnames = list(c('r1', 'r2', 'r3', 'r4', 'r5'), c('c1', 'c2','c3')))
nrow(A); ncol(A); dim(A)
colnames(A); rownames(A)
head(A, n=3);tail(A, n=3)
A[3, 2:3]
#----

#----
B=matrix(16:30, nrow=5)
A+B
cbind(A, B); rbind(A, B)

mean(A)
row.means=apply(B, 1, mean)
col.means=apply(A, 2, mean)
row.means
mean(B)


##1.5 data frame
theDF=data.frame(10:1,-4:5,letters[1:10])
colnames(theDF)=c("First","Second","Third")

rm(list=ls())
results=read.table('results.csv',header=T, sep=',')
is.matrix(results);is.data.frame(results)
results$arch1[5]
arch1
attach(results)
names(results)
arch1
arch1[] # same as above
detach(results)
arch1

mean(arch1)
summary(arch1)
#----

#----

##
store.df <- read.csv('store.csv')
str(store.df)
table(store.df$country)
table(store.df$p1price)
p1.table = table(store.df$p1price)
table(store.df$p1price, store.df$p1prom)

min(store.df$p1sales)
mean(store.df$p1prom)
mad(store.df$p1sales)
IQR(store.df$p1sales) #Interquartile range, 75th?V25th percentile


mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR") # same as below
#colnames(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

str(store.df) 
store.df$storeNum2 <- factor(store.df$storeNum)#a new variable for category
str(store.df)
dim(store.df)
head(store.df)
tail(store.df)

summary(store.df)
summary(store.df$p1sales)

p1sales.sum = aggregate(store.df$p1sales,
                        by=list(country=store.df$country), sum)

p1sales.sum

install.packages('psych')
library(psych) # make it available in current window
describe(store.df)
describe(store.df[ , c(2, 4:9)]) #column 2 and 4 to 9

apply(store.df[,2:10], 2, mean) #watch for categorical variable
apply(store.df[,2:9], 2, mean)
apply(store.df[, 2:9], 2, function(x){ mean(x) - median(x) })


##1.6
#set the graph to be a new window
options(device='windows') # device='quartz' for Mac
hist(store.df$p1sales)
hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All Stores", #main title
     xlab="Product 1 Sales (Units)", #x axis label
     ylab="Count" ) # y axis label
hist(store.df$p1sales,
       main="Product 1 Weekly Sales Frequencies, All Stores", #main title
       xlab="Product 1 Sales (Units)", #x axis label
       ylab="Count" ,  # y axis label
       breaks=30, # more columns
       col="lightblue") 
hist(store.df$p1sales,
       main="Product 1 Weekly Sales Frequencies, All Stores",
       xlab="Product 1 Sales (Units)",
       ylab="Relative frequency",
       breaks=30,
       col="lightblue",
       freq=FALSE, # freq=FALSE means plot density, not counts
       xaxt="n") # xaxt="n" means "x axis tick marks == no"
axis(side=1, at=seq(60, 300, by=20))
lines(density(store.df$p1sales,bw=10), # "bw= ..." adjusts the smoothing
      type="l", col="darkred", lwd=2) # lwd = line width

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)


###1.7
cust.df <- read.csv('customers.csv')
cust.df <- read.csv('customers.csv')
str(cust.df)
##scatter plot
plot(x=cust.df$age, y=cust.df$credit.score)
plot(cust.df$age, cust.df$credit.score,
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900),
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Customer Credit Score ")
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

plot(cust.df$store.spend, cust.df$online.spend,
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)" )

plot(log(cust.df$store.spend+1), log(cust.df$online.spend+1),
     main="Customers as of June 2014",cex=0.7,
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)" )

par(mfrow=c(2, 2))
plot(cust.df$distance.to.store, cust.df$store.spend, main="store")
plot(cust.df$distance.to.store, cust.df$online.spend, main="online")
plot(cust.df$distance.to.store, cust.df$store.spend+1, log="xy",
       main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend+1, log="xy",
       main="online, log")

install.packages("gpairs") # only run once
library(gpairs)
gpairs(cust.df[ , c(2:10)]) 

##correlation statistics
cor(cust.df$age, cust.df$credit.score)
cov(cust.df$age, cust.df$credit.score) /
  (sd(cust.df$age)*sd(cust.df$credit.score))
cor.test(cust.df$age, cust.df$credit.score)
cor(cust.df[, c(2, 3, 5:12)])
--
cor(cust.df[, c(2, 3, 5:12)],use='complete.obs')
--  
library(corrplot) # for correlation plot, install if needed
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),
                 upper="ellipse", tl.pos="lt")

##Data transformation
set.seed(49931)
x <- runif(1000, min=-10, max=10)
cor(x, x^2)
cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)
plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)


###1.8 comparing groups
seg.df <- read.csv('seg.csv')
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.income.mean

seg.df$Segment
table(seg.df$Segment)
seg.income.mean[seg.df$Segment, ]
seg.df$segIncome = seg.income.mean[seg.df$Segment, 2]

aggregate(seg.df$income, list(seg.df$Segment,seg.df$ownHome), mean)
aggregate(income~Segment + ownHome, data=seg.df, mean)

##frequency
with(seg.df, table(Segment, ownHome)) #with to specify data
--

--

library(lattice) #same as library()
#more than two categories
#histogram is for proportion! 
histogram(~subscribe | Segment, data=seg.df)
histogram(subscribe~Segment  , data=seg.df) #try it: y-axis as subscribe & x axis as segment
histogram(~subscribe | Segment, data=seg.df, type="count",
           layout=c(4,1), col=c("burlywood", "darkolivegreen"))

#yes proportion in each segment--> only one category: segement
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ],
         xlab="Subscriber proportion by Segment", col="darkolivegreen")
#same idea of barplot(table object for one category)
barplot(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ])


##groups: continous data
seg.mean <- aggregate(income~Segment, data=seg.df, mean)
library(lattice)
barchart(income~Segment, data=seg.mean, col="grey")


##statistics
chisq.test(table(seg.df$Segment))
table(seg.df$subscribe,seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

hist(seg.df$income) 
with(seg.df, hist(income[ownHome=="ownYes"])) 
with(seg.df, hist(income[ownHome=="ownNo"])) 
t.test(income ~ ownHome, data=seg.df)
t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))

