library(arules)
library(car)
library(arulesViz)
library(recommenderlab)
library(data.table)
setwd("D:\\1082\\商業分析\\Lecture2_data")

CharlesBookClub<-fread("CharlesBookClub.csv")
jester<-fread("jester-data-3.csv")
retail.raw<-fread("retail.txt")


###example 1
#install.packages('arules')
library(arules)
data('Groceries') #embeded data in the package
inspect(head(Groceries,3))
summary(Groceries)
groc.rules=apriori(Groceries, parameter = list(supp=0.01,conf=0.3,target='rules'))
inspect(subset(groc.rules,lift>3))

###example 2
##raw data processing
#each transaction as one line in file
#read each line into retail.raw separate by " "
retail.raw <- readLines(con=file("D:\\1082\\商業分析\\Lecture2_data\\retail.txt"))
head(retail.raw)
tail(retail.raw)
summary(retail.raw)
# convert the raw character lines into a list of item vectors
retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
str(retail.list)
install.packages('car')
library(car)
some(retail.list)
rm(retail.raw)
#convert transaction list into transaction object for association rule: many rows of transaction{i1,i2}
retail.trans <- as(retail.list, "transactions")

##SAVER DATA 
saveRDS(retail.trans,"retail.trans.rds")
retail.trans<-readRDS("retail.trans.rds")

##market basket anlaysis
inspect(head(retail.trans))
#what information we can get from summary()


#transactions as itemMatrix in sparse format with
#88162(交易) rows (elements/itemsets/transactions) and
#16470(產品) columns (items) and a density of 0.0006257289 

summary(retail.trans)
rm(retail.list)

#多少條規則
#writing ... [116 rule(s)] done [0.00s].
mkt.rules=apriori(retail.trans, parameter = list(supp=0.01,conf=0.4,target='rules'))

inspect(subset(mkt.rules,lift>3))
#sort the result by lift and see the first six rules
inspect(head(sort(mkt.rules, by='lift'),n=6))
#plot rule distribution
install.packages('arulesViz')
library(arulesViz)
plot(mkt.rules)
# interactive mode though not very handy
#one click to start region and second click to end region
#hit zoom in to see the subregion
#select a region agian
#hit inspect to show rules in that region
plot(mkt.rules,engine='interactive')

###example3: 
all.books.df <- CharlesBookClub
all.books.df <- read.csv("xx\\CharlesBookClub.csv")
## create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
#remove childBks for fewer rules found (simplicity)
incid.books.mat <- as.matrix(incid.books.df[,-1])
##  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(head(books.trans))
summary(books.trans)
# plot data
itemFrequencyPlot(books.trans)
# run apriori function
rules <- apriori(books.trans, parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))
# inspect rules
inspect(sort(rules, by = "lift"))

#association rule network: products as nodes, rules as edges, and support as strentgh
#ARN is a visulization tool and newtork "analysis" can obtain additional information
test=head(sort(mkt.rules, by = "lift"),20)
plot(test,method='graph',control=list(type='items'))



###Collaborative filtering
# load CSV version of jester subset


jester <-jester_data_3
jester <- read.csv("xx\\jester-data-3.csv", header = FALSE)
dim(jester)
#select 10000 samples
trows <- sample(nrow(jester), 10000)
jester <- jester[trows,]
rm(trows)

#first column contains a count of the number of jokes a user has rated, not rating
summary(jester$V1)
# remove first column since it does not contain user ratings
jester <- jester[,2:ncol(jester)]
# set all 99's to NA
jester[,][jester[,] == 99] <- NA
min(jester[][], na.rm = TRUE)
max(jester[][], na.rm = TRUE)
hist(as.vector(as.matrix(jester)), main = "Distribution of Jester Ratings",col = "yellow", xlab = "Ratings")
#number of rated items --> related to given inputs of new data
summary(apply(jester,1,function(x){length(na.omit(x))}))

##user-based collaboratie filtering
install.packages('recommenderlab')
library(recommenderlab)
# convert the jester data frame to a matrix
rmat <- as.matrix(jester)
# convert matrix to a recommenderlab realRatingMatrix: many rows of (user, item, preferences)
rmat <- as(rmat,"realRatingMatrix")
#calculate similarity
UB.Rec <- Recommender(rmat, "UBCF")
vignette("recommenderlab") #view package tutorial
# get recommendations
pred <- predict(UB.Rec, rmat, type="ratings")
#Can you verify your prediction?
as(pred, "matrix")[8,c(1:20)]
jester[8,c(1:20)]
# default top 10
pred1 <- predict(UB.Rec, rmat)
as(pred1, "matrix")[8,]
as(pred, "matrix")[8,]

##item-based collaborative filtering
UB.Rec <- Recommender(rmat, "IBCF")


##Come back after Regression:how to predict and verify during analysis
#for test data: given random 15 items as input
e <- evaluationScheme(rmat, method="split", train=0.8, given=15)
UB.Rec <- Recommender(getData(e, "train"), "UBCF")
# compute predicted ratings
##"known"= 剛剛保留的15筆
p1 <- predict(UB.Rec, getData(e, "known"), type="ratings")
# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < -10] <- -10
p1@data@x[p1@data@x[] > 10] <- 10
#verify prediction
calcPredictionAccuracy(p1, getData(e, "unknown"))


##item-based collaborative filtering
IB.Rec <- Recommender(getData(e, "train"), "IBCF")
p2 <- predict(IB.Rec, getData(e, "known"), type="ratings")
# set all predictions that fall outside the valid range to the boundary values
p2@data@x[p1@data@x[] < -10] <- -10
p2@data@x[p1@data@x[] > 10] <- 10
#verify prediction
calcPredictionAccuracy(p2, getData(e, "unknown"))

