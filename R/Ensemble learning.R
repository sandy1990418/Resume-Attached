setwd("D:\\1082\\商業分析\\Lecture8_data")
##Classification trees
##Supreme Court Cases
# Read in the data
Stevens = read.csv("Stevens.csv")
str(Stevens)

# Split the data
library(caTools)
library(rpart)
library(rpart.plot)
set.seed(9527)
spl = sample.split(Stevens$Reverse, SplitRatio = 0.7)
Train = subset(Stevens, spl==TRUE)
Test = subset(Stevens, spl==FALSE)

# Build a decision tree model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                    LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25,
                    parms = list(split="information"))

library(randomForest)
# Build a random forest model
##ntree = 多少棵樹
##nodesize = 每棵樹的minbucket
##mtry = how many variable for each tree

#如果沒有as.factor(Reverse)則電腦會把reverse當作連續型變數，有as.factor就會變成分類樹
#random forest的結果是由每顆tree的投票(1、0)以多數決方式決定
StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                            Petitioner + Respondent + LowerCourt + Unconst,
                             data = Train, ntree=200, nodesize=25)

# ROC curve
library(ROCR)
PredictTree = predict(StevensTree, newdata = Test, type = "prob")
PredictForest = predict(StevensForest, newdata = Test, type = "prob")
predTree = prediction(PredictTree[,2], Test$Reverse)
predForest = prediction(PredictForest[,2], Test$Reverse)
x11(width=8,height=5)
plot(performance(predTree, "tpr", "fpr"),col='green',lty=3,lwd=3)
plot(performance(predForest, "tpr", "fpr"),col='red',add=T,lty=3,lwd=3)
abline(0,1,lty=2)

performance(predTree, "auc")
performance(predForest, "auc")

##要有importance=TRUE，電腦會自動做validation，變數才會是對的
StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + Unconst,
                             data = Train, ntree=500, nodesize=25,
                             importance=TRUE)


##mean decrease gini看訓練階段的表現(在訓練階段哪個變數平均降低不確定最多的)
#哪個變數可以讓不確定性減少最多，電腦看有哪幾顆樹有用到該變數，
#使用該變數的樹不確定性降低多少，最後平均


##mean decrease accuracy看預測階段的表現(來自validation)
#少了哪個變數在預測時平均會減少多少準確率
varImpPlot(StevensForest)



##Regression trees
##Boston Housing 
# Read in data
Boston = read.csv("Boston.csv")
str(Boston)

# Load CART packages
library(rpart)
library(rpart.plot)
library(caTools)

# Let's use all the variables
# Split the data
set.seed(123)
split = sample.split(Boston$MEDV, SplitRatio = 0.7)
train = subset(Boston, split==TRUE)
test = subset(Boston, split==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + 
          AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

# Make predictions
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse

#Train a random forest model
library(randomForest)
set.seed(5566)
#random forest的結果是由每顆tree的y hat取平均
rf.boston=randomForest(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                       NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
                       data=train, mtry=6, ntree=200, importance=TRUE)
rf.pred = predict(rf.boston, newdata=test)
rf.sse = sum((rf.pred - test$MEDV)^2)
rf.sse

##Inc Node Purity看訓練階段的表現
#變數對於組內變異的改善(降低)

##%Inc MSE(percent increase in MSE)看預測階段的表現
#哪個變數會最有預測力
#每個變數亂排(只亂排該變數其他不動)之後在Validation的錯誤增加狀況
varImpPlot(rf.boston)
importance(rf.boston)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
               AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)
# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

##A bagging only forest(每顆樹的變數都一樣)
bag.boston=randomForest(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                         NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
                         data=train, mtry=13, ntree=200)
bag.boston
bag.pred = predict(bag.boston, newdata=test)
bag.sse = sum((bag.pred - test$MEDV)^2)
bag.sse


library(gbm)
#interaction.depth  =控制森林內的樹的複雜度(樹的高度)
#distribution=讓樹知道做回歸還是分類，gaussian = 做迴歸數，
gbm.boston=gbm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
             NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
             data=train, distribution="gaussian",
             n.trees=5000,interaction.depth=4)
gbm.pred=predict(gbm.boston,newdata=test,
                 n.trees=5000)
gbm.sse = sum((gbm.pred - test$MEDV)^2)
gbm.sse


##相對影響力
summary(gbm.boston)


##畫出模型的預測圖
plot(gbm.boston,i="RM")
plot(gbm.boston,i="CRIM")


gbm.boston=gbm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
                data=train, distribution="gaussian",
                n.trees=5000,interaction.depth=4,
                shrinkage=0.2,cv.folds=5)
##The best cross-validation iteration was 525=根據設的Lambda(0.2)及cv，525顆樹就夠了
##There were 13 predictors of which 13 had non-zero influence = 多少變數是非0的影響力
print(gbm.boston)
##n.trees=  ，沒有說用多少顆，則他會自己用最適合的棵數
gbm.pred=predict(gbm.boston,newdata=test,
                 n.trees= )
gbm.sse = sum((gbm.pred - test$MEDV)^2)
gbm.sse




##Analyze Bank.csv
Bank = read.csv("Bank.csv",header=TRUE,sep = ";")
colnames(Bank)
Bank.df = Bank[,c(1:8,17)]
colnames(Bank.df)
Bank.df[,9]=ifelse(Bank.df[,9]=="yes",1,0)

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(partykit)

set.seed(9527)
spl = sample.split(Bank.df$y, SplitRatio = 0.7)
Train = subset(Bank.df, spl==TRUE)
Test = subset(Bank.df, spl==FALSE)


#Fit a logistic regression model
logit.Bank=glm(y~., data=Train, family=binomial(link="logit"))
summary(logit.Bank)
logit.out=predict(logit.Bank,newdata=Test, type="response")
table(Bank$y)
tree.Bank=rpart(y ~ age + job + marital + education + default + balance
                + housing + loan , data = Train, method="class", 
                minbucket=25, parms = list(split="information"))
prp(tree.Bank)

tree.Bank=rpart(y ~ age + job + marital + education + default + balance
                + housing + loan , data = Train, method="class", 
                minbucket=5, parms = list(split="information"),cp=0.0005)
prp(tree.Bank)

tree.Bank=rpart(y ~ age + job + marital + education + default + balance
                + housing + loan , data = Train, method="class", 
                minbucket=30, parms = list(split="information"),cp=0.0006)
prp(tree.Bank)


plot(as.party(tree.Bank))
tree.out = predict(tree.Bank, newdata=Test, type="prob")

# Build a random forest model
rf.Bank = randomForest(as.factor(y)~ age + job + marital + education + default + balance
                       + housing + loan, data = Train, ntree=500, 
                       nodesize=25, importance=TRUE)
varImpPlot(rf.Bank)
rf.out=predict(rf.Bank, newdata = Test, type = "prob")


# bernoulli  =分類問題
gbm.Bank=gbm(y ~ age + job + marital + education + default + balance
             + housing + loan, data=Train, distribution="bernoulli",
             n.trees=1500,interaction.depth=4)
print(gbm.Bank)
gbm.out=predict(gbm.Bank, newdata=Test,
                n.trees=1500, type= "response")



# ROC curve
library(ROCR)
predlogit = prediction(logit.out, Test$y)
predtree = prediction(tree.out[,2], Test$y)
predrf = prediction(rf.out[,2], Test$y)
predgbm = prediction(gbm.out, Test$y)
#
x11(width=12,height=5)
par(mfrow=c(1,2))
plot(performance(predlogit, "acc"),col='red',lty=1,lwd=3)
plot(performance(predtree, "acc"),col='blue',add=T,lty=2,lwd=3)
plot(performance(predrf, "acc"),col='green',add=T,lty=3,lwd=3)
plot(performance(predgbm, "acc"),col='yellow',add=T,lty=4,lwd=3)
#
plot(performance(predlogit, "tpr", "fpr"),col='red',lty=1,lwd=3)
plot(performance(predtree, "tpr", "fpr"),col='blue',add=T,lty=2,lwd=3)
plot(performance(predrf, "tpr", "fpr"),col='green',add=T,lty=3,lwd=3)
plot(performance(predgbm, "tpr", "fpr"),col='yellow',add=T,lty=4,lwd=3)
abline(0,1,lty=2)

performance(predlogit, "auc")
performance(predtree, "auc")
performance(predrf, "auc")
performance(predgbm, "auc")



