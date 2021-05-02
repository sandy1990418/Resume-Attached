setwd("D:\\1082\\商業分析\\Lecture7_data")
##Classification trees
##Supreme Court Cases
# Read in the data
Stevens = read.csv("Stevens.csv")
str(Stevens)

# Split the data
library(caTools)
set.seed(9527)
#sample.split 確保資料在train跟test的0、1比例不會差太多
spl = sample.split(Stevens$Reverse, SplitRatio = 0.7)
Train = subset(Stevens, spl==TRUE)
Test = subset(Stevens, spl==FALSE)

# Install rpart library
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(partykit)

# CART model
#minbucket=25  =  分割後希望葉子至少有多少觀察值
#minbucket不要設太小

##mini bucket =>要形成一個葉子需要超過多少資料點
##mini splite =>至少有多少資料點才能繼續往下切
#mini bucket、mini splite小則模型越複雜
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                    LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25,
                    parms = list(split="information"))
StevensTree_gini = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                      LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25,
                    parms = list(split="gini"))

plot(StevensTree)
text(StevensTree,pretty=0)


##框框下的數字有多少是有多少不是(資料被拆成怎麼樣)
prp(StevensTree)
prp(StevensTree, type = 1, extra = 1, split.font = 1, varlen = -10)

plot(as.party(StevensTree))
plot(as.party(StevensTree),type="simple")
plot(as.party(StevensTree),type="extended")
# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
pred = prediction(PredictROC[,2], Test$Reverse)
x11(width=12,height=5)
par(mfrow=c(1,2))
plot(performance(pred, "acc"),col='green',lty=3,lwd=3)
plot(performance(pred, "tpr", "fpr"),col='green',lty=3,lwd=3)
abline(0,1,lty=2)


##Regression trees
##Boston Housing 
# Read in data
Boston = read.csv("Boston.csv")
str(Boston)

# Plot observations
plot(Boston$LON, Boston$LAT)

# Tracts alongside the Charles River
points(Boston$LON[Boston$CHAS==1], Boston$LAT[Boston$CHAS==1], 
       col="blue", pch=19)

# Plot MIT
points(Boston$LON[Boston$TRACT==3531],Boston$LAT[Boston$TRACT==3531],
       col="red", pch=20)

# Plot polution
summary(Boston$NOX)
points(Boston$LON[Boston$NOX>=0.55], Boston$LAT[Boston$NOX>=0.55], 
       col="green", pch=20)

# Plot prices
plot(Boston$LON, Boston$LAT)
summary(Boston$MEDV)
points(Boston$LON[Boston$MEDV>=21.2], Boston$LAT[Boston$MEDV>=21.2], 
       col="red", pch=20)


# Load CART packages
library(rpart)
library(rpart.plot)
library(caTools)

# CART model
latlontree = rpart(MEDV ~ LAT + LON, data=Boston)
prp(latlontree)

# Visualize output
plot(Boston$LON, Boston$LAT)
points(Boston$LON[Boston$MEDV>=21.2], Boston$LAT[Boston$MEDV>=21.2], 
       col="red", pch=20)

fittedvalues = predict(latlontree)
points(Boston$LON[fittedvalues>21.2], Boston$LAT[fittedvalues>=21.2], 
       col="blue", pch="$")

# Simplify tree by increasing minbucket
latlontree = rpart(MEDV ~ LAT + LON, data=Boston, minbucket=50)
plot(latlontree)
text(latlontree)


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


latlontree = rpart(MEDV ~ LAT + LON, data=train)
prp(latlontree)
latlon.pred = predict(latlontree, newdata=test)
latlon.sse = sum((latlon.pred - test$MEDV)^2)
latlon.sse


# Create a CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
             AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)

plot(tree)
text(tree,pretty=0)

plot(as.party(tree))


# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse
#rpart.control 
#xval=cross validation 有多少個
#cp = regulization (=penalty ->處罰模型)，電腦繼續往下切割所要的最低改進要求
#cp=0.03  =>除非模型改進幅度大於3%，否則不繼續往下切
#cp越小=>條件比較寬鬆，模型越複雜

cv.tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
                AGE + DIS + RAD + TAX + PTRATIO, data=train,
                cp=0.03)
prp(cv.tree)

##cp =改進幅度
##error比起一開始的訓練錯誤剩下多少百分比
##xerror 為cross validation的error(看這個)
##ex切第一次，  0.502868+0.49713  約等於1 

printcp(cv.tree)



plot(cv.tree$cptable[,"nsplit"], 
     cv.tree$cptable[,"xerror"],type='o',pch=1)


##prune=把樹修改成比較簡單的，cp=改進幅度條件設為0.03
treex=prune(cv.tree,cp=0.03)
prp(treex)
prune(cv.tree, cp = cv.tree$cptable[
  which.min(cv.tree$cptable[,"xerror"]),"CP"])


cv.tree.pred = predict(treex, newdata=test)
cv.tree.sse = sum((cv.tree.pred - test$MEDV)^2)
cv.tree.sse


##Below is an alternative way to do the cp search
# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)

# What did we just do?
1*0.001 
10*0.001 
0:10
0:10 * 0.001

# Cross-validation
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + 
            AGE + DIS + RAD + TAX + PTRATIO, data = train, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Make predictions
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse







