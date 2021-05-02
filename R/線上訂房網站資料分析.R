setwd("D:\\1082\\商業分析\\期末報告")


##############  read packages

##data tidy
library(tidyverse)
library(dplyr)
##excel
library(readxl)
##fread
library(data.table)
##EDA
library(DataExplorer)
##EDA REPORT
library(dlookr)
##map  visualization
library(sf)
library(mapview)
library(maps)
library(viridis)
library(fields)
library(ggrepel)

##model
library(ROCR)
library(ggplot2)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(partykit)
library(randomForest)
library(gbm)
library(ROCR)
library(glmnet)
library(h2o)
require(caret)
require(e1071)
library( ModelMetrics)
library(Metrics)


##  smote to deal  imbalanced problem
library(UBL)

## interpret  model 
library(lime)


##############  read data set
booking_data <- fread("booking_0614.csv", encoding = 'UTF-8')
booking<-read_excel("clust0614.xlsx")
booking<-fread("booking_0614(1).csv")


## convert to factor
which(colnames(booking_data)=="hotel_type_code")
which(colnames(booking)=="sq_foot")
which(colnames(booking)=="hotel_number")

booking <- merge(booking_data,booking,by=c("hotel_title","hotel_type_code"),all.x=TRUE)

x<-c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" )

for (i in x){
  booking[[i]]<-as.factor(booking[[i]])
}


write.csv(booking,"booking0614.csv")
##get data 
booking<-booking_data[,-c(2:16)]
booking<-booking[complete.cases(booking[ , "hotel_total_review"]),]


which(colnames(booking)=="hotel_address")
eda_data<- booking[,-c(1,2,4)]
##delete rowa that hotel_review is na
eda_data<-eda_data[complete.cases(eda_data[ , 1]),]



#####################     Eda Report
#https://www.rdocumentation.org/packages/dlookr/versions/0.3.13/topics/eda_report
eda_report(eda_data,mean_price, output_format = "html", output_file = "EDA_booking0615.html",output_dir="D:\\1082\\商業分析\\期末報告",na.omit=TRUE)
DataExplorer::create_report(data=eda_data,output_file = "booking_data_report.html")


##mean_price  density plot
which(colnames(booking_data)=="hotel_type_code")
booking_data$cluster<-as.factor(booking_data$cluster)
qplot(mean_price, data = booking_data,color = cluster,geom = c("density"),main = "Density Plot",ylab = "density")

##high cp mean_price density plot
high_cp <- booking[booking$group==3,]
high_cp_eda_data <- eda_data[eda_data$cluster==3,]
high_cp_eda_data <-high_cp_eda_data[,-c("早餐","cluster","ac")]

qplot(mean_price, data = high_cp ,geom = c("density"),ylab = "density",main = "High cp Density Plot")
eda_report(high_cp_eda_data,mean_price, output_format = "html", output_file = "EDA_booking_high_cp.html",output_dir="D:\\1082\\商業分析\\期末報告",na.omit=TRUE)
DataExplorer::create_report(data=high_cp,output_file = "booking_data_high_cp_report.html")




####################  map graph 


##convet to chinese
booking$location_ch <- ifelse(booking$hotel_location ==1,"東區",
                              ifelse(booking$hotel_location == 2,"西區",
                                     ifelse(booking$hotel_location == 3,"南區",
                                            ifelse(booking$hotel_location == 4,"北區",
                                                   ifelse(booking$hotel_location == 5,"中區",
                                                          ifelse(booking$hotel_location == 6,"西屯區",
                                                                 ifelse(booking$hotel_location == 7,"太平區","NA")))))))

##get number
booking%>%
  group_by(location_ch)%>%
  mutate(count_all = n())->booking
booking_map <- booking[,c("location_ch","count_all","lon","lat")]
booking_map%>%
  distinct(location_ch,count_all,.keep_all = TRUE)->booking_map




##merge  lon and lat
shp <- st_read("D:\\1082\\商業分析\\期末報告\\map\\TOWN_MOI_1090324.shp")
Taichung<-shp[shp$COUNTYNAME =="臺中市",c("TOWNNAME","geometry")]
head(Taichung)
booking_map<-left_join(booking_map,Taichung,by= c("location_ch"= "TOWNNAME"))
booking_map<-booking_map[is.na(booking_map$location_ch)!=TRUE,]

location <-c("東區","西區","南區", "北區","中區","西屯區","太平區")
long <-c(120.7,120.66,120.67,120.675,120.6775,120.625,120.77)
lat <-c(24.14,24.145,24.125,24.16,24.143,24.18,24.12)

booking_map[booking_map$location_ch=="東區" , "lon"]<-long[1]
booking_map[booking_map$location_ch=="東區" , "lat"]<-lat[1]

booking_map[booking_map$location_ch=="西區" , "lon"]<-long[2]
booking_map[booking_map$location_ch=="西區" , "lat"]<-lat[2]

booking_map[booking_map$location_ch=="南區" , "lon"]<-long[3]
booking_map[booking_map$location_ch=="南區" , "lat"]<-lat[3]


booking_map[booking_map$location_ch=="北區" , "lon"]<-long[4]
booking_map[booking_map$location_ch=="北區" , "lat"]<-lat[4]

booking_map[booking_map$location_ch=="中區" , "lon"]<-long[5]
booking_map[booking_map$location_ch=="中區" , "lat"]<-lat[5]

booking_map[booking_map$location_ch=="西屯區" , "lon"]<-long[6]
booking_map[booking_map$location_ch=="西屯區" , "lat"]<-lat[6]

booking_map[booking_map$location_ch=="太平區" , "lon"]<-long[7]
booking_map[booking_map$location_ch=="太平區" , "lat"]<-lat[7]


head(booking_map)
booking_map<- as_tibble(booking_map)
class(booking_map)




#http://www.hmwu.idv.tw/web/R/E04-hmwu_R-Map.pdf
###### graph 
ggplot(booking_map,aes(fill = count_all))+
  geom_sf(aes(geometry = geometry))+
  geom_sf_text(aes(label = location_ch, geometry = geometry), fun.geometry = st_centroid)+
  geom_sf_text(aes(label = format(count_all), geometry = geometry),colour = "grey10", size = 4,position = position_nudge(y = -0.0069))+
  scale_fill_distiller(palette = "Spectral",name = "間數")+
  labs(title="台中市飯店區域分布圖", x ="經度", y = "緯度")->graph
graph




set.seed(42)
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
ggplot(booking_map,aes(fill = count_all))+
  geom_sf(aes(geometry = geometry))+
  geom_text_repel(data=booking_map,aes(x = lon, y = lat,label = location_ch),
                      fontface = "bold",max.iter=0.051)+
  scale_fill_distiller(palette = "Spectral",name = "間數")+
  labs(title="台中市飯店區域分布圖", x ="經度", y = "緯度")->graph
graph


###################      decision tree    ################### 
which(colnames(booking)=="mean_price")
which(colnames(booking)=="count_all")
which(colnames(booking)=="hotel_address")
which(colnames(booking)=="sq_foot")
high_cp<-booking[booking$cluster==3,c("mean_price","hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" )]

##沒有排除房間尺吋
High_cp_Tree <- rpart(mean_price~ ., 
                  data = high_cp, minbucket=3,cp=0.03)
rparty.tree<-plot(as.party(High_cp_Tree))
as.party(High_cp_Tree)
#http://s3.amazonaws.com/assets.datacamp.com/production/course_3022/slides/chapter2.pdf

pred_tree <- predict(object = High_cp_Tree, 
                     newdata = high_cp)
#mse
mse(high_cp$mean_price, pred_tree)
#rmse
rmse(high_cp$mean_price, pred_tree)
#mae
mae(high_cp$mean_price, pred_tree)



##排除房間尺吋
high_cp_no_sq<-booking[booking$group==3,c("mean_price","hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" )]
high_cp_no_sq<-high_cp_no_sq[complete.cases(high_cp_no_sq),]
High_cp_no_sq_Tree <- rpart(mean_price~ ., 
                  data = high_cp_no_sq, minbucket=3,cp=0.03)

na.omit(high_cp_no_sq)
rparty.tree<-plot(as.party(High_cp_no_sq_Tree))
as.party(High_cp_no_sq_Tree)
pred_tree <- predict(object = High_cp_no_sq_Tree, 
                     newdata = high_cp_no_sq)
#mse
mse(high_cp_no_sq$mean_price, pred_tree)
#rmse
rmse(high_cp_no_sq$mean_price, pred_tree)
#mae
mae(high_cp_no_sq$mean_price, pred_tree)



###################      random forest    ################### 
set.seed(1)

##high cp with sq_foot
hight_cp_rf<-randomForest(mean_price~.,
                          data=high_cp,nodesize=3,ntree=67, importance=TRUE, na.action = na.roughfix)
importance(hight_cp_rf)
varImpPlot(hight_cp_rf)
#https://www.jamleecute.com/random-forests-%E9%9A%A8%E6%A9%9F%E6%A3%AE%E6%9E%97/
plot(hight_cp_rf) 
which.min(hight_cp_rf$mse)
sqrt(hight_cp_rf$mse[which.min(hight_cp_rf$mse)])

##mse
min(hight_cp_rf$mse)
##rmse
sqrt(min(hight_cp_rf$mse))
##mae
high_cp1<-high_cp[complete.cases(high_cp),]
y_hat_rf <- predict(hight_cp_rf,high_cp1,na.action = na.roughfix)
mae(high_cp1$mean_price, y_hat_rf)



##high cp without sq_foot
high_cp_no_sq_rf<-randomForest(mean_price~.,
                          data= high_cp_no_sq,nodesize=4,ntree=25, importance=TRUE)
importance(high_cp_no_sq_rf)
varImpPlot(high_cp_no_sq_rf)
plot(high_cp_no_sq_rf) 
which.min(high_cp_no_sq_rf$mse)
sqrt(high_cp_no_sq_rf$mse[which.min(high_cp_no_sq_rf$mse)])

##mse
min(high_cp_no_sq_rf$mse)
##rmse
sqrt(min(high_cp_no_sq_rf$mse))
##mae
high_cp_no_sq1<-high_cp_no_sq[complete.cases(high_cp_no_sq),]
y_hat_rf <- predict(high_cp_no_sq_rf,high_cp_no_sq1,na.action = na.roughfix)
mae(high_cp_no_sq1$mean_price, y_hat_rf)



###################      gbm    ################### 

high_cp_gbm<- gbm(mean_price~ .,
              data=high_cp, distribution="gaussian",
              n.trees=1200,interaction.depth=4,shrinkage=0.03)
print(high_cp_gbm)
summary(high_cp_gbm)
##mse
min(high_cp_gbm$train.error)
##rmse
sqrt(min(high_cp_gbm$train.error))
y_hat_xgb <- predict(high_cp_gbm, high_cp,n.trees=1200)
print(paste("xgb", mae(high_cp$mean_price, y_hat_xgb)))

### WITHOUT SQ_FOOT
high_cp_no_sq_gbm<- gbm(mean_price~ .,
                    data=high_cp_no_sq, distribution="gaussian",
                    n.trees=1500,interaction.depth=4,shrinkage=0.0000001)
print(high_cp_no_sq_gbm)
summary(high_cp_no_sq_gbm)
##mse
min(high_cp_no_sq_gbm$train.error)
##rms
sqrt(min(high_cp_no_sq_gbm$train.error))
y_hat_xgb <- predict(high_cp_no_sq_gbm, high_cp_no_sq,n.trees=1200)
print(paste("xgb", mae(high_cp_no_sq$mean_price, y_hat_xgb)))



###################      h2o decision tree    ################### 
h2o.init(nthreads = -2)


######　　decision tree with sq_foot
h2o_high_cp_tree <-h2o.randomForest(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                    training_frame =  h2o_high_cp, ntree = 1, sample_rate = 1,min_rows= 1,max_depth = 1000,seed=1,stopping_tolerance=0.01)
summary(h2o_high_cp_tree)
h2o.varimp(h2o_high_cp_tree)
h2o.varimp_plot(h2o_high_cp_tree,num_of_features = 21)


######　　decision tree without sq_foot
h2o_high_cp_no_sq_tree <-h2o.randomForest(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                        training_frame =  h2o_high_cp_no_sq,ntree = 1, sample_rate = 1,min_rows= 1, max_depth = 1000,seed=1,stopping_tolerance=0.01)
h2o.getModel(h2o_high_cp_no_sq_tree@model_id)
h2o.performance(h2o_high_cp_no_sq_tree)
h2o.varimp(h2o_high_cp_no_sq_tree)
h2o.varimp_plot(h2o_high_cp_no_sq_tree,num_of_features = 20)





###################      h2o random forest    ###################


##high cp with sq foot
h2o_high_cp <-as.h2o(high_cp)
c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" )
h2o_high_cp_rf <-h2o.randomForest(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                   training_frame =  h2o_high_cp, ntree = 1200, max_depth = 1000,seed=1,stopping_tolerance=0.01)
h2o.getModel(h2o_high_cp_rf@model_id)
h2o.performance(h2o_high_cp_rf)
h2o.varimp(h2o_high_cp_rf)
h2o.varimp_plot(h2o_high_cp_rf,num_of_features = 21)
h2o.mse(h2o_high_cp_rf)



##high cp without sq foot
h2o_high_cp_no_sq <-as.h2o(high_cp_no_sq)
h2o_high_cp_no_sq_rf <-h2o.randomForest(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                        training_frame =  h2o_high_cp_no_sq, ntree = 500, max_depth = 100,seed=1,stopping_tolerance=0.01)
h2o.getModel(h2o_high_cp_no_sq_rf@model_id)
h2o.performance(h2o_high_cp_no_sq_rf)
h2o.varimp(h2o_high_cp_no_sq_rf)
h2o.varimp_plot(h2o_high_cp_no_sq_rf,num_of_features = 20)

###################      h2o gbm    ###################


##high cp  with sq_foot 
h2o_high_cp_gbm <-h2o.gbm(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                         training_frame =  h2o_high_cp, ntree = 1200, max_depth = 100,seed=1,stopping_tolerance=0.01,nfolds = 10)
h2o.performance(h2o_high_cp_gbm)
h2o.varimp(h2o_high_cp_gbm)
h2o.varimp_plot(h2o_high_cp_gbm,num_of_features = 20)



##high cp  without sq_foot 
h2o_high_cp_no_sq_gbm <-h2o.gbm(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                training_frame = h2o_high_cp_no_sq, ntree = 1200, max_depth = 100,seed=1,stopping_tolerance=0.01,nfolds = 10)
h2o.performance(h2o_high_cp_no_sq_gbm)
h2o.varimp(h2o_high_cp_no_sq_gbm)
h2o.varimp_plot(h2o_high_cp_no_sq_gbm,num_of_features = 20)
h2o.getModelTree(h2o_high_cp_no_sq_gbm, tree_number=1200)



###################      h2o automl    ###################
#https://www.kaggle.com/sajal0jain/h2o-automl-in-r-deep-learning-model
##high cp  with sq_foot 
h2o_high_cp_auto <-h2o.automl(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"sq_foot","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                  training_frame =  h2o_high_cp,nfolds=5,stopping_tolerance=0.01, max_runtime_secs = 30)


h2o_high_cp_auto@leaderboard
h2o_high_cp_auto@leader

#https://stackoverflow.com/questions/54852453/how-to-print-variable-importance-of-all-the-models-in-the-leaderboard-of-h2o-aut
# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(h2o_high_cp_auto@leaderboard$model_id)[1,1]

# View variable importance for all the models (besides Stacked Ensemble)
for (model_id in model_ids) {
  print(model_id)
  m <- h2o.getModel(model_id)
  print(h2o.varimp(m))
  h2o.varimp_plot(m,num_of_features = 20)
}



##high cp  without sq_foot 
h2o_high_cp_no_sq_auto <-h2o.automl(y = "mean_price", x = c("hotel_type_code","hotel_location","hotel_cancel","hotel_room_type","ac","view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" ), 
                                    training_frame = h2o_high_cp_no_sq,nfolds=5,stopping_tolerance=0.01, max_runtime_secs = 30)
h2o_high_cp_no_sq_auto@leaderboard
h2o_high_cp_no_sq_auto@leader



model_ids <- as.data.frame(h2o_high_cp_no_sq_auto@leaderboard$model_id)[1,1]

# View variable importance for all the models (besides Stacked Ensemble)
for (model_id in model_ids) {
  print(model_id)
  m <- h2o.getModel(model_id)
  print(h2o.varimp(m))
  h2o.varimp_plot(m,num_of_features = 20)
}




booking<-read_excel("clust.xlsx")



##################    sample splite
booking<-as.data.frame(booking)
train<-c()
test<-c()
for (i in c(1:4)){
  set.seed(1)
  temp<-c()
  train_temp <-c()
  test_temp <-c()
  temp <- booking[booking$cluster==i,]
  sample <- sample.split(temp, SplitRatio = 0.7)
  train_temp <- subset(temp, sample==TRUE)
  test_temp <- subset(temp, sample==FALSE)
  train<-rbind(train,train_temp)
  test<-rbind(test,test_temp)
}

train<-as.data.frame(train)
train<-train[,c(5:12,22)]
train<- train[complete.cases(train),]
train[,c(9)]<-as.factor(train[,c(9)])
##看一下各群比例
prop.table(table(train[,c(9)]))
train1<-SmoteClassif(cluster~., train ,C.perc = "balance")
##看一下各群比例
prop.table(table(train1[,c(11)]))
label<-train[,c(9)]
label1<-train1[,c(8)]


test<-as.data.frame(test)
test<-test[,c(5:12,22)]
test<-test[is.na(test$hotel_total_review)!=TRUE,]
test_label<-test[,c(9)]
test_label<-as.factor(test_label)

###################      knn    ###################
#https://blog.csdn.net/weixin_43408110/article/details/87559323

library(class)
library(dplyr)


## train without using smote method
cluster_knn <- c()
error.rate <- c()

for(i in 1:30){
  set.seed(100)
  cluster_knn<-knn(train[c(1:7)], test = test[,c(1:7)],as.vector(label),i)
  error.rate[i] <- mean(test_label!=cluster_knn)
  #caret::confusionMatrix(cluster_knn,test_label)
}
print(error.rate)

k.values <- 1:30
error.df <- data.frame(error.rate,k.values)
error.df
ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
caret::confusionMatrix(cluster_knn, test_label)

###  train with  smote method
cluster_knn <- c()
error.rate <- c()

for(i in 1:30){
  set.seed(100)
  cluster_knn<-knn(train1[c(1:7)], test = test[,c(1:7)],as.vector(label1),i)
  error.rate[i] <- mean(test_label!=cluster_knn)
  #caret::confusionMatrix(cluster_knn,test_label)
}
print(error.rate)

k.values <- 1:30
error.df <- data.frame(error.rate,k.values)
error.df
ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
caret::confusionMatrix(cluster_knn, test_label)








###### decision tree model

##without smote
Tree <- rpart(cluster~ ., 
              data = train, method="class", minbucket=5,
              parms = list(split="information"))

Tree_pred <-predict(Tree,test,type = "class")
caret::confusionMatrix( Tree_pred , test_label)
rparty.Tree<-plot(as.party(Tree))
printcp(Tree)

caret::confusionMatrix(Tree_pred , test_label)

#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Validation_Croisee_Suite.pdf
###### decision  tree cross
train_control <- trainControl(method="cv", number=100)
train_control.model <- train(cluster~ ., data=train, method="rpart", trControl=train_control)
train_control.model
plot(train_control.model)




##with smote
Tree_smote <- rpart(cluster~ ., 
                    data = train1, method="class", minbucket=5,
                    parms = list(split="information"))

Tree_pred_smote  <-predict(Tree_smote ,test,type = "class")
caret::confusionMatrix( Tree_pred_smote  , test_label)
rparty.Tree_smote <-plot(as.party(Tree_smote ))
printcp(Tree_smote )

#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Validation_Croisee_Suite.pdf
###### decision  tree cross
train_control_smote  <- trainControl(method="cv", number=100)
train_control.model_smote  <- train(cluster~ ., data=train1, method="rpart", trControl=train_control)
train_control.model_smote 
plot(train_control.model_smote )



######################################################
##PREDICT PRICE MODEL 

##################    sample splite
booking<-read_excel("clust.xlsx")

booking<-as.data.frame(booking)
train<-c()
test<-c()
for (i in c(1,2,4)){
  set.seed(1)
  temp<-c()
  train_temp <-c()
  test_temp <-c()
  temp <- booking[booking$cluster==i,]
  sample <- sample.split(temp, SplitRatio = 0.7)
  train_temp <- subset(temp, sample==TRUE)
  test_temp <- subset(temp, sample==FALSE)
  train<-rbind(train,train_temp)
  test<-rbind(test,test_temp)
}
c("mean_price","hotel_total_review","hotel_review","員工素質","清潔程度","住宿地點","性價比","設施","舒適程度","hotel_title","hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material")
train<-as.data.frame(train)
train<-train[,c("hotel_total_review","hotel_review","員工素質","清潔程度","住宿地點","性價比","設施","舒適程度","hotel_title")]
Tree_pred <-as.data.frame(predict(Tree,train,type = "class"))
predict<-cbind(train,Tree_pred)

colnames(predict)[10]=c("class")

predict%>%
  filter(class==3)->predic_high

booking<-fread("booking_0614(1).csv")

predic_high_booking<-merge(predic_high,booking,by="hotel_title",aall.x = TRUE)

predic_high_booking<-predic_high_booking[,c("mean_price","hotel_type_code","hotel_location","hotel_cancel","hotel_room_type" ,"view","soundpf","wifi","tv","bathroom","balcony","toiletries","socket","towel","media","high","upstairs","bathroom_equip","floor_material" )]


pred_tree <- predict(object = High_cp_no_sq_Tree, 
                     newdata = predic_high_booking)
#mse
mse(predic_high_booking$mean_price, pred_tree)
#rmse
rmse(predic_high_booking$mean_price, pred_tree)
#mae
mae(predic_high_booking$mean_price, pred_tree)


predic_high_booking<-cbind(predic_high_booking,pred_tree)

write.csv(predic_high_booking,"predic_high_booking.csv")







'''
#######get lon and  lat
get_lon_lat<-function(){
  location <-c("東區","西區","南區", "北區","中區","西屯區","太平區") 
  lon_lat<-c()
  lis<-list()
  w <- 1
  for (i in c(336,337,333,340,335,341,338)){
    l<- as.data.frame(shp$geometry[[i]][1])[1,]
    colnames(l)<-c("lon","lat")
    lis[[w]]<-l
    w<-w+1
  }
  for(j in c(1:7)){
    temp<-c()
    temp<-lis[[j]]
    temp$location_ch<-location[j]
    lon_lat<-rbind(lon_lat,temp)
  }
  lon_lat<<-lon_lat
  return(lon_lat)
}
lon_lat<-get_lon_lat()

##get number of hotel type
booking%>%
  group_by(location_ch,hotel_type_code)%>%
  mutate(count_hotel_type = n())->booking
merge(booking,lon_lat,by=c("location_ch"),all.x=TRUE)->booking
booking_map1 <- booking[,c("location_ch","hotel_type_code","count_hotel_type","lon","lat")]
booking_map1%>%
  distinct(location_ch,hotel_type_code,count_all_hotel_type,.keep_all = TRUE)->booking_map1
booking_map1<-booking_map1[-23,]
'''


get_lon_lat<-function(){
  location <-c("東區","西區","南區", "北區","中區","西屯區","太平區") 
  lon_lat<-c()
  lis<-list()
  w <- 1
  for (i in c(336,337,333,340,335,341,338)){
    l<- as.data.frame(shp$geometry[[i]][1])[200,]
    colnames(l)<-c("lon","lat")
    lis[[w]]<-l
    w<-w+1
  }
  for(j in c(1:7)){
    temp<-c()
    temp<-lis[[j]]
    temp$location_ch<-location[j]
    lon_lat<-rbind(lon_lat,temp)
  }
  lon_lat<<-lon_lat
  return(lon_lat)
}
lon_lat<-get_lon_lat()

merge(booking,lon_lat,by=c("location_ch"),all.x=TRUE)->booking
