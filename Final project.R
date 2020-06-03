library(data.table)
library(corrplot)
library(datasets)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
#downloading the dataset
members <- fread("C:/Users/my/Desktop/members.csv")
songs <- fread("C:/Users/my/Desktop/songs.csv")
song_extra <- fread("C:/Users/my/Desktop/song_extra_info.csv")
train <- fread("C:/Users/my/Desktop/train.csv")
test <- fread("C:/Users/my/Desktop/test.csv")

#replacing the two time feature with the duration between them
members$registration_init_time<-as.Date(as.character(members$registration_init_time),"%Y%m%d")
members$expiration_date <- as.Date(as.character(members$expiration_date),"%Y%m%d")
members$membership_length <- difftime(members$expiration_date, members$registration_init_time, units="days")
members <- members[,-c(6:7)]

#choose the 1% of the train dataset
T <- nrow(train)
T_trn <- round(0.01*T)  
train_set <- train[1:T_trn, ]

#merge the "train","songs","members"
train_member <- merge(train_set,members,by="msno")
train_merged <- merge(train_member,songs,by="song_id")

#deal with the empty values
for(i in names(train_merged)){
  if(class(train_merged[[i]]) == "character"){
    train_merged[is.na(train_merged[[i]]),eval(i):=""]
    train_merged[,eval(i):=as.integer(
      as.factor(train_merged[[i]]))]
  }
  else
    train_merged[is.na(train_merged[[i]]),eval(i):= -999]
}
train_merged$membership_length <- as.numeric(train_merged$membership_length)

#draw the correlation plot
corrplot(cor(train_merged), main="\n\nCorrelation of Variables", method="circle", type="lower", order="hclust", addCoef.col = "black")

#split the train dataset into two:train_df and test_df
train1<- train_merged
data_matrix <- data.matrix(train1)
X <- data_matrix[, c(1:5,7:17)]
y <- factor(data_matrix[, 6])
set.seed(22)
intrain <- createDataPartition(y = y, p = 0.8, list=FALSE)
X_train <- X[intrain, ]
y_train <- y[intrain]
X_test <- X[-intrain, ]
y_test <- y[-intrain]
train_df <- data.frame(X_train, y_train)
test_df  <- data.frame(X_test, y_test)
names(train_df) <- c( "song_id","msno","source_system_tab","source_screen_name", "source_type","city","bd","gender","registered_via","membership_length","song_length","genre_ids" , "artist_name","composer","lyricist","language","target")
names(test_df) <- c( "song_id","msno","source_system_tab","source_screen_name", "source_type","city","bd","gender","registered_via","membership_length","song_length","genre_ids" , "artist_name","composer","lyricist","language","target")


#Decision tree
fit <- rpart(target~., data = train_df)
fit$variable.importance

set.seed(3333)
train_df$target <- factor(train_df$target)
dtree_fit <- train(target~. , train_df, method = "rpart")
y_predict <- predict(dtree_fit, newdata = test_df)
confusionMatrix(factor(y_test), factor(y_predict))

#draw the decision tree 
fit <- rpart(target~., data = train_df)
rpart.plot(fit)

#Random forest 
randomforest <- randomForest(target~. , train_df, ntree = 50,importance=TRUE)
randomforest
y_predict <- predict(randomforest, newdata = test_df)
confusionMatrix(factor(y_test), factor(y_predict))

#Boosting algorithm
gbm_model <- gbm(target~. , train_df,
                 distribution = "multinomial",
                 n.trees = 200) 
y_predict <- predict.gbm(gbm_model, newdata = test_df,
                         n.trees = 200,
                         type = "response")
predicted_labels = apply(y_predict, 1, which.max)
predicted_labels = as.factor(ifelse(predicted_labels == 2,1,0))
confusionMatrix(factor(y_test), predicted_labels)

#using the Random forest to predict the test data 
test <- test[,-1]
test_member1 <- merge(test,members,by="msno")
test_merged1 <- merge(test_member1,songs,by="song_id")
for(i in names(test_merged1)){
  if(class(test_merged1[[i]]) == "character"){
    test_merged1[is.na(test_merged1[[i]]),eval(i):=""]
    test_merged1[,eval(i):=as.integer(
      as.factor(test_merged1[[i]]))]
  }
  else
    test_merged1[is.na(test_merged1[[i]]),eval(i):= -999]
}

target<- c(rep(1,nrow(test_merged1)))
test_merged1 <- cbind(test_merged1,target)

pred1<-predict(randomforest, newdata=test_merged1)

test <- fread("C:/Users/my/Desktop/test.csv")
merged_test1 <- merge(test,members,by="msno")
merged_test2 <- merge(merged_test1,songs,by = "song_id")
pred <- as.numeric(pred1)-1
result <- cbind(merged_test2$id,pred)
names(result) <- c("id","target")
write.csv(result,file = "C:/Users/my/Desktop/result.xls")