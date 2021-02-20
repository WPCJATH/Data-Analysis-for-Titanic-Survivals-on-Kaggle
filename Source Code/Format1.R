library(readr)
train <- read_csv("C:/Users/Ryan/Desktop/option2_attachment/train.csv")
test <- read_csv("C:/Users/Ryan/Desktop/option2_attachment/test.csv")
names(train)      #names
test$Survived <- NA
data <- rbind(train,test)
str(data)
data$Survived <- as.factor(data$Survived)   
data$Pclass <- as.factor(data$Pclass) 
data$Sex <- as.factor(data$Sex) 
data$Embarked <- as.factor(data$Embarked)
sapply(data,function(x) sum(is.na(x)))    #the missing data
data$Fare[is.na(data$Fare)] <- median(data$Fare,na.rm = TRUE)
data[is.na(data$Embarked),]
data$Embarked[is.na(data$Embarked)] <- "C"
library(ggplot2)
ggplot(data[(1:891),], aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Pclass's impacts",x = "Pclass", y = "count")

data$Title <- sapply(data$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
data$Title <- sub(' ', '', data$Title)
table(data$Title)
data$Title[data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'   
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
data$Title[data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
data$Title <- factor(data$Title)

ggplot(data[(1:891),], aes(x = Title, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "Title's impacts",x = "Title", y = "count")

ggplot(data[(1:891),], aes(x = Sex, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "Sex's impacts",x = "Sex", y = "count")

train$Survived <- as.factor(train$Survived)
ggplot(train[!is.na(train$Age),], aes(x = Age, color = Survived)) +
  geom_line(aes(label = ..count..),stat = "bin",binwidth = 5,na.rm = TRUE) +
  labs(title = "Age's impacts",x = "Age", y = "count")

ggplot(data[(1:891),], aes(x = SibSp, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "SibSp's impacts",x = "SibSp", y = "count")

ggplot(data[(1:891),], aes(x = Parch, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "Parch's impacts",x = "Parch", y = "count")

data$FamilySize <- data$Parch + data$SibSp + 1
ggplot(data[(1:891),], aes(x = FamilySize, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "FamilySize's impacts",x = "FamilySize", y = "count")

ggplot(data[(1:891),], aes(x = Fare, color = Survived)) +
  geom_line(aes(label = ..count..),stat = "bin",binwidth = 5,na.rm = TRUE) +
  labs(title = "Fare's impacts",x = "Fare", y = "count")

ggplot(data[(1:891),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  labs(title = "Embarked's impacts",x = "Embarked", y = "count")

library(rpart)   
Age.model <- rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamilySize,data = data[!is.na(data$Age),],method = "anova")
data$Age[is.na(data$Age)] <- predict(Age.model,data[is.na(data$Age),])
data$Age2 <- "15-"
data$Age2[data$Age >= 15 & data$Age < 30 ] <- "15-30"
data$Age2[data$Age >= 30 & data$Age < 45 ] <- "30-45"
data$Age2[data$Age >= 45 & data$Age < 60 ] <- "45-60"
data$Age2[data$Age >= 60 ] <- "60+"
newtrain <- data[1:891,]
newtest <- data[892:1309,]
library(e1071)             #naivebayes function
model <- naiveBayes(Survived ~ Pclass + Sex + Age2 + SibSp + Parch + Embarked + Fare + Title + FamilySize, newtrain ,na.action = na.pass)
prediction <- predict(model,newtest)
result <- data.frame(newtest$PassengerId,prediction[1])
names(result) <- c("PassengerId","Survived")
write.csv(result,file = "D:/ZXY.csv",row.names = FALSE)

