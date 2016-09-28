#import kaggle train and test data

train <- read.csv("~/RStudio Projects/Kaggle/Titanic/data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/RStudio Projects/Kaggle/Titanic/data/test.csv", stringsAsFactors = FALSE)

#head(train)

#with(train, table(Survived == 1))

#with(train, table(is.na(Age)))

#summary(train$Age)

#summary(train$Sex)

#prop.table(table(train$Sex, train$Survived), 1)

#summary(train)

#sample(1:10, replace = TRUE)

#train$Name[1]

#str(train)

#str(test)

#### all males died ####

# prop.table(table(train$Sex, train$Survived), 1)

test$Survived <- 0

test$Survived[test$Sex == 'female'] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

#str(submit)

#table(submit$Survived)

write.csv(submit, file = "~/RStudio Projects/Kaggle/Titanic/data/submissions/AllMenDie.csv", row.names = FALSE)


#cleaning up age

summary(train$Age)

train$AgeChild <- 0

train$AgeChild[train$Age < 18] <- 1

str(train)

table(train$AgeChild)

train$AgeAdult <- 0
train$AgeAdult[train$Age >= 18 | train$Age < 65] <- 1

train$AgeElderly <- 0
train$AgeElderly[train$Age >= 65] <- 1

table(train$AgeElderly) #11

table(train$AgeAdult) #714

table(train$AgeChild) #113

table(train$Age)

table(train$Fare)

str(train$Fare)

summary(train$Fare)

train$Fare_lt10 <- 0
train$Fare_lt10[train$Fare < 10] <- 1

train$Fare_10to20 <- 0
train$Fare_10to20[train$Fare >= 10 & train$Fare < 20] <- 1

train$Fare_20to30 <- 0
train$Fare_20to30[train$Fare >= 20 & train$Fare < 30] <- 1

train$Fare_gte30 <- 0
train$Fare_gte30[train$Fare >= 30] <- 1

table(train$Fare_lt10)

aggregate(Survived ~ train$Fare_lt10 + train$Fare_10to20 + train$Fare_20to30 + train$Fare_gte30 + train$Pclass + train$Sex, data = train, FUN = function(x) {sum(x) / length(x)})

test$Survived <- 0

test$Survived[test$Sex == 'female'] <- 1

test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 30] <- 0

write.csv(submit, file = "~/RStudio Projects/Kaggle/Titanic/data/submissions/submission3.csv", row.names = FALSE)


# import rpart

library(rpart)

?rpart

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class")

plot(fit)
text(fit)


install.packages('rattle')

install.packages('rpart.plot')

install.packages('RColorBrewer')

library(rattle)

library(rpart.plot)

library(RColorBrewer)

fancyRpartPlot(fit)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class",
             control = rpart.control(minsplit = 2, cp = 0))

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "~/RStudio Projects/Kaggle/Titanic/data/submissions/myfirstdtree.csv", row.names = FALSE)

set.seed(415)

install.packages('party')

library(party)

train <- read.csv("~/RStudio Projects/Kaggle/Titanic/data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/RStudio Projects/Kaggle/Titanic/data/test.csv", stringsAsFactors = FALSE)

train$Name[1]

test$Survived <- NA

combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)

trim <- function(x) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

install.packages("stringr", dependencies = TRUE)

library(stringr)

combi$Title <- 0

#function(x)
#{
#    trim(strsplit(x, split='[,.]')[[1]][2])
#}

combi$Title <- sapply(combi$Name, FUN = trim)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

table(combi$Title)

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN = trim)

combi$FamilyID <- 0

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]

test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data = train,
             method = "class")

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "./Titanic/data/submissions/AddingFeatures.csv", row.names = FALSE)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age),],
                method = "anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

table(combi$Embarked)

which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"

combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)

combi$FamilyID2 <- combi$FamilyID

combi$FamilyID2 <- as.character(combi$FamilyID2)

combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'

combi$FamilyID2 <- factor(combi$FamilyID2)

combi$Survived <-  factor(combi$Survived)

combi$Title <-  factor(combi$Title)

combi$Pclass <-  factor(combi$Pclass)

combi$Sex <-  factor(combi$Sex)

train <- combi[1:891,]

test <- combi[892:1309,]

install.packages('randomForest')

library(randomForest)

set.seed(415)

fit <- randomForest(Survived ~ train$Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE,
                    ntree = 2000)

varImpPlot(fit)

Prediction <- predict(fit, test)

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "./Titanic/data/submissions/randomforest.csv", row.names = FALSE)

install.packages('party')

library(party)

set.seed(415)

fit <- cforest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "./Titanic/data/submissions/cforest.csv", row.names = FALSE)
