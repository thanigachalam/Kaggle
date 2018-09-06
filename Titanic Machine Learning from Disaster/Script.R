#-----------------------------------------------------
#importing data from orginal dataset
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
#-----------------------------------------------------
# copying to temp variable
cp_train <- train
cp_test <- test

cp_train$IsTrainSet = TRUE
cp_test$IsTrainSet = FALSE
#cp_train_survived_ds <- train[ , (names(train) %in% c('PassengerId','Survived'))]
cp_test$Survived <- NA
#-----------------------------------------------------
#combining train and test data using rbind
#-----------------------------------------------------
complete_data <- rbind(cp_train, cp_test)
#table(complete_data$Embarked)
#-----------------------------------------------------
#NA data check : validating the passenger fare NA record.
#-----------------------------------------------------
#
library("dplyr")
list_na <- colnames(complete_data)[ apply(complete_data, 2, anyNA) ]
list_na
#-----------------------------------------------------

complete_data$Title <- gsub('(.*, )|(\\..*)', '', complete_data$Name)

library(dplyr)
library(plyr)

complete_data %>% 
  plyr::mutate(Title =
                 dplyr::case_when(Title == 'Mlle'  ~ "Miss",
                                  Title == 'Ms'  ~ "Miss",
                                  Title == 'Mme'  ~ "Mrs",
                                  Title == 'Lady'  ~ "Mrs",
                                  Title == 'Jonkheer'  ~ "Mrs",
                                  Title == 'the Countess'  ~ "Mrs",
                                  Title == 'Don'  ~ "Mr",
                                  Title == 'Dona'  ~ "Mrs",
                                  Title == 'Mlle'  ~ "Miss",
                                  Title == 'Capt' | Title == 'Col' | Title == 'Sir' | Title == 'Major'  ~ "Honorary Title",
                                  TRUE                     ~ Title
                 )) -> complete_data


table(complete_data$Sex, complete_data$Title)

sum(is.na(complete_titanic_ds$PassengerId))

#------------------------------------------------------------------

#Fare Range

complete_data %>% 
  plyr::mutate(Fare_Range =
                 dplyr::case_when(complete_data$Fare >= 0 && complete_data$Fare <= 10  ~ "0-10",
                                  complete_data$Fare >= 11 && complete_data$Fare <= 20  ~ "11-20",
                                  complete_data$Fare >= 21 && complete_data$Fare <= 30  ~ "21-30",
                                  complete_data$Fare >= 31 && complete_data$Fare <= 40  ~ "31-40",
                                  complete_data$Fare >= 41 && complete_data$Fare <= 50  ~ "41-50",
                                  complete_data$Fare >= 51 && complete_data$Fare <= 60  ~ "51-60",
                                  complete_data$Fare >= 61 && complete_data$Fare <= 70  ~ "61-70",
                                  complete_data$Fare >= 71 && complete_data$Fare <= 80  ~ "71-80",
                                  complete_data$Fare >= 81 && complete_data$Fare <= 90  ~ "81-90",
                                  complete_data$Fare >= 91 && complete_data$Fare <= 100  ~ "91-100",
                                  complete_data$Fare > 100  ~ ">100",
                                  TRUE                     ~ Title
                 )) -> complete_data


#-----------------------------------------------------

complete_data$Surname <- sapply(complete_data$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

length(unique(complete_data$Surname))

#-----------------------------------------------------

complete_data$family_size <- complete_data$SibSp + complete_data$Parch
summary(factor(complete_data$family_size))

#complete_data_cp <- merge(complete_data, cp_train_survived_ds, by.x = "PassengerId", by.y = "PassengerId")

#-----------------------------------------------------
write.csv(complete_data,"complete_data.csv")

#------------------------------------------------------------



boxplot(complete_titanic_ds$Fare)
#There are outliers on the upper end in Monthly Income
quantile(complete_titanic_ds$Fare, seq(0,1,0.01))
#modifying all Monthly incomes above 85% to 56.929200
complete_titanic_ds$Fare[which(complete_titanic_ds$Fare>56.929200)]<- 56.929200



#-------------------------------------------------------------------
library(dplyr)
# Exclude the missing records
no_na_records_ds <-complete_data %>%
  na.omit()		
dim(complete_data)

#creating mean values for missing data
average_missing <- apply(complete_data[,colnames(complete_data) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing
# Create a new variable with the mean and median
complete_data_replace <- complete_data %>%
  mutate(replace_mean_age  = ifelse(is.na(Age), average_missing[1], Age),
         replace_mean_fare = ifelse(is.na(Fare), average_missing[2], Fare))


median_missing <- apply(complete_data[,colnames(complete_data) %in% list_na],
                        2,
                        median,
                        na.rm =  TRUE)

complete_data_replace <- complete_data_replace %>%
  mutate(replace_median_age  = ifelse(is.na(Age), median_missing[1], Age), 
         replace_median_fare = ifelse(is.na(Fare), median_missing[2], Fare))

head(complete_data_replace)
write.csv(complete_data_replace,"complete_data_replace.csv")

# We are trying to handle the NA values in the dataset as follows
# 1. for ingnoring the NA values we might be missing 20% of data from the complete dataset. This might impact our analysis.
# 2. considering the mean for the Age and Fare column, where in for the Fare there are many outliers which will impact our anlysis.
# 3. compare to the mean, median is better where in Age is close to the mean and Fare value is reasonable compare to the age , Pclass and boading place

#---
# commenting the below code, 
#filter(complete_data, is.na(complete_data$Fare))

#updating NA value for Fare column
#Pclass = 3 and Embarked = S
#filter_data <- filter(complete_data, !is.na(complete_data$Fare))
#filter_data <- filter(filter_data, filter_data$Pclass == 3)
#filter_data <- filter(filter_data, filter_data$Embarked == 'S')
#filter_data <- filter(filter_data, filter_data$Age >= 60)
#filter_data <- filter(filter_data, filter_data$Age <= 61)
#mean(filter_data$Fare)

#filter(complete_data, is.na(complete_data$Fare)) <- mean(filter_data$Fare)

#complete_data$Fare <- replace(
#  complete_data$Fare,
#  is.na(complete_data$Fare), mean(filter_data$Fare))

#-----------------------------------------------------------------
complete_titanic_ds <- complete_data_replace
#updating median values to actual columns
complete_titanic_ds$Age <- complete_titanic_ds$replace_median_age
complete_titanic_ds$Fare <- complete_titanic_ds$replace_median_fare

#-----------------------------------------------------------------
#----------------------------------------------------------------------------------
titanic.train <- complete_titanic_ds[complete_titanic_ds$IsTrainSet == TRUE,]
titanic.test <- complete_titanic_ds[complete_titanic_ds$IsTrainSet == FALSE,]


titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Sex <- as.factor(titanic.train$Sex)
titanic.train$Embarked <- as.factor(titanic.train$Embarked)
titanic.train$Survived <- as.factor(titanic.train$Survived)

str(titanic.train)

library(randomForest)


survived.equ <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equ)


#randomForest
#Survived ~  Pclass + Sex + Age + SibSp + Parch + Fare + Embarked
model <- randomForest(formula = survived.formula, data = titanic.train, ntree=500, mtry=3, nodsize =0.01 * nrow(titanic.test))


#------------------------------------------------------------------
titanic.test$Pclass <- as.factor(titanic.test$Pclass)
titanic.test$Sex <- as.factor(titanic.test$Sex)
titanic.test$Embarked <- as.factor(titanic.test$Embarked)
titanic.test$Survived <- as.factor(titanic.test$Survived)

#predict
levels(titanic.test$Embarked) <- levels(titanic.train$Embarked)

survived.features.equ <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(model, newdata = titanic.test)

Survived

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

output.df

write.csv(output.df, file="kaggle_submission.csv", row.names = FALSE)
#------------------------------------------------------------------
