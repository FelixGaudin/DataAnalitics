# You may need to install those packages
# install.packages("lsr")
# install.packages("corrplot")

# Importing libs
library(e1071)
library("lattice")
library("MASS")
library("nnet")
library(lsr)
library("caret")
library("dplyr")
library(corrplot)

# Import the data-set (pay attention to the path)
train_dataset <- read.csv(file="Documents/cours_ucl/bac3/data/DataAnalitics/Train.csv", header = TRUE)
str(train_dataset)

#we remove the useless variables
train_dataset$Licence_Plate <- NULL # It's different for everyone

#we transform '1' in 'Yes' and '0' in 'No' in the 'Credit_Owner' column
train_dataset$Credit_Owner <- factor(train_dataset$Credit_Owner, labels = c("No", "Yes"))
#we check the number of missing values we have in our data set
sum(is.na(train_dataset))
summary(train_dataset)

#we transform all the 'character' values to 'factor' values so that we can count them
#using summary()

train_dataset$Gender <- factor(train_dataset$Gender)
train_dataset$Ever_Married <- factor(train_dataset$Ever_Married)
train_dataset$Graduated <- factor(train_dataset$Graduated)
train_dataset$Profession <- factor(train_dataset$Profession)
train_dataset$Spending_Score <- factor(train_dataset$Spending_Score)
train_dataset$Var_1 <- factor(train_dataset$Var_1)
train_dataset$Segmentation <- factor(train_dataset$Segmentation)

# Assign the NA (missing) values
train_dataset$Gender <- replace(train_dataset$Gender, is.na(train_dataset$Gender), "Male")
train_dataset$Ever_Married <- replace(train_dataset$Ever_Married, is.na(train_dataset$Ever_Married), "Yes")
train_dataset$Graduated <- replace(train_dataset$Graduated, is.na(train_dataset$Graduated), "Yes")
train_dataset$Profession <- replace(train_dataset$Profession, is.na(train_dataset$Profession), "Artist")
train_dataset$Spending_Score <- replace(train_dataset$Spending_Score, is.na(train_dataset$Spending_Score), "Low")
train_dataset$Credit_Owner <- replace(train_dataset$Credit_Owner, is.na(train_dataset$Credit_Owner), "No")
train_dataset$Var_1 <- replace(train_dataset$Var_1, is.na(train_dataset$Var_1), "Cat_6")
train_dataset$Segmentation <- replace(train_dataset$Segmentation, is.na(train_dataset$Segmentation), "D")

train_dataset$Age <- replace(train_dataset$Age, is.na(train_dataset$Age), 43L)
train_dataset$Family_Size <- replace(train_dataset$Family_Size, is.na(train_dataset$Family_Size), 3L)
train_dataset$Work_Experience <- replace(train_dataset$Work_Experience, is.na(train_dataset$Work_Experience), 3L)
train_dataset$Child <- replace(train_dataset$Child, is.na(train_dataset$Child), 1L)
train_dataset$Car <- replace(train_dataset$Car, is.na(train_dataset$Car), 5L)


View(train_dataset)
sum(is.na(train_dataset))
summary(train_dataset)

# Standard deviation of the numerical values
sd(train_dataset$Age)
sd(train_dataset$Work_Experience)
sd(train_dataset$Family_Size)
sd(train_dataset$Car)
sd(train_dataset$Child)

# Compute the skewness of the numerical values
skewness(train_dataset$Age)
skewness(train_dataset$Work_Experience)
skewness(train_dataset$Family_Size)
skewness(train_dataset$Car)
skewness(train_dataset$Child)

# Compute the kurtosis of the numerical values
kurtosis(train_dataset$Age)
kurtosis(train_dataset$Work_Experience)
kurtosis(train_dataset$Family_Size)
kurtosis(train_dataset$Car)
kurtosis(train_dataset$Child)

# Histogram and bar-plot of the numerical variables (Histogram for continuous variable and bar-plot for discrete)

hist(train_dataset$Age, prob = FALSE, xlab="Age", main="Age")
hist(train_dataset$Work_Experience, prob = FALSE, xlab="Work Experience", main="Work Experience")
barplot(table(train_dataset$Family_Size), xlab="Family Size", main="Family Size")
hist(train_dataset$Car, prob = FALSE, xlab="Car", main="Car")
barplot(table(train_dataset$Child), xlab="Child", main="Child")

# Bar-plots for the categorical variables
barplot(table(train_dataset$Gender), main = "Gender")
barplot(table(train_dataset$Ever_Married), main = "Ever Married")
barplot(table(train_dataset$Graduated), main = "Graduated")
barplot(table(train_dataset$Profession), main = "Profession", las=2, cex.names=.75)
barplot(table(train_dataset$Spending_Score), main = "Spending Score")
barplot(table(train_dataset$Credit_Owner), main = "Credit Owner")
barplot(table(train_dataset$Var_1), main = "Var_1")
barplot(table(train_dataset$Segmentation), main = "Segmentation")

# Correlation matrix (for numerical values)
MM<- cor(train_dataset[ , purrr::map_lgl(train_dataset, is.numeric)]) # syntactic sugar to compute only on numerical columns
corrplot(MM, method="number")


# Cramer' V (for non-numerical values)
get_cramersV_correlation_matrix <- function(df) {
  # inspire by https://stackoverflow.com/questions/44070853/association-matrix-in-r
  # Initialize empty matrix to store coefficients
  resp <- matrix(ncol = length(df),
                 nrow = length(df),
                 dimnames = list(names(df), 
                                 names(df)))
  # Computation
  for (r in seq(nrow(resp))){
    for (c in seq(ncol(resp))){
      resp[[r, c]] <- cramersV(df[[r]], df[[c]])
    }
  }
  return(resp)
}

corrplot(get_cramersV_correlation_matrix(train_dataset %>% select(1, 2, 4, 5, 7, 10, 12, 13)), method="number")


# Scatter Plot
ggplot(data.frame(Age = train_dataset$Age, Work_Experience=train_dataset$Work_Experience), aes(x=Age, y=Work_Experience)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)
ggplot(data.frame(Age = train_dataset$Age, Family_Size=train_dataset$Family_Size), aes(x=Age, y=Family_Size)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)
ggplot(data.frame(Work_Experience = train_dataset$Work_Experience, Family_Size=train_dataset$Family_Size), aes(x=Work_Experience, y=Family_Size)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)

# Drop columns
final_dataset <- train_dataset
final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Car"))] # Drop Car and keep Age (correlation of 0.97 so we don't need both columns)
final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Child"))] # Drop Child and keep Family_Size (correlation of 0.79) 
final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Ever_Married"))] # Drop Ever_Married and keep Spending_Score (correlation of 0.67) 

head(final_dataset)


# Set accuracy test : K-CV with K=10
controler <- trainControl(method = "cv", number=10)

# Naive Bayes predictor
naive_bayes_predictor <- train(Segmentation~ ., data=final_dataset, trControl=controler, method="naive_bayes")
naive_bayes_predictor
summary(naive_bayes_predictor$finalModel)

#Data partition
train_ind <- createDataPartition(train_dataset$Segmentation, p=0.6, list = FALSE)
training <- train_dataset[train_ind, ]
testing <- train_dataset[-train_ind, ]

#Logistic regression

#We check the variables which have an impact on determining the segmentation(significant variables)
summary(glm(formula = Segmentation ~ ., family = binomial(link = 'logit'), data = training))#to see which variables are significant

#we use every variable to predict the segmentation
model1 <- multinom(Segmentation ~ . , data=training)
predict(model1, testing, type="prob")
cm1 <- table(predict(model1, testing), testing$Segmentation)
cm1
confusionMatrix(cm1)

#only numerical variables
model2 <- multinom(Segmentation ~ Age + Work_Experience + Family_Size + Car + Child, data=training)
predict(model2, testing, type="prob")
cm2 <- table(predict(model2, testing), testing$Segmentation)
cm2
confusionMatrix(cm2)

#only categorical variables
model3 <- multinom(Segmentation ~ Gender + Ever_Married + Graduated + Profession + Spending_Score + Credit_Owner + Var_1, data=training)
predict(model3, testing, type="prob")
cm3 <- table(predict(model3, testing), testing$Segmentation)
cm3
confusionMatrix(cm3)

#only significant variables (*** and **)
model4 <- multinom(Segmentation ~ Profession + Work_Experience + Spending_Score, data=training)
predict(model4, testing, type="prob")
cm4 <- table(predict(model4, testing), testing$Segmentation)
cm4
confusionMatrix(cm4)

# Decision tree predictor
desition_tree_predictor <- train(Segmentation ~ ., data=final_dataset, method="rpart", trControl=controler)
desition_tree_predictor
summary(desition_tree_predictor$finalModel)
