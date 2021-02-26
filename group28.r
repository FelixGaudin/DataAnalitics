library(e1071)
library("lattice")
library("MASS")
library("nnet")
install.packages("lsr")
library(lsr)
library("caret")
library("dplyr")


train <- read.csv(file="Train.csv", header = TRUE)
str(train)

#we remove the useless variables
train$Licence_Plate <- NULL

#we transform '1' in 'Yes' and '0' in 'No' in the 'Credit_Owner' column
train$Credit_Owner <- factor(train$Credit_Owner, labels = c("No", "Yes"))

#we check the number of missing values we have in our data set
sum(is.na(train))
summary(train)

#we transform all the 'character' values to 'factor' values so that we can count them
#using summary()

train$Gender <- factor(train$Gender)
train$Ever_Married <- factor(train$Ever_Married)
train$Graduated <- factor(train$Graduated)
train$Profession <- factor(train$Profession)
train$Spending_Score <- factor(train$Spending_Score)
train$Var_1 <- factor(train$Var_1)
train$Segmentation <- factor(train$Segmentation)

#assign the NA (missing) values
train$Gender <- replace(train$Gender, is.na(train$Gender), "Male")
train$Ever_Married <- replace(train$Ever_Married, is.na(train$Ever_Married), "Yes")
train$Graduated <- replace(train$Graduated, is.na(train$Graduated), "Yes")
train$Profession <- replace(train$Profession, is.na(train$Profession), "Artist")
train$Spending_Score <- replace(train$Spending_Score, is.na(train$Spending_Score), "Low")
train$Credit_Owner <- replace(train$Credit_Owner, is.na(train$Credit_Owner), "No")
train$Var_1 <- replace(train$Var_1, is.na(train$Var_1), "Cat_6")
train$Segmentation <- replace(train$Segmentation, is.na(train$Segmentation), "D")

train$Age <- replace(train$Age, is.na(train$Age), 43L)
train$Family_Size <- replace(train$Family_Size, is.na(train$Family_Size), 3L)
train$Work_Experience <- replace(train$Work_Experience, is.na(train$Work_Experience), 3L)
train$Child <- replace(train$Child, is.na(train$Child), 1L)
train$Car <- replace(train$Car, is.na(train$Car), 5L)


View(train)
sum(is.na(train))
summary(train)

#standard deviation of the numerical values
sd(train$Age)
sd(train$Work_Experience)
sd(train$Family_Size)
sd(train$Car)
sd(train$Child)

#compute the skewness of the numerical values

skewness(train$Age)
skewness(train$Work_Experience)
skewness(train$Family_Size)
skewness(train$Car)
skewness(train$Child)

#compute the kurtosis of the numerical values

kurtosis(train$Age)
kurtosis(train$Work_Experience)
kurtosis(train$Family_Size)
kurtosis(train$Car)
kurtosis(train$Child)

#histogram of the numerical variables

hist(train$Age, prob = FALSE)
hist(train$Work_Experience, prob = FALSE)
hist(train$Family_Size, prob = FALSE)
hist(train$Car, prob = FALSE)
hist(train$Child, prob = FALSE)

#barplots for the categorical variables
barplot(table(train$Gender), main = "Gender")
barplot(table(train$Ever_Married), main = "Ever Married")
barplot(table(train$Graduated), main = "Graduated")
barplot(table(train$Profession), main = "Profession", las=2, cex.names=.75)
barplot(table(train$Spending_Score), main = "Spending Score")
barplot(table(train$Credit_Owner), main = "Credit Owner")
barplot(table(train$Var_1), main = "Var_1")
barplot(table(train$Segmentation), main = "Segmentation")

#matrice 
cor(train[c(3,6,8,9, 11)])



