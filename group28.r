library(e1071)
library("lattice")
library("MASS")
library("nnet")
# install.packages("lsr")
library(lsr)
library("caret")
library("dplyr")
# install.packages("corrplot")
library(corrplot)


train <- read.csv(file="Documents/cours_ucl/bac3/data/DataAnalitics/Train.csv", header = TRUE)
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

# Assign the NA (missing) values
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

# Standard deviation of the numerical values
sd(train$Age)
sd(train$Work_Experience)
sd(train$Family_Size)
sd(train$Car)
sd(train$Child)

# Compute the skewness of the numerical values

skewness(train$Age)
skewness(train$Work_Experience)
skewness(train$Family_Size)
skewness(train$Car)
skewness(train$Child)

# Compute the kurtosis of the numerical values

kurtosis(train$Age)
kurtosis(train$Work_Experience)
kurtosis(train$Family_Size)
kurtosis(train$Car)
kurtosis(train$Child)

# Histogram and bar-plot of the numerical variables (Histogram for continuous variable and bar-plot for discrete)

hist(train$Age, prob = FALSE, xlab="Age")
hist(train$Work_Experience, prob = FALSE, xlab="Work Experience")
barplot(table(train$Family_Size), xlab="Family Size")
hist(train$Car, prob = FALSE, xlab="Car")
barplot(table(train$Child), xlab="Child")

# Bar-plots for the categorical variables
barplot(table(train$Gender), main = "Gender")
barplot(table(train$Ever_Married), main = "Ever Married")
barplot(table(train$Graduated), main = "Graduated")
barplot(table(train$Profession), main = "Profession", las=2, cex.names=.75)
barplot(table(train$Spending_Score), main = "Spending Score")
barplot(table(train$Credit_Owner), main = "Credit Owner")
barplot(table(train$Var_1), main = "Var_1")
barplot(table(train$Segmentation), main = "Segmentation")

# Correlation matrix 
M<-cor(train[c(3,6,8,9, 11)])
corrplot(M, method="number")

# Scatter Plot
ggplot(data.frame(Age = train$Age, Work_Experience=train$Work_Experience), aes(x=Age, y=Work_Experience)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)
ggplot(data.frame(Age = train$Age, Family_Size=train$Family_Size), aes(x=Age, y=Family_Size)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)
ggplot(data.frame(Work_Experience = train$Work_Experience, Family_Size=train$Family_Size), aes(x=Work_Experience, y=Family_Size)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)


#Cramer' V
cramersV(train$Gender, train$Ever_Married)
cramersV(train$Gender, train$Graduated)
cramersV(train$Gender, train$Profession)
cramersV(train$Gender, train$Spending_Score)
cramersV(train$Gender, train$Credit_Owner)
cramersV(train$Gender, train$Var_1)
cramersV(train$Gender, train$Segmentation)

cramersV(train$Ever_Married, train$Gender)
cramersV(train$Ever_Married, train$Graduated)
cramersV(train$Ever_Married, train$Profession)
cramersV(train$Ever_Married, train$Spending_Score)
cramersV(train$Ever_Married, train$Credit_Owner)
cramersV(train$Ever_Married, train$Var_1)
cramersV(train$Ever_Married, train$Segmentation)

cramersV(train$Graduated, train$Gender)
cramersV(train$Graduated, train$Ever_Married)
cramersV(train$Graduated, train$Profession)
cramersV(train$Graduated, train$Spending_Score)
cramersV(train$Graduated, train$Credit_Owner)
cramersV(train$Graduated, train$Var_1)
cramersV(train$Graduated, train$Segmentation)

cramersV(train$Profession, train$Gender)
cramersV(train$Profession, train$Ever_Married)
cramersV(train$Profession, train$Graduated)
cramersV(train$Profession, train$Spending_Score)
cramersV(train$Profession, train$Credit_Owner)
cramersV(train$Profession, train$Var_1)
cramersV(train$Profession, train$Segmentation)

cramersV(train$Spending_Score, train$Gender)
cramersV(train$Spending_Score, train$Ever_Married)
cramersV(train$Spending_Score, train$Graduated)
cramersV(train$Spending_Score, train$Profession)
cramersV(train$Spending_Score, train$Credit_Owner)
cramersV(train$Spending_Score, train$Var_1)
cramersV(train$Spending_Score, train$Segmentation)

cramersV(train$Credit_Owner, train$Gender)
cramersV(train$Credit_Owner, train$Ever_Married)
cramersV(train$Credit_Owner, train$Graduated)
cramersV(train$Credit_Owner, train$Profession)
cramersV(train$Credit_Owner, train$Spending_Score)
cramersV(train$Credit_Owner, train$Var_1)
cramersV(train$Credit_Owner, train$Segmentation)

cramersV(train$Var_1, train$Segmentation)
