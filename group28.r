# You may need to install those packages
# install.packages("lsr")
# install.packages("corrplot")
install.packages(c("FactoMineR", "factoextra"))
# Importing libs
library(e1071)
library("lattice")
library("MASS")
library("nnet")
library(lsr)
library("caret")
library("dplyr")
library(corrplot)
library(rpart)
library(FactoMineR)
library("factoextra")


# Import the data-set (pay attention to the path)
train_dataset <- read.csv(file="Train.csv", header = TRUE)
str(train_dataset)
summary(train_dataset)
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
#final_dataset <- train_dataset
#final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Car"))] # Drop Car and keep Age (correlation of 0.97 so we don't need both columns)
#final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Child"))] # Drop Child and keep Family_Size (correlation of 0.79) 
#final_dataset <- final_dataset[ , -which(names(final_dataset) %in% c("Ever_Married"))] # Drop Ever_Married and keep Spending_Score (correlation of 0.67) 

#head(final_dataset)


# Set accuracy test : K-CV with K=10
controler <- trainControl(method = "cv", number=10)

#PCA (only numerical variables)
train_dataset.pca <- prcomp(train_dataset[,c(3,6,8,9,11)], center = TRUE,scale = TRUE)
summary(train_dataset.pca)

#We create a set with PCA dimensions
pca_set <- data.frame(matrix(nrow = nrow(train_dataset), ncol = 4))
colnames(pca_set) <- c("PC1","PC2","PC3","PC4")
for (i in 1:4)
{
  pca_set[,i]= train_dataset.pca$x[,i]
}
head(pca_set)

#MCA (categorical variables)
train_dataset.mca <- MCA(train_dataset[c(1,2,4,5,7,10,12,13)], graph = FALSE, ncp = 11)
summary(train_dataset.mca)
var <- get_mca_ind(train_dataset.mca)
var$coord


#We create a set with MCA dimensions
mca_set <- data.frame(matrix(nrow = nrow(train_dataset), ncol = 11))
colnames(mca_set) <- c("DIM1","DIM2","DIM3","DIM4", "DIM5", "DIM6","DIM7","DIM8","DIM9", "DIM10", "DIM11")
for (i in 1:11)
{
  mca_set[,i]= var$coord[,i]
}
head(mca_set)

#linear discriminant analysis
lda <- lda(formula=Segmentation ~ ., data=train_dataset)
lda$prior
lda$counts
lda$scaling

lda_set <- data.frame(matrix(nrow=nrow(train_dataset), ncol = 3))
colnames(lda_set) <- c("LDA1", "LDA2", "LDA3");
lda_val = predict(lda)
for(i in 1:3)
{
  lda_set[,i] = lda_val$x[,i]
}

#significant variables
summary(glm(formula = Segmentation ~ ., family = binomial(link = 'logit'), data = train_dataset))

#we combine everything in only one dataset
final_train_dataset <- cbind(lda_set, pca_set, mca_set, train_dataset[c(1:13)])


#Logistic regression
#Tenfold cross-validation
#https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation

#every variable 0.4692205
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated +Profession + Work_Experience + Spending_Score + Family_Size + Car +Credit_Owner + Child + Var_1 , data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#categorical variables 0.4553365
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~ Gender + Ever_Married + Graduated +Profession + Spending_Score  +Credit_Owner + Var_1 , data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#numerical variables  0.3962984
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~ Age + Work_Experience + Family_Size + Car + Child , data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#only significant variables (*** and **) 0.4420129
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~ Profession + Work_Experience + Spending_Score , data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#PC1 2 3 4 + numerical variables 0.3962984
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~ PC1 + PC2 +PC3 + PC4 + Age + Work_Experience + Family_Size + Car + Child , data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#MCA + categorical vars MCA +DIM 1 acc = 1 ??????
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~  Gender + Ever_Married + Graduated +Profession + Spending_Score  +Credit_Owner + Var_1 + DIM1, data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#LDA variables + Significant 0.4717444
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- multinom(Segmentation ~  LDA1+LDA2+LDA3 + Profession + Work_Experience + Spending_Score, data=training_dataset)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

# Naive Bayes predictor

#every variable 0.4610856
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~ Gender + Ever_Married + Age + Graduated +Profession + Work_Experience + Spending_Score + Family_Size + Car +Credit_Owner + Child + Var_1 , data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#categorical variables 0.4570197
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~  Gender + Ever_Married + Graduated +Profession + Spending_Score  +Credit_Owner + Var_1, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)


#only significant variables (*** and **) 0.4445382
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~   Profession + Work_Experience + Spending_Score, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)


#PCA and numerical variables 0.3947562
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~  PC1 + PC2 +PC3 + PC4 + Age + Work_Experience + Family_Size + Car + Child, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#MCA + categorical variables 0.784313
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~  Gender + Ever_Married + Graduated +Profession + Spending_Score  +Credit_Owner + Var_1 + DIM1 +DIM2 +DIM3+DIM4+DIM5+DIM6+DIM7+DIM8+DIM9+DIM10+DIM11, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)

#LDA + significant 0.4671153
vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~   LDA1+LDA2+LDA3 + Profession + Work_Experience + Spending_Score, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)


vals <- c()
folds <- cut(seq(1,nrow(final_train_dataset)),breaks=10,labels=FALSE)
for(i in 1:10){
  indexes <- which(folds==i,arr.ind=TRUE)
  testing_dataset <- final_train_dataset[indexes, ]
  training_dataset <- final_train_dataset[-indexes, ]
  model <- naiveBayes(Segmentation ~   LDA1+LDA2+LDA3+PC1+PC2+PC3, data=training_dataset, laplace = 3)
  pred <- predict(model, testing_dataset)
  tab <- table(pred, testing_dataset$Segmentation)
  cm <- confusionMatrix(tab)
  overall <- cm$overall
  acc <-  overall['Accuracy'] 
  vals <- c(vals, acc)
}
vals
mean(vals)
min(vals)
max(vals)


# Decision tree predictor
desition_tree_predictor <- train(Segmentation ~ ., data=final_dataset, method="rpart", trControl=controler)
desition_tree_predictor
summary(desition_tree_predictor$finalModel)

plot(desition_tree_predictor$finalModel, uniform=FALSE,
     main="Classification Tree")
text(desition_tree_predictor$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

# Prettier for graphs but the model is the same
tree <- rpart(Segmentation~., data=final_dataset, cp=.02)
rpart.plot(tree, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

