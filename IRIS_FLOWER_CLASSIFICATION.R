# A model that can classify iris flowers into different species based on their sepal
#and petal measurements.

rm(list=ls())
set.seed(1234)

data=iris
print(data)
attach(data)



#DATA PREPROCESSING,

# dimension of the data
dim(data)

# structure of the dataset
str(data)

# statistics of the data
summary(data)

# handling missing values if any
data=na.omit(data);data





# VISUALISATION

# Scatterplot
library(ggplot2)
ggplot(data,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+
  geom_point(size=3)+labs(title="Scatterplot of Iris dataset",
                          x="Sepal Length",y="Sepal Width")+theme_minimal()

# Multiple Scatterplot
library(GGally)
ggpairs(data = data, columns = 1:4, aes(col = Species))





# MODELING

# Spliting the data into training (80%) and testing (20%) sets
install.packages("caTools")
library(caTools) 
split=sample.split(data$Species, SplitRatio = 0.8)
train_data=subset(data, split == T)
test_data=subset(data, split == F)


# Decision Tree Model
install.packages("rpart")
library(rpart)
dt_model=rpart(Species ~ ., data = train_data, method = "class")
plot(dt_model)
text(dt_model, use.n = TRUE)
dt_predictions=predict(dt_model, test_data, type = "class")
dt_confusion_matrix=table(Predicted = dt_predictions, Actual = test_data$Species)
print(dt_confusion_matrix)
dt_accuracy=sum(diag(dt_confusion_matrix)) / sum(dt_confusion_matrix)
print(paste("Decision Tree Accuracy:", round(dt_accuracy * 100, 2), "%"))


# Logistic Regression Model
install.packages("nnet") 
library(nnet)
lr_model=multinom(Species ~ ., data = train_data)
lr_predictions=predict(lr_model, test_data)
lr_confusion_matrix=table(Predicted = lr_predictions, Actual = test_data$Species)
print(lr_confusion_matrix)
lr_accuracy=sum(diag(lr_confusion_matrix)) / sum(lr_confusion_matrix)
print(paste("Logistic Regression Accuracy:", round(lr_accuracy * 100, 2), "%"))


# Random Forest Model
install.packages("randomForest")
library(randomForest)
rf_model=randomForest(Species ~ ., data = train_data, ntree = 100)
rf_predictions=predict(rf_model, test_data)
rf_confusion_matrix=table(Predicted = rf_predictions, Actual = test_data$Species)
print(rf_confusion_matrix)
rf_accuracy=sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)
print(paste("Random Forest Accuracy:", round(rf_accuracy * 100, 2), "%"))


# Accuracy comparision 
method=c("Decision Tree Model","Logistic Regression Model","Random Forest Model")
accuracy=c(dt_accuracy,lr_accuracy,rf_accuracy)
d=data.frame(method,accuracy);d











