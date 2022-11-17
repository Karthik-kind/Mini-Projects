#Import Libraries
library(rpart)
library(rpart.plot)

#Import Dataset

data= read.csv("C:/Users/Tony_Stark/Downloads/archive/heart.csv")
View(data)

head(data,3)

#Data Preprocessing

data$Age = ifelse(is.na(data$Age),
                     ave(data$Age, FUN = function (x)mean(x, na.rm = TRUE)),
                     data$Age)

data$ChestPainType = ifelse(is.na(data$ChestPainType),
                  ave(data$ChestPainType, FUN = function (x)mean(x, na.rm = TRUE)),
                  data$ChestPainType)

data$ChestPainType = factor(data$ChestPainType, 
                         levels = c('ATA','NAP','ASY','TA'), 
                         labels = c(1.0, 2.0 , 3.0, 4.0 ))

View(data)


data$ExerciseAngina = factor(data$ExerciseAngina,
                           levels = c('N', 'Y'),
                           labels = c(0, 1))

data$ST_Slope = factor(data$ST_Slope,
                             levels = c("Up", "Flat"),
                             labels = c(1, 0))

summary(data)

str(data)


head(data,5)

# required library for data splition

library(caTools)
set.seed(123)
split = sample.split(data$ChestPainType, SplitRatio = 0.8)# returns true if observation goes to the Training set and false if observation goes to the test set.

#Creating the training set and test set separately
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)
training_set
test_set


#using Decision Tree For Prediction
tree <- rpart(RestingBP ~ Cholesterol+Age, data)
a <- data.frame(Cholesterol=c(250),Age=c(50))
result <- predict(tree,a)
print(result)

rpart.plot(tree, box.palette = "azure3")

tree <- rpart(Cholesterol ~ RestingBP+Age, data)
a <- data.frame(RestingBP=c(120),Age=c(50))
result <- predict(tree,a)
print(result)


rpart.plot(tree, box.palette = "azure3")


#Predicting HeartDisease 


tree <- rpart(HeartDisease ~ Cholesterol+Sex, data)
a <- data.frame(Cholesterol=c(150),Sex=c("M"))
result <- predict(tree,a)
print(result)

rpart.plot(tree, box.palette = "darkgrey")

tree <- rpart(Cholesterol ~ RestingBP+Age, data)
a <- data.frame(RestingBP=c(120),Age=c(50))
result <- predict(tree,a)
print(result)

rpart.plot(tree, box.palette = "darkgrey")



tree <- rpart(HeartDisease ~ Oldpeak + Sex, data)
a <- data.frame(Oldpeak=c(Oldpeak=>1), Sex=c("F"))
result <- predict(tree,a)
print(result)

rpart.plot(tree, box.palette = "darkgrey")


tree <- rpart(HeartDisease ~ Oldpeak + ChestPainType, data)
a <- data.frame(Oldpeak=c(Oldpeak=>1), ChestPainType=c(1))
result <- predict(tree,a)
print(result)


rpart.plot(tree, box.palette = "darkgrey")
#
data= read.csv("C:/Users/Tony_Stark/Downloads/archive/heart.csv")
data$Age = ifelse(is.na(data$Age),
                  ave(data$Age, FUN = function (x)mean(x, na.rm = TRUE)),
                  data$Age)

data$ChestPainType = ifelse(is.na(data$ChestPainType),
                            ave(data$ChestPainType, FUN = function (x)mean(x, na.rm = TRUE)),
                            data$ChestPainType)

data$ChestPainType = factor(data$ChestPainType, 
                            levels = c('ATA','NAP','ASY','TA'), 
                            labels = c(1.0, 2.0 , 3.0, 4.0 ))
data$ExerciseAngina = factor(data$ExerciseAngina,
                             levels = c('N', 'Y'),
                             labels = c(0, 1))

data$ST_Slope = factor(data$ST_Slope,
                       levels = c("Up", "Flat"),
                       labels = c(1, 0))
dataset = data[10:12]
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 2)
y_hc
# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of HeartDisease'),
         xlab = 'Oldpeak',
         ylab = 'HeartDisease')
