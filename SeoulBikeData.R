# Importing Libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(corrplot)
# Importing Dataset
bike_data = read_xlsx('SeoulBikeData.xlsx')

# Data Preprocessing
str(bike_data)

# Detect missing values in the entire data frame
N_missing <- is.na(bike_data)
colSums(N_missing)

# Detection of Outliers
boxplot(bike_data[,2:11])

# Treatment of Ouliers
library(dplyr)
winsorize <- function(x, trim = 0.01) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}
bike_data <- bike_data %>%
  mutate(across(where(is.numeric), winsorize))  #Outliers treated

boxplot(bike_data[,2:11])

# Renaming columns
bike_data <- setNames(bike_data,c("Date","Rented.Bike.Count","Hour","Temperature", "Humidity","Wind.Speed","Visibility","Dew.point.temperature","Solar.Radiation","Rainfall","Snowfall","Seasons","Holiday","Functioning.Day"))
# Encoding categorical variables
unique(bike_data$Seasons)
unique(bike_data$Holiday)
unique(bike_data$Functioning.Day)
bike_data$Seasons = factor(bike_data$Seasons,
                           levels = c('Winter','Spring','Summer','Autumn'),
                           labels = c(1,2,3,4))
bike_data$Holiday = factor(bike_data$Holiday,
                           levels = c('No Holiday','Holiday'),
                           labels = c(0,1))
bike_data$`Functioning Day` = factor(bike_data$Functioning.Day,
                           levels = c('Yes','No'),
                           labels = c(0,1))

# Checking Correlations
numeric_variables<- bike_data[sapply(bike_data, is.numeric)]
correlation <- cor(numeric_variables)
corrplot(correlation,method = 'color')

# Splitting the data into training and test set
library(caTools)
set.seed(22220861)
split = sample.split(bike_data$Rented.Bike.Count, SplitRatio = 0.7)
training_set = subset(bike_data, split == TRUE)
test_set = subset(bike_data, split == FALSE)
                     

# Feature Scaling
training_set[,3:11] = scale(training_set[,3:11])
test_set[,3:11] = scale(test_set[,3:11])

# Fitting Linear Regression to the training set and considering all the independent features
lr_regressor= lm(formula = Rented.Bike.Count ~ .,
            data = training_set )
summary(lr_regressor)

# Fitting Linear Regression model after removing those features which have p-value > 0.05
lr_regressor = lm(formula = Rented.Bike.Count ~ Date+Hour+Temperature+Humidity+Wind.Speed+Solar.Radiation+Rainfall+Snowfall+Seasons+Holiday+Functioning.Day,
            data = training_set )
summary(lr_regressor)

# Checking Multicollinearity between independent features
vif_values <- car::vif(lr_regressor)
print(lr_regressor)

# Fitting Linear Regression model after removing those features which have VIF >5 i.e Seasons
lr_regressor= lm(formula = Rented.Bike.Count ~ Date+Hour+Temperature+Humidity+Wind.Speed+Solar.Radiation+Rainfall+Snowfall+Holiday+Functioning.Day,
            data = training_set )
summary(lr_regressor)

vif_values <- car::vif(lr_regressor)
print(vif_values)

#Fitting Linear Regression model after removing those features which have p-value>0.05
lr_regressor = lm(formula = Rented.Bike.Count ~ Date+Hour+Temperature+Humidity+Wind.Speed+Solar.Radiation+Rainfall+Holiday+Functioning.Day,
            data = training_set )
summary(lr_regressor)

#Predicting the Test Results
y_pred = predict(lr_regressor,
                 newdata = test_set)

# Evaluation metrics of Model4
mae <- mean(abs(y_pred - test_set$Rented.Bike.Count))
mse <- mean((test_set$Rented.Bike.Count - y_pred)^2)
rmse <- sqrt(mse)
mape <- mean(abs((test_set$Rented.Bike.Count - y_pred) / test_set$Rented.Bike.Count)) * 100
# Print or use the calculated MSE
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Mean Absolute Error (MAE):",mae))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Percentage Error:",mape))

# Support Vector Machine for Regression
library(e1071)
svm_regressor = svm(formula = Rented.Bike.Count ~ Date+Hour+Temperature+Humidity+Wind.Speed+Solar.Radiation+Rainfall+Holiday+Functioning.Day,
                    data = training_set,
                    type = "eps-regression")
summary(svm_regressor)

# Predicting the test set result
y_pred = predict(svm_regressor, data = test_set)

mse <- mean((test_set$Rented.Bike.Count - y_pred)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_set$Rented.Bike.Count - y_pred))
mape <- mean(abs((test_set$Rented.Bike.Count - y_pred) / test_set$Rented.Bike.Count)) * 100
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Mean Absolute Error (MAE):",mae))
print(paste("Root Mean Squared Error (RMSE):", rmse))