# Importing Libraries
library(caTools)
library(caret)
library(dplyr)
library(corrplot)
library(MLmetrics)
library(randomForest)
# Importing the data set
dry_bean = read.csv("Dry_Bean_Dataset.csv")

# Exploratory Data Analysis
  str(dry_bean)
  dim(dry_bean)
  ## Checking for missing values
    N_missing <- is.na(dry_bean)
    colSums(N_missing)
    # There no missing values in the data
  ## Checking for outliers in the data
    boxplot(dry_bean[,-17])
    # We can found that there are outliers in the data

# Data Preparation
  # 1. Treatment of Outliers
    rep_out <- function(data, vars) {
      data %>%
        mutate(across(all_of(vars), ~ifelse(. < quantile(., 0.01) | . > quantile(., 0.99), quantile(., c(0.01, 0.99)), .)))
      }
    dry_bean <- rep_out(dry_bean, c("Area","Perimeter","MajorAxisLength","MinorAxisLength",
                                       "AspectRation","Eccentricity","ConvexArea",
                                       "EquivDiameter","Extent","Solidity","roundness",
                                       "Compactness","ShapeFactor1","ShapeFactor2",
                                       "ShapeFactor3","ShapeFactor4"))
    boxplot(dry_bean[,-17])
    # Outliers Treated

  # 2. Encoding the y variable as factor
    dry_bean$Class = factor(dry_bean$Class,
                       levels = c('BARBUNYA','BOMBAY','CALI',
                                  'DERMASON','HOROZ','SEKER','SIRA'),
                       labels = c(1,2,3,4,5,6,7))
  
  # 3. Checking Correlation of data
    numeric_variables<- dry_bean[sapply(dry_bean, is.numeric)]
    correlation <- cor(numeric_variables)
    corrplot(correlation,method = 'color')

# Data Modelling
    # 1. Split the dataset into training and test set
      set.seed(22220861)
      split = sample.split(dry_bean$Class,
                     SplitRatio = 0.7)
      training_set = subset(dry_bean, split == TRUE)
      test_set = subset(dry_bean, split == FALSE)
    # 2. Feature Scaling
      training_set[,1:16] = scale(training_set[,1:16])
      test_set[,1:16] = scale(test_set[,1:16])
    # 3. Random Forest Classification Model
      classifier = randomForest(x = training_set[-17],
                          y = training_set$Class,
                          ntree = 10)
    # 4. Predicting the Test set results
      y_predict = predict(classifier, newd ata = test_set[-17])
   
    # 5. Calculating Evaluation Metrics
      cm = confusionMatrix(y_predict, test_set$Class)
      Accuracy <- cm$overall["Accuracy"]
      Precision <- cm$byClass[,"Precision"]
      Recall <- cm$byClass[,"Sensitivity"]
      f1_score <- F1_Score(test_set$Class,y_predict)
      print(paste(" Test Accuracy:", round(Accuracy,2)))
      print(cm)
      cat("Precision:\n")
      print(Precision)
      cat("\nRecall:\n")
      print(Recall)
      print(paste("F1 Score:", round(f1_score, 2)))