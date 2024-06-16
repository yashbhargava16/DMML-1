# Importing essential libraries
  library(caTools)
  library(caret)
  library(dplyr)
  library(rpart)
  library(ROSE)
  library(smotefamily)
  library(corrplot)
#Import the bank_marketing dataset
  bank_marketing = read.csv("bank-full.csv")

# Exploratory Data Analysis
  str(bank_marketing)
  dim(bank_marketing)
  ## Checking for missing values
    N_missing <- is.na(bank_marketing)
    colSums(N_missing)
      # There no missing values in the data
  ## Checking for outliers in the data
    boxplot(bank_marketing[,sapply(bank_marketing, is.numeric)])
      # We can found that there are outliers in the dataset

# Data Cleaning
  # 1. Treatment of Outliers
    rep_out <- function(data, vars) {
    data %>%
    mutate(across(all_of(vars), ~ifelse(. < quantile(., 0.01) | . > quantile(., 0.99), quantile(., c(0.01, 0.99)), .)))
    }
    bank_marketing <- rep_out(bank_marketing, c("age","balance","duration",
                              "pdays","previous"))
    boxplot(bank_marketing[,sapply(bank_marketing, is.numeric)])
      # Outiers Treated
  # 2. Encoding the y variable as factor
    bank_marketing$y = factor(bank_marketing$y,
                       levels = c("no","yes"),
                       labels = c(0,1))
  # 3. Encoding categorical variables
    bank_marketing$job = factor(bank_marketing$job, 
                     levels = c("management","technician","entrepreneur","blue-collar",
                                "unknown","retired","admin.","services","self-employed",
                                "unemployed","housemaid","student"),
                     labels = c(1,2,3,4,
                                5,6,7,8,9,
                                10,11,12))
    bank_marketing$marital = factor(bank_marketing$marital,
                   levels = c("married","single","divorced"),
                   labels = c(1,2,3))
    bank_marketing$education = factor(bank_marketing$education,
                         levels = c("unknown","primary","secondary","tertiary"),
                         labels = c(0,1,2,3))
    bank_marketing$default = factor(bank_marketing$default,
                         levels = c("no","yes"),
                         labels = c(0,1))
    bank_marketing$housing = factor(bank_marketing$housing,
                         levels = c("no","yes"),
                         labels = c(0,1))
    bank_marketing$loan = factor(bank_marketing$loan,
                         levels = c("no","yes"),
                         labels = c(0,1))
    bank_marketing$contact = factor(bank_marketing$contact,
                      levels = c("unknown","cellular","telephone"),
                      labels = c(0,1,2))
    bank_marketing$month = factor(bank_marketing$month,
                       levels = c("may","jun","jul","aug","oct","nov","dec","jan","feb","mar","apr","sep"),
                       labels = c(5,6,7,8,10,11,12,1,2,3,4,9))
    bank_marketing$poutcome = factor(bank_marketing$poutcome,
                          levels = c("unknown", "failure", "success", "other"),
                          labels = c(0,1,2,3))
  # Correlations
    numeric_variables<- bank_marketing[sapply(bank_marketing, is.numeric)]
    correlation <- cor(numeric_variables)
    corrplot(correlation,method = 'color')

# Splitting the dataset into the Training set and Test set
  set.seed(22220861)
  split = sample.split(bank_marketing$y, SplitRatio = 0.7)
  training_set = subset(bank_marketing, split == TRUE)
  test_set = subset(bank_marketing, split == FALSE)


# Feature Scaling
  training_set[,sapply(training_set, is.numeric)] = scale(training_set[,sapply(training_set, is.numeric)])
  test_set[,sapply(test_set, is.numeric)] = scale(test_set[,sapply(test_set, is.numeric)])

#Logistic Regression
    classifier_lr = glm(formula = y ~ ., family = binomial, data = training_set)

  # Predicting the Test set results for Model 1
    prob_predicted_lr = predict(classifier_lr, type = 'response', newdata = test_set[,-17])
    y_predicted_lr = ifelse(prob_predicted_lr > 0.5, 1, 0)

  # Evaluation Metrics for Logistic Regression Mpdel
    test_set["y_predicted_lr"] <- y_predicted_lr
    cm_lr = confusionMatrix(as.factor(test_set$y_predicted_lr),test_set$y)
    print(cm_lr)
    print(cm_lr$overall["Accuracy"])
    print(cm_lr$byClass["Precision"])
    print(cm_lr$byClass["Sensitivity"])
  
# Checking the balance of our bank marketing dataset
  table(bank_marketing$y)
  barplot(table(bank_marketing$y))
# Balancing the data by Random Oversampling
  balanced_dataset = ovun.sample(y~.,
                       data = training_set, 
                       method = 'over')
  balanced_dataset = balanced_dataset$data
  barplot(table(balanced_dataset$y))
  
# Logistic Regression on Balanced dataset
    classifier_lr_balanced = glm(formula = y ~ ., family = binomial, data = balanced_dataset)
    
    # Predicting the Test set results
    prob_predicted_lr_balanced = predict(classifier_lr_balanced, type = 'response', newdata = test_set[,-17])
    y_predicted_lr_balanced = ifelse(prob_predicted_lr_balanced > 0.5, 1, 0)
    
    # Evaluation Metrics of this Model
    test_set["y_predicted_lr2"] <- y_predicted_lr_balanced
    cm_lr_balanced = confusionMatrix(as.factor(test_set$y_predicted_lr2),test_set$y)
    print(cm_lr_balanced)
    print(cm_lr_balanced$overall["Accuracy"])
    print(cm_lr_balanced$byClass["Precision"])
    print(cm_lr_balanced$byClass["Sensitivity"])

# Decision Tree Classifier on balanced dataset
    classifier_dt = rpart(formula = y ~ ., data = balanced_dataset)

  # Predicting the Test set results
    y_predicted_dt = predict(classifier_dt, newdata = test_set[-17], type = 'class')
    test_set["y_predicted_dt"] <- y_predicted_dt

  # Evaluation Metrics for Model 2
    cm_dt = confusionMatrix(test_set$y_predicted_dt,test_set$y)
    print(cm_dt)
    print(cm_dt$overall["Accuracy"])
    print(cm_dt$byClass["Precision"])
    print(cm_dt$byClass["Sensitivity"])