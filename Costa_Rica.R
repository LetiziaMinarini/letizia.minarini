
## Authors: Group Barnard's star

# Luca Martorano (u306264)
# Riccardo Rampoldi (u129700)
# Letizia Minarini (u736034)
# Shashank Srinivasan (u336695)
# Tolgahan Koçyigit (u826393)


## Load Packages ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(corrplot)
library(reshape2)

## Load Data 

costa_rica <- read.delim("input/train.csv", stringsAsFactors = FALSE)
costa_rica_copy <- costa_rica

## EDA (Exploratory Data Analysis)

str(costa_rica)   # Majority of coulmns are int, some strings.

full_missing = costa_rica[, sapply(costa_rica, anyNA), drop = FALSE]
str(full_missing)  # NAs inspection

full_numerical = costa_rica %>%  #Keeping just the numerical variables
  select_if(is.numeric) %>% 
  select(
    -v2a1,
    -v18q1,
    -rez_esc)

replace_na = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
full_numerical = apply(full_numerical, 2, replace_na)

costa_rica_matrix <- data.frame(full_numerical)

# Correlation matrix

corr_mat = cor(full_numerical)
corrplot(corr_mat, tl.cex = 0.30)

cr_corr <- subset(melt(corr_mat), value > 0.90)
cr_corr_equal <- cr_corr %>%
  rowwise() %>%
  filter(Var1 == Var2)

cr_clean <- anti_join(cr_corr, cr_corr_equal)

costa_rica_final <- costa_rica_matrix %>%
  select(
    -tamhog,
    -hogar_total,
    -SQBage,
    -r4t3,
    -agesq,
    -SQBhogar_total,
    -SQBmeaned,
    -SQBovercrowding,
    -SQBescolari,
    -tamviv)

# Creating the binary classification column

costa_rica_final <- costa_rica_final %>%
  mutate(classification = if_else(Target > 2, "Not Poor"," Poor"))

# Remove any variable that has only a single value for all observations

costa_rica_final$elimbasu5 = NULL
costa_rica_final$planpri = NULL
costa_rica_final$Target = NULL


# Informative graphs

# Looking at the distribution of the classes to predict

costa_rica_final %>%
  ggplot(aes(classification))+
  geom_bar(color = 'black', fill = 'orange')+
  labs(title = "Distribution of target classes")

# Looking at the distribution of people within households

ggplot(data = costa_rica_final, aes(x = hhsize, fill = "orange")) +
  geom_histogram(bins = 15)

# Looking at the year of scholarization in households

escolari_plot <- ggplot(data = costa_rica_final) +
  geom_boxplot(aes(y = escolari, x = factor(classification),
                   fill = factor(classification))) +
  labs(title = "Years of Schooling by Target Household", y = "Number of Years",
       x = "Household Target ") +
  theme_light() +
  theme(legend.position = "none")
escolari_plot

# Looking at the distribution of rooms in houses per classification level

room_plot <- ggplot(data = costa_rica_final) +
  geom_histogram(aes(x = rooms, fill = factor(classification)), stat = "count",
                 position = "dodge", alpha=0.5) +
  theme_light() +
  labs(title = "Count of Rooms per Household", x = "Number of Rooms",
       y = "Number of Househols") +
  scale_fill_discrete(name = "Target Household") +
  theme(legend.position = "bottom")
room_plot



# Data Partitioning (Train and Test sets)

costa_rica_final$classification = as.factor(costa_rica_final$classification)

set.seed(1)

tr_index <- createDataPartition(y = costa_rica_final$classification, p = 0.70,
                                list = FALSE)
cr_train <- costa_rica_final[tr_index, ]
cr_test <- costa_rica_final[-tr_index, ]


##### KNN MODEL #### (with all varibales) --------------------------------------

cr_knn <- train(classification ~ ., method = "knn", data = cr_train, 
                trControl = trainControl(method = "cv", number = 5),
                preProcess = c("center", "scale"))

plot(cr_knn)

cr_knn$results

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_knn, cr_test)
predicted_outcomes[1:10]

cr_test$classification <- factor(cr_test$classification)

# Confusion Matrix creation

knn_confM <- confusionMatrix(predicted_outcomes, cr_test$classification)
knn_confM

# Looking for the 20 most important variables

varImp(cr_knn)

cr_imp_knn <- costa_rica_final %>%
  select(
    meaneduc,          
    SQBdependency,      
    escolari,           
    hogar_nin,          
    SQBhogar_nin,      
    overcrowding,      
    epared3,            
    r4t1,               
    eviv3,              
    cielorazo,          
    paredblolad,        
    pisomoscer,        
    rooms,              
    SQBedjefe,          
    qmobilephone,       
    etecho3,            
    r4m1,               
    hogar_adul,         
    v18q,               
    bedrooms,
    classification
  )


# Data Partitioning (Train and Test sets)

cr_imp_knn$classification = as.factor(cr_imp_knn$classification)

set.seed(1)

tr_index_t20_knn <- createDataPartition(y = cr_imp_knn$classification, p = 0.70,
                                        list = FALSE)
cr_train_t20_knn <- cr_imp_knn[tr_index, ]
cr_test_t20_knn <- cr_imp_knn[-tr_index, ]


#### KNN MODEL #### (top 20 variables) -----------------------------------------

cr_knn_t20 <- train(classification ~ ., method = "knn", data = cr_train_t20_knn, 
                trControl = trainControl(method = "cv", number = 5),
                preProcess = c("center", "scale"))

plot(cr_knn_t20)

cr_knn_t20$results

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_knn_t20, cr_test_t20_knn)
predicted_outcomes[1:10]

cr_test_t20_knn$classification <- factor(cr_test_t20_knn$classification)

# Confusion Matrix creation

knn_confM_t20 <- confusionMatrix(predicted_outcomes, cr_test_t20_knn$classification)
knn_confM_t20

#### LOGISTIC REGRESSION MODEL #### (with all variables) -----------------------


cr_glm <- train(classification ~ ., method = "glm",
                 family = binomial(link = "logit"), data = cr_train,
                 trControl = trainControl(method = "cv", number = 5))

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_glm, cr_test)
predicted_outcomes[1:10]

cr_test$classification <- factor(cr_test$classification)

# Confusion Matrix creation

glm_confM <- confusionMatrix(predicted_outcomes, cr_test$classification)
glm_confM

# Looking at the mist important variables

summary(cr_glm)
varImp((cr_glm))

cr_imp_glm <- costa_rica_final %>%
  select(
    meaneduc,
    hogar_nin,
    eviv3,
    paredblolad,
    SQBedjefe,          
    qmobilephone,
    r4m1,               
    hogar_adul,         
    v18q,
    classification
  )

# Data Partitioning (Train and Test sets)

cr_imp_glm$classification = as.factor(cr_imp_glm$classification)

set.seed(1)

tr_index_t20_glm <- createDataPartition(y = cr_imp_glm$classification, p = 0.70,
                                list = FALSE)
cr_train_t20_glm <- cr_imp_glm[tr_index, ]
cr_test_t20_glm <- cr_imp_glm[-tr_index, ]

#### LOGISTIC REGRESSION MODEL #### (with top 20 variables)---------------------

cr_glm_t20 <- train(classification ~ ., method = "glm", data = cr_train_t20_glm, 
                trControl = trainControl(method = "cv", number = 5),
                preProcess = c("center", "scale"))

cr_glm_t20$results

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_glm_t20, cr_test_t20_glm)
predicted_outcomes[1:10]

cr_test_t20_glm$classification <- factor(cr_test_t20_glm$classification)

# Confusion Matrix creation

glm_confM_t20 <- confusionMatrix(predicted_outcomes, cr_test_t20_glm$classification)
glm_confM_t20
    

#### RANDOM FOREST MODEL #### (with all variables)------------------------------

cr_rf <- train(classification ~ ., method = "rf", data = cr_train, 
                trControl = trainControl(method = "cv", number = 5),
                preProcess = c("center", "scale"))

cr_rf$results

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_rf, cr_test)
predicted_outcomes[1:10]

cr_test$classification <- factor(cr_test$classification)

# Confusion Matrix creation

rf_confM <- confusionMatrix(predicted_outcomes, cr_test$classification)
rf_confM

# looking for the most important varibales

varImp(cr_rf)

cr_imp_rf <- costa_rica_final %>%
  select(
    meaneduc,      
    SQBdependency,   
    overcrowding,    
    SQBedjefe,       
    qmobilephone,    
    age,             
    rooms,           
    eviv3,           
    r4t1,            
    r4m3,            
    SQBhogar_nin,    
    hogar_nin,       
    r4m1,            
    r4h2,            
    r4t2,            
    cielorazo,       
    v18q,            
    paredblolad,    
    r4h3,            
    hogar_adul,
    classification
  )

# Data Partitioning (Train and Test sets)  

cr_imp_rf$classification = as.factor(cr_imp_rf$classification)

set.seed(1)

tr_index_rf_t20 <- createDataPartition(y = cr_imp_rf$classification, p = 0.70,
                                       list = FALSE)
cr_train_rf_t20  <- cr_imp_rf[tr_index, ]
cr_test_rf_t20  <- cr_imp_rf[-tr_index, ]

#### RANDOM FOREST MODEL #### (with all 20 most important variables)------------

cr_rf_t20 <- train(classification ~ ., method = "rf", data = cr_train_rf_t20 , 
               trControl = trainControl(method = "cv", number = 5),
               preProcess = c("center", "scale"))

# Model evaluation

set.seed(1)

predicted_outcomes <- predict(cr_rf_t20, cr_test_rf_t20)
predicted_outcomes[1:10]

cr_test_rf_t20$classification <- factor(cr_test_rf_t20$classification)

# Confusion Matrix creation

rf_confM_t20 <- confusionMatrix(predicted_outcomes, cr_test_rf_t20$classification)
rf_confM_t20 
