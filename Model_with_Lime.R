##https://shirinsplayground.netlify.com/2017/12/lime_sketchnotes/


library(farff)
library(missForest)
library(tidyverse)
library(dummies)
library(caret)
library(lime)

data_file <- file.path("Chronic_Kidney_Disease/chronic_kidney_disease_full.arff")

data <- readARFF(data_file)

##Imputing missing data
sapply(data, function(x) sum(is.na(x))/nrow(data)*100)

data_imp <- missForest(data)


##One-hot encoding

data_imp_final <- data_imp$ximp
data_dummy <- dummy.data.frame(dplyr::select(data_imp_final, -class), sep="_")
data_new <- cbind(dplyr::select(data_imp_final, class), scale(data_dummy, 
                                                          center = apply(data_dummy, 2, min),
                                                          scale = apply(data_dummy, 2, max)))


##train - test split
library(caTools)
set.seed(123)
sample = sample.split(data_new$class, SplitRatio = 0.9)
train = data_new[sample==TRUE,]
test = data_new[sample==FALSE,]

##Modelling

model_rf <- caret::train(class~.,
                         data=train, 
                         method= "rf",
                         trControl = trainControl(method ="repeatedcv",
                                                  number = 10,
                                                  repeats = 5,
                                                  verboseIter = FALSE))


model_rf


##predictions

pred <-  data.frame(sample_id = 1:nrow(test), predict(model_rf, test, type="prob"), 
                    actual = test$class) %>%  
          mutate(prediction = colnames(.)[2:3][apply(.[, 2:3], 1, which.max)] , correct = ifelse(actual == prediction, "correct", "wrong"))

pred$prediction <- as.factor(pred$prediction)
confusionMatrix(pred$actual, pred$prediction)


##LIME

train_x <- dplyr::select(train, -class)
test_x <- dplyr::select(test, -class)

train_y <- dplyr::select(train, class)
test_y <- dplyr::select(test,class)


##build explainer
explainer <- lime(train_x, model_rf, n_bins =5, quantile_bines = TRUE)

##run explain() function
explanation_df <- lime::explain(test_x, explainer, n_labels = 1, n_features = 8, n_permutations = 1000,
                            feature_select = "forward_selection")


##model reliability

explanation_df %>% ggplot(aes(x=model_r2, fill = label)) + geom_density(alpha = 0.5)


#plot explanations
plot_features(explanation_df[1:24,], ncol=1)
