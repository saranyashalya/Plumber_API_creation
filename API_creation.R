##https://shirinsplayground.netlify.com/2018/01/plumber/

library(tidyverse)
library(rjson)

# take first test case for prediction
input_data <- test[1, ] %>%
  select(-class)

# predict test case using model
pred <- predict(model_rf, input_data)
cat("----------------\nTest case predicted to be", as.character(pred), "\n----------------")


##input required

var_names <- model_rf$finalModel$xNames
var_names

# show parameter definition for the first three features
for (i in 1:3) {
  # if you wanted to see it for all features, use
  #for (i in 1:length(var_names)) {
  var <- var_names[i]
  train_data_subs <- train[, which(colnames(train) == var)]
  type <- class(train_data_subs)
  
  if (type == "numeric") {
    min <- min(train_data_subs)
    max <- max(train_data_subs)
  }
  
  cat("Variable:", var, "is of type:", type, "\n",
      "Min value in training data =", min, "\n",
      "Max value in training data =", max, "\n----------\n")
  
}


test_cast_json <- toJSON(input_data)
cat(test_cast_json)
