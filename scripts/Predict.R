library(caret)

predict_data = function(input_dataset, pp) {
  
  feature_list <- readRDS("feature_list.rds")
  
  colnames(dataset) <- feature_list
  
  dataset <- input_dataset[feature_list]
  
  #Dummify
  cont_var <- c('ApplicantIncome', 'CoapplicantIncome', 'LoanAmount')
  cat_var <- dataset %>% select(-cont_var) %>% colnames()
  
  categorical <- dataset[cat_var]
  continuous <- dataset %>% select(cont_var)
  
  dmy <- dummyVars( ~ ., data = categorical)
  
  new_categorical <- data.frame(predict(dmy, newdata = categorical)) %>% 
    select(-Gender., -Married., -Dependents., -Self_Employed.)
  
  
  dummified_dataset <- cbind(continuous, new_categorical)
  
  #################3
  #print(dummified_dataset)
  
  
  #Preprocess
  categorical <- dummified_dataset %>% select(-cont_var)
  continuous <- dummified_dataset %>% select(cont_var)
  
  new_dataset <-predict(pp, continuous) %>% cbind(categorical)
  
  #################
  #print(new_dataset)
  
  #Load model and predict
  readRDS("model.rds") %>% predict(dummified_dataset)
}