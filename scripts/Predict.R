library(caret)

predict_data = function(input_dataset, pp) {
  
  feature_list <- readRDS("feature_list.rds")
  
  colnames(dataset) <- feature_list
  
  dataset <- input_dataset[feature_list]

  
  #Preprocess
  categorical <- dataset %>% select(-cont_var)
  continuous <- dataset %>% select(cont_var)
  
  new_dataset <- predict(pp, continuous) %>% cbind(categorical)
  
  
  #Load model and predict
  readRDS("model.rds") %>% predict(new_dataset)
}