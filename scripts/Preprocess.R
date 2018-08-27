library(caret)

preprocess_data = function(dataset, prediction = TRUE) {
  cont_var <- c('ApplicantIncome', 'CoapplicantIncome', 'LoanAmount')
  if (!prediction) {
    cat_var <- dataset %>% select(-cont_var, -Loan_Status) %>% colnames()
  } else {
    cat_var <- dataset %>% select(-cont_var) %>% colnames()
  }
  dummified_dataset <- dataset[cat_var] %>% dummify() 
  
  categorical <- dummified_dataset %>% select(-cont_var)
  continuous <- dummified_dataset %>% select(cont_var)

  result <- preProcess(x = continuous) %>% 
    predict(continuous) %>% cbind(categorical)
}

dummify = function(dataset, prediction = TRUE) {
  cont_var <- c('ApplicantIncome', 'CoapplicantIncome', 'LoanAmount')
  if (!prediction) {
    cat_var <- dataset %>% select(-cont_var, -Loan_Status) %>% colnames()
  } else {
    cat_var <- dataset %>% select(-cont_var) %>% colnames()
  }
  
  categorical <- dataset[cat_var]
  continuous <- dataset %>% select(cont_var)
  
  dmy <- dummyVars( ~ ., data = categorical)
  
  new_categorical <- data.frame(predict(dmy, newdata = categorical)) %>% 
    select(-Gender., -Married., -Dependents., -Self_Employed.)

  if (!prediction) {
    new_dataset <- cbind(continuous, new_categorical, dataset['Loan_Status'])
  } else {
    new_dataset <- cbind(continuous, new_categorical)
  }
} 