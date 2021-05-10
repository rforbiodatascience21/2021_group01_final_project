Data_selection <- function(data, var, rm_na = FALSE, rm_na_from = NULL) {
  # data = data set
  # var = list of variables
  # Rm_na = True if you want to remove na, False if you don't 
  # rm_na_from = list of variables from which na should be removed
  ## If
  if(rm_na == TRUE & is.null(rm_na_from)){
    rm_na_from <- var}
  
  ifelse(rm_na==TRUE,
          Data<-data %>%
            select(var) 
          %>%
            drop_na(rm_na_from),
          Data<-data %>%
            select(var)
          )
  return(Data)
}

Confusion_matrix <-function(Models, Data_test){
    Data_test <- Data_test %>%
      mutate(Predict = predict(Models,Data_test, type = "response"))
  
    Matrix_confusion <- Data_test %>%
      mutate(Predict_No = case_when(Predict > 0.5 ~ 1, Predict <= 0.5 ~0)) %>%
      select(Diagnosis_of_disease_No, Predict_No) %>%
      mutate(Confusion = case_when(Diagnosis_of_disease_No == 1 & Predict_No == 1 ~ "True Positive",
                               Diagnosis_of_disease_No == 0 & Predict_No == 1 ~ "False Positive",
                               Diagnosis_of_disease_No == 1 & Predict_No == 0 ~ "False Negative",
                               Diagnosis_of_disease_No == 0 & Predict_No == 0 ~ "True Negative")) %>%
      group_by(Confusion) %>%
      summarize(Nr = n()) %>%
      ungroup()%>%
      column_to_rownames("Confusion")%>%
      t() %>%
      as_tibble()


    Matrix_confusion <- Matrix_confusion %>%
      mutate(accuracy = (`True Positive`+`True Negative`)/(`True Positive`+`True Negative`+`False Positive`+`False Negative`),
         Precision = (`True Positive`)/(`True Positive`+`False Positive`),
         Recall = (`True Positive`)/(`True Positive`+`False Negative`),
         F_score = 2*Precision*Recall/(Precision+Recall))
  return(Matrix_confusion)
}