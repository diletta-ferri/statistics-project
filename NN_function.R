library(neuralnet)
library(caret)
library(pROC)

NEURALNETWORK <- function(type,target,train_data,test_data,results) {
  
  all_columns <- names(train_data)
  features <- setdiff(all_columns, target)
  
  set.seed(123)
  formula <- as.formula(paste(target, "~", paste(features, collapse = "+")))
  
  #nota: sto dando per scontato che i dataset passati alla funzione siano già normalizzati con media e sd 
  switch (type,
    regression = {
      nn_model <- neuralnet(formula, data = train_data, hidden = 50, linear.output = TRUE)
      pr_nn <- predict(nn_model, newdata = test_data)
      #Tolgo la normalizzazione (questa era la formula per annullare la normalizzazione come l'avevo fatta io, ovviamente in caso va cambiata):
      unscaled_predicted <- pr.nn * scaling_factors[2,target] + scaling_factors[1,target] 
      #Calcolo RMSE:
      rmse <- sqrt(mean((unscaled_predicted- test_data[[target]])^2))
      #aggiungo il risultato al dataframe o quello che sarà in results
      #da decidere come fare
    },
    
    bin_classification = {
      nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 50, linear.output = FALSE)
      pr_nn_probs <- predict(nn_model, newdata = scaled_test_data) #da le probabilità di ognuni classe
      #rendo uniformi le classi predette e quella del test, per poter calcolare l'AUC
      column_names <- unique(test_data[[target]])
      max_column_indices <- max.col(pr_nn_probs)
      pred_class_names <- column_names[max_column_indices]
      pred_class_names <- as.numeric(pred_class_names)
      #Calcolo AUC:
      roc_obj <- roc(test_data[[target]], pred_class_names)
      auc_score <- auc(roc_obj)
      #aggiungo il risultato al dataframe o quello che sarà in results
      #da decidere come fare
    },
    
    multiclass = {
      train_data[[target]] <- as.factor(train_data[[target]]) #mi assicuro che sia in livelli, se lo sono già tutte non è necessario
      nn_model <- neuralnet(formula, data = train_data, hidden = 50, linear.output = FALSE)
      pr_nn_probs <- predict(nn_model, newdata = test_data)
      colnames(pr_nn_probs) <- unique(test_data[[target]])
      #Calcolo AUNU:
      library(mlr3measures)
      aunu_score <- mauc_aunu(test_data[[target]], pr_nn_probs)
      #aggiungo il risultato al dataframe o quello che sarà in results
      #da decidere come fare
    }
  )
}