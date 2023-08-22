library(neuralnet)
library(caret)
library(pROC)


NEURALNETWORK <- function(type,target,train_data,test_data,results) {
  
  all_columns <- names(train_data)
  features <- setdiff(all_columns, target)
  
  #per la normalizzazione (salvo in columns_with_only_0_1 i nomi delle colonne one-hot-encoded)
  columns_with_only_0_1 <- character(0)
  for (col in features) {
    column_values <- train_data[[col]]
    if (all(column_values %in% c(0, 1))) {
      columns_with_only_0_1 <- c(columns_with_only_0_1, col)
    }
  }
  
  set.seed(123)
  formula <- as.formula(paste(target, "~", paste(features, collapse = "+")))
  
  #nota: sto dando per scontato che i dataset passati alla funzione siano già normalizzati con media e sd 
  switch (type,
    regression = {
      #Normalization:
      feat_norm <- setdiff(all_columns, columns_with_only_0_1)
      scaling_factors <- apply(train_data[, feat_norm], 2, function(x) c(mean(x), sd(x))) 
      scaled_train_data <- as.data.frame(scale(train_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      scaled_test_data <- as.data.frame(scale(test_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      #riaggiungo le variabili one-hot-encoded
      for (col in columns_with_only_0_1) {
        scaled_train_data[[col]] <- train_data[[col]]
        scaled_test_data[[col]] <- test_data[[col]]
      }
      
      #NN:
      nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 50, linear.output = TRUE)
      pr_nn <- predict(nn_model, newdata = scaled_test_data)
      #Tolgo la normalizzazione (questa era la formula per annullare la normalizzazione come l'avevo fatta io, ovviamente in caso va cambiata):
      unscaled_predicted <- pr.nn * scaling_factors[2,target] + scaling_factors[1,target] 
      #Calcolo RMSE:
      rmse <- sqrt(mean((unscaled_predicted- test_data[[target]])^2))
      #aggiungo il risultato al dataframe o quello che sarà in results
      #da decidere come fare
    },
    
    bin_classification = {
      #Normalization:
      feat_norm <- setdiff(features, columns_with_only_0_1)
      scaling_factors <- apply(train_data[, feat_norm], 2, function(x) c(mean(x), sd(x))) 
      scaled_train_data <- as.data.frame(scale(train_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      scaled_test_data <- as.data.frame(scale(test_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      #riaggiungo la variabile target
      scaled_train_data[[target]] <- train_data[[target]]
      scaled_test_data[[target]] <- test_data[[target]]
      #riaggiungo le variabili one-hot-encoded
      for (col in columns_with_only_0_1) {
        scaled_train_data[[col]] <- train_data[[col]]
        scaled_test_data[[col]] <- test_data[[col]]
      }
      
      #NN:
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
      #Normalization:
      feat_norm <- setdiff(features, columns_with_only_0_1)
      scaling_factors <- apply(train_data[, feat_norm], 2, function(x) c(mean(x), sd(x))) 
      scaled_train_data <- as.data.frame(scale(train_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      scaled_test_data <- as.data.frame(scale(test_data[, feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
      #riaggiungo la variabile target
      scaled_train_data[[target]] <- train_data[[target]]
      scaled_test_data[[target]] <- test_data[[target]]
      #riaggiungo le variabili one-hot-encoded
      for (col in columns_with_only_0_1) {
        scaled_train_data[[col]] <- train_data[[col]]
        scaled_test_data[[col]] <- test_data[[col]]
      }
      
      #NN:
      train_data[[target]] <- as.factor(train_data[[target]]) #mi assicuro che sia in livelli, se lo sono già tutte non è necessario
      nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 50, linear.output = FALSE)
      pr_nn_probs <- predict(nn_model, newdata = scaled_test_data)
      colnames(pr_nn_probs) <- unique(test_data[[target]])
      #Calcolo AUNU:
      library(mlr3measures)
      aunu_score <- mauc_aunu(test_data[[target]], pr_nn_probs)
      #aggiungo il risultato al dataframe o quello che sarà in results
      #da decidere come fare
    }
  )
}