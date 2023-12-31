NEURALNETWORK <- function(type,target,train_data,test_data,results,name_encoding) {
  
  all_columns <- names(train_data)
  feat <- setdiff(all_columns, target)
  
  setDT(train_data)
  setDT(test_data)
  
  #mi assicuro che tutte le colonne siano numeriche (faccio i controlli su tutte tranne la target, sto assumendo che nei casi di regressione sia giusta e numerica)
  non_numeric_columns <- sapply(train_data[, ..feat], function(x) !is.numeric(x))
  non_numeric_columns <- names(non_numeric_columns)[non_numeric_columns]
  for (col in non_numeric_columns) {
    train_data[[col]] <- as.numeric(train_data[[col]])
  }
  
  non_numeric_columns_test <- sapply(test_data[, ..feat], function(x) !is.numeric(x))
  non_numeric_columns_test <- names(non_numeric_columns_test)[non_numeric_columns_test]
  for (col in non_numeric_columns_test) {
    test_data[[col]] <- as.numeric(test_data[[col]])
  }
  
  
  #per la normalizzazione (salvo in columns_with_only_0_1 i nomi delle colonne one-hot-encoded)
  columns_with_only_0_1 <- character(0)
  for (col in feat) {
    column_values <- train_data[[col]]
    if (all(column_values %in% c(0, 1))) {
      columns_with_only_0_1 <- c(columns_with_only_0_1, col)
    }
  }
  
  set.seed(123)
  formula <- as.formula(paste(target, "~", paste(feat, collapse = "+")))
  
  #nota: sto dando per scontato che i dataset passati alla funzione siano già normalizzati con media e sd 
  switch (type,
          "regression" = {
            #Normalization:
            feat_norm <- setdiff(all_columns, columns_with_only_0_1)
            if (length(feat_norm)!=0) {
              scaling_factors <- apply(train_data[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
              scaled_train_data <- as.data.frame(scale(train_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              scaled_test_data <- as.data.frame(scale(test_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              #riaggiungo le variabili one-hot-encoded
              for (col in columns_with_only_0_1) {
                scaled_train_data[[col]] <- train_data[[col]]
                scaled_test_data[[col]] <- test_data[[col]]
              }
            } else {
              scaled_train_data <- encoded_train
              scaled_test_data <- encoded_test
            }
            
            
            #NN:
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=1, linear.output = TRUE)
            for (i in 1:1) {
              pr_nn <- predict(nn_model, rep=i, newdata = scaled_test_data) 
              #Tolgo la normalizzazione (questa era la formula per annullare la normalizzazione come l'avevo fatta io, ovviamente in caso va cambiata):
              unscaled_predicted <- pr_nn * scaling_factors[2,target] + scaling_factors[1,target] 
              #Calcolo RMSE:
              rmse <- sqrt(mean((unscaled_predicted- test_data[[target]])^2))
              results[i,name_encoding] <- rmse
            }
            
          },
          
          "bin_classification" = {
            #Normalization:
            feat_norm <- setdiff(feat, columns_with_only_0_1)
            if (length(feat_norm)!=0) {
              scaling_factors <- apply(encoded_train[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
              scaled_train_data <- as.data.frame(scale(encoded_train[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              scaled_test_data <- as.data.frame(scale(encoded_test[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              #riaggiungo la variabile target
              scaled_train_data[[target]] <- encoded_train[[target]]
              scaled_test_data[[target]] <- encoded_test[[target]]
              #riaggiungo le variabili one-hot-encoded
              for (col in columns_with_only_0_1) {
                scaled_train_data[[col]] <- encoded_train[[col]]
                scaled_test_data[[col]] <- encoded_test[[col]]
              }
            } else {
              scaled_train_data <- encoded_train
              scaled_test_data <- encoded_test
            }
            
            
            #NN:
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=1, linear.output = FALSE)
            for (i in 1:1) {
              pr_nn_probs <- predict(nn_model, rep=i, newdata = scaled_test_data) #da le probabilità di ognuni classe
              #rendo uniformi le classi predette e quella del test, per poter calcolare l'AUC
              column_names <- unique(test_data[[target]])
              max_column_indices <- max.col(pr_nn_probs)
              pred_class_names <- column_names[max_column_indices]
              pred_class_names <- as.numeric(pred_class_names)
              #Calcolo AUC:
              roc_obj <- roc(test_data[[target]], pred_class_names)
              auc_score <- roc_obj$auc              
              results[i,name_encoding] <- auc_score #salva nella colonna corrispondente i risultati della rete
            }
            
          },
          
          "multiclass" = {
            #Normalization:
            feat_norm <- setdiff(feat, columns_with_only_0_1)
            if (length(feat_norm)!=0) {
              scaling_factors <- apply(encoded_train[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
              scaled_train_data <- as.data.frame(scale(encoded_train[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              scaled_test_data <- as.data.frame(scale(encoded_test[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
              #riaggiungo la variabile target
              scaled_train_data[[target]] <- encoded_train[[target]]
              scaled_test_data[[target]] <- encoded_test[[target]]
              #riaggiungo le variabili one-hot-encoded
              for (col in columns_with_only_0_1) {
                scaled_train_data[[col]] <- encoded_train[[col]]
                scaled_test_data[[col]] <- encoded_test[[col]]
              }
            } else {
              scaled_train_data <- encoded_train
              scaled_test_data <- encoded_test
            }
            
            #NN:
            train_data[[target]] <- as.factor(train_data[[target]]) #mi assicuro che sia in livelli, se lo sono già tutte non è necessario
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=1, linear.output = FALSE)
            
            for (i in 1:1) {
              pr_nn_probs <- predict(nn_model, rep=i, newdata = scaled_test_data) #da le probabilità di ognuni classe
              colnames(pr_nn_probs) <- unique(test_data[[target]])
              #Calcolo AUNU:
              library(mlr3measures)
              aunu_score <- mauc_aunu(test_data[[target]], pr_nn_probs)
              results[i,name_encoding] <- aunu_score
            }
            
          }
  )
  
  return(results) #restituisce il dataset dei risultati in uscita
}
