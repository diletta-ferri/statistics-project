library(neuralnet)
library(caret)
library(pROC)
library(ranger)
library(ISwR)  # for utftoint in hash
library(dplyr)
library(tibble)
library(fastDummies) # for dummy
library(lme4) # for aunu 


get_encoded_data = function(data,target, encoder, thr){ 
  
  if (encoder == "integer"){
    encoded_data= integer_encoding_tt(train_data, test_data, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
  }
  if (encoder == "frequency"){
    encoded_data= frequency_encoding_tt(train_data, test_data, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
  }
  if (encoder == "impact"){
    encoded_data= impact_encoding_tt(train_data, test_data,target, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
  }
  
  if (encoder == "hash"){
    encoded_data= hash_encoding_tt(train_data, test_data, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
  }
  if (encoder == "remove"){
    encoded_data= remove_encoding_tt(train_data, test_data, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
  }
  if (encoder == "glmm"){
    encoded_data = glmm_encoding_wrapper(train_data, test_data, target, thr)
    encoded_train = encoded_data$train
    encoded_test = encoded_data$test 
  }
  if (encoder == "leaf"){
    encoded_data = leaf_encoding_train(train_data, target, thr)
    encoded_train = encoded_data$data
    foglie_comuni = encoded_data$most_common_leaves
    tabella_codifica = encoded_data$output_table
    encoded_test = leaf_encoding_test(test_data, encoded_train, target, foglie_comuni, tabella_codifica)
  }
  
  return (list(train= encoded_train, test= encoded_test))
  
} 


#------- 

NEURALNETWORK <- function(type,target,train_data,test_data,results,name_encoding) {
  
  results <- cbind(results, name_encoding = c(0,0,0,0,0)) #nel dataframe results aggiungo una colonna che si chiama come il nome dell'encoding e inizializzo i valori a 0
  
  all_columns <- names(train_data)
  features <- setdiff(all_columns, target)
  
  #mi assicuro che tutte le colonne siano numeriche (faccio i controlli su tutte tranne la target, sto assumendo che nei casi di regressione sia giusta e numerica)
  non_numeric_columns <- sapply(train_data[,features], function(x) !is.numeric(x))
  non_numeric_columns <- names(non_numeric_columns)[non_numeric_columns]
  for (col in non_numeric_columns) {
    train_data[[col]] <- as.numeric(train_data[[col]])
  }
  
  non_numeric_columns_test <- sapply(test_data[,features], function(x) !is.numeric(x))
  non_numeric_columns_test <- names(non_numeric_columns_test)[non_numeric_columns_test]
  for (col in non_numeric_columns_test) {
    test_data[[col]] <- as.numeric(test_data[[col]])
  }
  
  
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
          "regression" = {
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
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=3, linear.output = TRUE)
            for (i in 1:5) {
              pr_nn <- predict(nn_model, rep=i, newdata = scaled_test_data) 
              #Tolgo la normalizzazione (questa era la formula per annullare la normalizzazione come l'avevo fatta io, ovviamente in caso va cambiata):
              unscaled_predicted <- pr.nn * scaling_factors[2,target] + scaling_factors[1,target] 
              #Calcolo RMSE:
              rmse <- sqrt(mean((unscaled_predicted- test_data[[target]])^2))
              results[i,name_encoding] <- rmse
            }
            
          },
          
          "bin_classification" = {
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
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=5, linear.output = FALSE)
            for (i in 1:5) {
              pr_nn_probs <- predict(nn_model, rep=i, newdata = scaled_test_data) #da le probabilità di ognuni classe
              #rendo uniformi le classi predette e quella del test, per poter calcolare l'AUC
              column_names <- unique(test_data[[target]])
              max_column_indices <- max.col(pr_nn_probs)
              pred_class_names <- column_names[max_column_indices]
              pred_class_names <- as.numeric(pred_class_names)
              #Calcolo AUC:
              roc_obj <- roc(test_data[[target]], pred_class_names)
              auc_score <- auc(roc_obj)
              results[i,name_encoding] <- auc_score #salva nella colonna corrispondente i risultati della rete
            }
            
          },
          
          "multiclass" = {
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
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=5, linear.output = FALSE)
            
            for (i in 1:5) {
              pr_nn_probs <- predict(nn_model, rep=i, newdata = scaled_test_data) #da le probabilità di ognuni classe
              colnames(pr_nn_probs) <- unique(test_data[[target]])
              #Calcolo AUNU:
              library(mlr3measures)
              aunu_score <- mauc_aunu(test_data[[target]], pr_nn_probs)
              results[i,name_encoding] <- auc_score
            }
            
          }
  )
  
  return(results)
}

#----------

drop_cost= function(data){
  
  col_names = colnames(data)
  
  for (col in col_names){
    if (length(unique(data[[col]])) == 1 ) {
      data[[col]] = NULL  # Drop constants
    }
  }
  
  return(data)
}

#----------

first_prep = function(data, target) {
  col_names = colnames(data)
  
  for (col in col_names) {
    if (is.numeric(data[[col]])) {
      col_mean = mean(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] = col_mean
    } 
    else if (is.logical(data[[col]]) || length(unique(data[[col]])) == 2) {
      col_mode = names(sort(-table(data[[col]], useNA = "always")))[1]
      data[[col]][is.na(data[[col]])] = as.logical(col_mode)
    } 
    else if (is.factor(data[[col]]) || (is.character(data[[col]]) && length(unique(data[[col]])) > 2)) {
      
      
      data[[col]] <- sapply(data[[col]], function(x) gsub("[^[:alnum:]\\_]", "_", x)) # remove non alpha numeric characters
      
      data[[col]]=factor(data[[col]])
      
      if (sum(is.na(data[[col]])>0) && col!= target){
        levels(data[[col]]) = c(levels(data[[col]]), "Missing")
        data[[col]][is.na(data[[col]])] = "Missing"
      }
    }
    
    else if (length(unique(data[[col]])) == 1) {
      data[[col]] = NULL
    }
  }
  
  return(data)
}

#----------

is_binary = function(column) {
  unique_values = unique(column)
  num_unique = length(unique_values)
  return(num_unique == 2 && all(unique_values %in% c(0, 1)))
}

#------------

tt_split= function(data,target,p){
  #set.seed(123)
  split_indices = createDataPartition(data[[target]], p = p, list = FALSE)
  train_data = data[-split_indices, ]
  test_data = data[split_indices, ]
  
  return(list(train=train_data, test= test_data))
}

#-----------

cbind.fill <- function(...){   # per creare output_table in leaf encoding (unire df quando hanno un numero diverso di righe e inserisce na)
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}