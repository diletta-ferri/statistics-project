library("OpenML")
library(farff)
library(data.table)
source("encoding_functions.R") # per chiamare funzioni da un altro script

library(neuralnet)
library(caret)
library(pROC)
library(lme4)
library(rpart)
install.packages("treeClust")
library(treeClust)

#__________prove varie__________________________________
# set configuration per openML -  API key to read only
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f", arff.reader = "farff")
oml_dat = getOMLDataSet(data.id = 41283) #è il churn dataset, classificazione binaria
desc = oml_dat$desc
data = as.data.table(oml_dat$data)
View(data)
oml_dat[["desc"]]
target = desc$default.target.attribute
#________________________________________

#creare il dataset iniziale:
#churn_result <- data.frame(prima_col=c(0,0,0,0,0)) #creo il dataset in cui salverò i risultati della rete
#finire di aggiungere:
churn_result <- readRDS("churn_results_quasicompleto.rds")
tipo_problema = "bin_classification"

megaf= function(data,target, encoder, thr){ # le funzioni che chiamo in megaf ce le ho in uno script a parte
  
  # handle missing values and remove constant columns
  # encoding. Per ora le funzioni sono uguali sia per train che test, TODO in corso
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
    if(class(encoded_data) == "character"){
      next # smetto di iterare sulle thr
    }
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
  
  # removing costants again
  
  #encoded_train = drop_cost(encoded_train)
  #encoded_test = drop_cost(encoded_test)
  
  # final one hot encoding 
  
  #ohe_train = dummyVars(" ~ .", data = encoded_train)
  #ohe_test = dummyVars(" ~ .", data = encoded_test)    
  #final_train = data.frame(predict(ohe_train, newdata = encoded_train))
  #final_test = data.frame(predict(ohe_train, newdata = encoded_test))
  
  return (list(train= encoded_train, test= encoded_test))
  
}   

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
            scaling_factors <- apply(train_data[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
            scaled_train_data <- as.data.frame(scale(train_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
            scaled_test_data <- as.data.frame(scale(test_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
            #riaggiungo le variabili one-hot-encoded
            for (col in columns_with_only_0_1) {
              scaled_train_data[[col]] <- train_data[[col]]
              scaled_test_data[[col]] <- test_data[[col]]
            }
            
            #NN:
            nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.01, rep=5, linear.output = TRUE)
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
            feat_norm <- setdiff(feat, columns_with_only_0_1)
            scaling_factors <- apply(train_data[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
            scaled_train_data <- as.data.frame(scale(train_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
            scaled_test_data <- as.data.frame(scale(test_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
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
            feat_norm <- setdiff(feat, columns_with_only_0_1)
            scaling_factors <- apply(train_data[, ..feat_norm], 2, function(x) c(mean(x), sd(x))) 
            scaled_train_data <- as.data.frame(scale(train_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
            scaled_test_data <- as.data.frame(scale(test_data[, ..feat_norm], center = scaling_factors[1,], scale = scaling_factors[2,]))
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

#PIPELINE:

encodings= c("integer", "impact", "frequency", "hash", "onehot", "dummy", "remove", "none")
enc_1 = c("none","integer","hash","remove")
enc_2 = c("frequency","impact","glmm","leaf")
enc_2.1 = c("leaf")
thresholds = c(10,25,125)

data_prep = first_prep(data)

# splitting
# tt=tt_split(data_prep, target, 0.3)
# train_data= tt$train
# test_data= tt$test

# codice alternativo per splittare: 
set.seed(123)
test_percentage <- 0.3
num_test <- round(nrow(data_prep) * test_percentage)
test_indices <- sample(seq_len(nrow(data_prep)), size = num_test)
test_data <- data_prep[test_indices, ]
train_data <- data_prep[-test_indices, ]


for (encoder in enc_2.1){# per ogni dataset, ogni encoding
  if (encoder=="none"){# none fa parte delle control conditions, l'ho messo fuori perchè non fa nulla e non ha bisgno di threshold. Di conseguenza andrebbe gestito a parte.
    encoded_train= train_data
    encoded_test= test_data
    # mettere modello
    churn_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, churn_result, encoder)
    print(churn_result)
    next
    
  }
  if (encoder=="onehot"){# anche lui va messo fuori perchè non ha threshold
    # in questo caso lavoro pre-splitting perchè diventava un casino gestire i vari livelli delle categoriche 
    #e si rischiava di avere shape diverse tra train e test
    encoded_data= one_hot_encoding(data_prep,target)
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    #mettere modello
    churn_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, churn_result, encoder)
    print(churn_result)
    next
    
  }
  if (encoder=="dummy"){# stesso discorso di onehot
    encoded_data= dummy_encoding(data_prep, target)
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    #mettere modello
    churn_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, churn_result, encoder)
    print(churn_result)
    next
  }
  for (thr in thresholds){# per ogni encoding 
    encoded_data= megaf(data_prep, target, encoder, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
    # mettere modello
    encandthr <- paste0(encoder, thr) #nome colonna per i risultati
    churn_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, churn_result, encandthr)
    print(churn_result)
    
  }
}

saveRDS(churn_result, file = "churn_results_senza_onehot_e_dummy.rds")
#quando lo vorrò ottenere di nuovo:
#loaded_data <- readRDS("churn_results_quasicompleto.rds")