#Midwest survey 
#Fatti: onehot, integer, frequency, impact 
#gli altri mi da errore la funzione megaf

library("OpenML")
library(farff)
library(data.table)
source("encoding_functions.R") # per chiamare funzioni da un altro script
source("first_prep.R")

library(neuralnet)
library(caret)
library(pROC)
library(lme4)
library(rpart)
#install.packages("treeClust")
library(treeClust)

#prendere il dataset:
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f", arff.reader = "farff")
get_data = getOMLDataSet(data.id = 42345) #inserire il codice del dataset (42345 è Traffic violations)
data = get_data$data
#specifico la variabile target (nome della colonna target)
target=get_data$target.features
target
View(data)
#creo il dataset dei risultati (chiamarlo con il nome del dataset che sto analizzando)
traffic_violations_result <- data.frame(prima_col=c(0)) #ha una sola riga, ed avrà una prima colonna che rimane a 0, serve per creare il dataframe

#Specifico che tipo di problema è
#tipo_problema = "regression"
#tipo_problema = "bin_classification"
tipo_problema = "multiclass" #è un multiclasse perchè ci sono tre tipi di violation possibili

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
    tabella_codifica = encoding_data$output_table
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
              auc_score <- auc(roc_obj)
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


data_prep = first_prep(data,target)
target <- gsub("[^[:alnum:]\\_]", "_", target) #sistemo prima la target da sola, sennò rischio di perderla se la trasformo dopo
colnames(data_prep) <- gsub("[^[:alnum:]\\_]", "_", colnames(data_prep)) #sistemo tutti i nomi delle colonne
set.seed(123)
test_percentage <- 0.3
num_test <- round(nrow(data_prep) * test_percentage)
test_indices <- sample(seq_len(nrow(data_prep)), size = num_test)
test_data <- data_prep[test_indices, ]
train_data <- data_prep[-test_indices, ]

enc_prova = c("impact")
enc_1 = c("dummy","hash","integer")
enc_1.1 = c("glmm","leaf","remove")
enc_1.2 = c("onehot","integer")
enc_2 = c("frequency","impact","glmm","leaf", "remove")
thresholds = c(10,25,125)

#metto qui il ciclo for degli encoder e delle threshold, per avere il come si aggiunge il modello:
#ogni volta che finisce un encoder stampa il dataset dei risulati
for (encoder in enc_prova){# per ogni dataset, ogni encoding
  if (encoder=="none"){# none fa parte delle control conditions, l'ho messo fuori perchè non fa nulla e non ha bisgno di threshold. Di conseguenza andrebbe gestito a parte.
    encoded_train= train_set
    encoded_test= test_set
    # mettere modello
    traffic_violations_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, traffic_violations_result, encoder)
    print(traffic_violations_result)
    next
    
  }
  if (encoder=="onehot"){# anche lui va messo fuori perchè non ha threshold
    # in questo caso lavoro pre-splitting perchè diventava un casino gestire i vari livelli delle categoriche 
    #e si rischiava di avere shape diverse tra train e test
    encoded_data= one_hot_encoding(data_prep,target)
    colnames(encoded_data) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_data)) #sistemo tutti i nomi delle colonne
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    #mettere modello
    traffic_violations_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, traffic_violations_result, encoder)
    print(traffic_violations_result)
    next
    
  }
  if (encoder=="dummy"){# stesso discorso di onehot
    encoded_data= one_hot_encoding(data_prep, target, dummy = T)
    colnames(encoded_data) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_data)) #sistemo tutti i nomi delle colonne
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    #mettere modello
    traffic_violations_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, traffic_violations_result, encoder)
    print(traffic_violations_result)
    next
  }
  for (thr in thresholds){# per ogni encoding 
    encoded_data= megaf1(data_prep, target, encoder, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
    colnames(encoded_train) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_train)) #sistemo tutti i nomi delle colonne
    colnames(encoded_test) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_test)) #sistemo tutti i nomi delle colonne
    # mettere modello
    encandthr <- paste0(encoder, thr) #nome colonna per i risultati
    traffic_violations_result= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, traffic_violations_result, encandthr)
    print(traffic_violations_result)
    
  }
}

#salvare il dataframe dei risultati creato:
saveRDS(midwest_risultati, file = "midwest_survey_risultati.rds")



#------- 
#PROVA FATTA A MANO CON UN ENCODER, UN THR PER VEDERE SE FUNZIONA, POTETE LASCIARLO PERDERE, MA SE VI SERVE ALMENO È QUI

#prova fatta a mano con integer (per vedere se funziona)
t=10
encoded_data= integer_encoding_tt(train_data, test_data, t)
encoded_train= encoded_data$train
encoded_test= encoded_data$test
colnames(encoded_train) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_train)) #sistemo tutti i nomi delle colonne
colnames(encoded_test) <- gsub("[^[:alnum:]\\_]", "_", colnames(encoded_test)) #sistemo tutti i nomi delle colonne




all_columns <- names(encoded_train)
feat <- setdiff(all_columns, target)

setDT(encoded_train)
setDT(encoded_test)

#mi assicuro che tutte le colonne siano numeriche (faccio i controlli su tutte tranne la target, sto assumendo che nei casi di regressione sia giusta e numerica)
non_numeric_columns <- sapply(encoded_train[, ..feat], function(x) !is.numeric(x))
non_numeric_columns <- names(non_numeric_columns)[non_numeric_columns]
for (col in non_numeric_columns) {
  encoded_train[[col]] <- as.numeric(encoded_train[[col]])
}

non_numeric_columns_test <- sapply(encoded_test[, ..feat], function(x) !is.numeric(x))
non_numeric_columns_test <- names(non_numeric_columns_test)[non_numeric_columns_test]
for (col in non_numeric_columns_test) {
  encoded_test[[col]] <- as.numeric(encoded_test[[col]])
}


#per la normalizzazione (salvo in columns_with_only_0_1 i nomi delle colonne one-hot-encoded)
columns_with_only_0_1 <- character(0)
for (col in feat) {
  column_values <- encoded_train[[col]]
  if (all(column_values %in% c(0, 1))) {
    columns_with_only_0_1 <- c(columns_with_only_0_1, col)
  }
}

set.seed(123)


formula <- as.formula(paste(target, "~", paste(feat, collapse = "+")))


name_encoding <- "integer10"

#nota: sto dando per scontato che i dataset passati alla funzione siano già normalizzati con media e sd 
        
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
          scaled_train_data[[target]] <- as.factor(scaled_train_data[[target]]) #mi assicuro che sia in livelli, se lo sono già tutte non è necessario
          nn_model <- neuralnet(formula, data = scaled_train_data, hidden = 10, threshold=0.1, rep=1, linear.output = FALSE)
          
          for (i in 1:1) {
            pr_nn_probs <- predict(nn_model, rep=i, newdata = scaled_test_data) #da le probabilità di ognuni classe
            colnames(pr_nn_probs) <- unique(encoded_test[[target]])
            #Calcolo AUNU:
            library(mlr3measures)
            aunu_score <- mauc_aunu(encoded_test[[target]], pr_nn_probs)
            traffic_violations_result[i,name_encoding] <- aunu_score
          }
          
          traffic_violations_result

