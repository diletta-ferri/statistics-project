library(caret)
library(ranger)
library(ISwR)
library(dyplr)

#_________________MISC.___________________________________


first_prep = function(data) {
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
      levels(data[[col]]) <- c(levels(data[[col]]), "Missing")
      data[[col]][is.na(data[[col]])] = "Missing"
    } 
    else if (length(unique(data[[col]])) == 1) {
      data[[col]] = NULL
    }
  }
  
  return(data)
}


#-------

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
  
#----------------------ENCODING-FUNCTIONS-------------------------------------------


integer_encoding= function(data, threshold) {
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col)) # check whether a column is categorical , return a T F vector
  
  for (col_name in names(data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
    col_levels = length(unique(data[[col_name]]))
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    
    if (col_levels > threshold) {
      data[[col_name]] = as.integer(factor(data[[col_name]]))-1 # factor conversion is used in case of chr
    } 
    else {
      encoded_col = model.matrix(~data[[col_name]]-1 , data = data)
      data[[col_name]]=NULL
      colnames(encoded_col) = gsub("data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      data = cbind(data, encoded_col) # add new encoded columns
    }  
  }
  
  return(data)
}




integer_encoding_tt = function(train_data, test_data, threshold) {
  # Apply encoding to training data
  train_encoded = integer_encoding(train_data, threshold)
  
  # Store encoding mapping for each column
  encoding_mapping = list()
  categorical_cols = sapply(train_encoded, function(col) is.factor(col) || is.character(col))
  for (col_name in names(train_encoded)[categorical_cols]) {
    encoding_mapping[[col_name]] = unique(train_encoded[[col_name]])
  }
  
  # Apply encoding to test data
  test_encoded = integer_encoding(test_data, threshold)
  
  # Map unseen levels in test data to mode of training data encoding
  for (col_name in names(test_encoded)[categorical_cols]) {
   
      test_levels = unique(test_encoded[[col_name]])
      train_levels = encoding_mapping[[col_name]]
      
      # Identify unseen levels in test data
      unseen_levels = setdiff(test_levels, train_levels)  # check values in x not in y
      
      if (length(unseen_levels) > 0) {
        # Map unseen levels to the mode of training data encoding
        mode_value = as.integer(names(sort(table(train_encoded[[col_name]]),decreasing=T)[1]))
        test_encoded[[col_name]][test_encoded[[col_name]] %in% unseen_levels] = mode_value # change unseen values in test with mode of train
      }
    
  }
  
  # Return the encoded test data
  return(list(train=train_encoded, test=test_encoded))
}



}

#--------------



frequency_encoding= function(data, threshold) {
    categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col))# check whether a colummn is categorical , return a T F vector
    
    for (col_name in names(data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
      data[[col_name]]= factor(data[[col_name]])
      col_levels = nlevels(data[[col_name]])
      
      if (col_levels > threshold) {
        frequencies=table(data[[col_name]])
        data[[col_name]]=frequencies[data[[col_name]]]
      } 
      else {
        encoded_col = model.matrix(~data[[col_name]]-1 , data = data)
        data[[col_name]]=NULL
        colnames(encoded_col) = gsub("data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
        data = cbind(data, encoded_col) # add new encoded columns
        
        
      }
    }
    
    return(data)
}



frequency_encoding_tt= function(train_data, test_data, threshold) {
  
  # train_enc= frequency_encoding(train_data, threshold)
  # test_enc= frequency_encoding(test_data, threshold)

  categorical_cols = sapply(train_data, function(col) is.factor(col) || is.character(col))# check whether a colummn is categorical , return a T F vector

  for (col_name in names(train_data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector
    train_data[[col_name]]= factor(train_data[[col_name]])
    col_levels = nlevels(train_data[[col_name]])
    if (col_name == target){ # avoids encoding the target when it's categorical
      next
    }

    if (col_levels > threshold) {
      frequencies=table(train_data[[col_name]])
      train_data[[col_name]] = frequencies[train_data[[col_name]]]
      test_data[[col_name]] = frequencies[test_data[[col_name]]]
      test_data[[col_name]][is.na(test_data[[col_name]])]=1 # in case of new levels. i.e where the freqs in train is 0
    }
    else {
      encoded_col = model.matrix(~train_data[[col_name]]-1 , data = train_data)
      train_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("train_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col))
      train_data = cbind(train_data, encoded_col) # add new encoded columns
      #same for test
      encoded_col = model.matrix(~test_data[[col_name]]-1 , data = test_data)
      test_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("test_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col))
      test_data = cbind(test_data, encoded_col) # add new encoded columns

    }
  }

  return(list(train=train_data, test= test_data))
}




#------------------
  
  

impact_encoding = function(data,target,threshold, smoothing_factor = 0.0001 ){
                       
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col))# check whether a colummn is categorical , return a T F vector
  
  for (col_name in names(data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
    col_levels = length(unique(data[[col_name]]))
    
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    
    if (col_levels > threshold) {
       
        if (is_binary(data[[target]]) || is.numeric(data[[target]])) { # case when target is binary or numeric
          
          unique_levels = unique(data[[col_name]])
          conditional_means = rep(0,length(unique_levels)) 
          apriori_y= sum(data[[target]])/length(data[[target]]) #  global y_mean 
          freqs=table(data[[col_name]]) # frequencies for each level 
          
          for (i in seq_along(unique_levels)) {
            level_count = as.numeric(freqs[i])
            lambda = 1 / (1+exp(-level_count/smoothing_factor)) #weighting factor
            level_name = unique_levels[i]
            subset_target = data[[target]][data[[col_name]] == level_name] # where the target == level
            conditional_means[i] = lambda * mean(subset_target) + (1-lambda) * apriori_y # conditional (adjusted) means 
          }
          result_df = data.frame(Level = unique_levels, Value = conditional_means)
          data[[paste0("impt_enc_",col_name, sep="_")]] = result_df$Value[match(data[[col_name]], result_df$Level)]
        }
           
      
        else {
          unique_levels = unique(data[[col_name]])
          freqs = table(data[[col_name]]) #frequencies of each level
          
          for ( j in unique(data[[target]])) { #outer loop ranging on classes
            conditional_prop = rep(0,length(unique_levels)) 
            apriori_y= 1
            for (i in seq_along(unique_levels)) { #inner loop ranging along levels
              level_count = as.numeric(freqs[i])
              alpha = 1 #/ (1+exp(-level_count/smoothing_factor)) 
              level_name = names(freqs[i]) 
              cond_class_count = sum(data[[col_name]] == level_name & data[[target]] == j )
              conditional_prop[i] = alpha * (cond_class_count/level_count) + (1-alpha) * apriori_y
            }
            
            result_df = data.frame(Level = unique_levels, Value = conditional_prop)
            data[[paste0("impt_enc_",col_name,j, sep="_")]] = result_df$Value[match(data[[col_name]], result_df$Level)]
        }
      }
    } 
    else { # case for one hot
      encoded_col = model.matrix(~data[[col_name]]-1 , data = data)
      data[[col_name]]=NULL
      colnames(encoded_col) = gsub("data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      data = cbind(data, encoded_col) # add new encoded columns
      
    }
  }

  return(data)
}




impact_encoding_tt = function(train_data,test_data,target,threshold, smoothing_factor = 0.0001 ){
  
  if (is_binary(train_data[[target]])){
    train_data[[target]]= as.numeric(train_data[[target]])-1 # this solves the case in which target is binary factor 
  }
  categorical_cols = sapply(train_data, function(col) is.factor(col) || is.character(col))# check whether a colummn is categorical , return a T F vector
  
  for (col_name in names(train_data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
    train_data[[col_name]]= factor(train_data[[col_name]]) # work with factors so we can use nlevels instead of unique
    col_levels = nlevels(train_data[[col_name]])
    
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    
    if (col_levels > threshold) {
      
      
      if (is.numeric(train_data[[target]])) { # case when target is (ex)binary or numeric
        
        #unique_levels = unique(train_data[[col_name]])
        levs= levels(train_data[[col_name]])
        conditional_means = rep(0, col_levels) 
        apriori_y= sum(train_data[[target]])/length(train_data[[target]]) #  global y_mean 
        freqs=table(train_data[[col_name]]) # frequencies for each level 
        
        for (i in seq_along(levs)) {
          level_count = as.numeric(freqs[i])
          lambda = 1 / (1+exp(-level_count/smoothing_factor)) #weighting factor
          level_name = names(freqs[i])
          subset_target = train_data[[target]][train_data[[col_name]] == level_name] # where the target == level
          conditional_means[i] = lambda * mean(subset_target) + (1-lambda) * apriori_y # conditional (adjusted) means 
          conditional_means[is.nan(conditional_means)]= 0
        }
        
        result_df = data.frame(Level = levs, Value = conditional_means)
        train_data[[paste0("impt_enc_",col_name, sep="_")]] = result_df$Value[match(train_data[[col_name]], result_df$Level)]
        test_data[[paste0("impt_enc_",col_name, sep="_")]] = result_df$Value[match(test_data[[col_name]], result_df$Level)]
        last_col=colnames(test_data)[ncol(test_data)]
        test_data[[last_col]][is.na(test_data[[last_col]])]= 0
      }
      
      else {
        #unique_levels = unique(train_data[[col_name]])
        train_data[[target]]= factor(train_data[[target]])
        levs= levels(train_data[[col_name]])
        freqs = table(train_data[[col_name]]) #frequencies of each level
        
        for ( j in levels(train_data[[target]])) { #outer loop ranging on classes
          conditional_prop = rep(0,col_levels) 
          apriori_y= 1
          for (i in seq_along(levs)) { #inner loop ranging along levels
            level_count = as.numeric(freqs[i])
            alpha = 1/ (1+exp(-level_count/smoothing_factor)) 
            level_name = names(freqs[i]) 
            cond_class_count = sum(train_data[[col_name]] == level_name & train_data[[target]] == j )
            conditional_prop[i] = alpha * (cond_class_count/level_count) + (1-alpha) * apriori_y
            conditional_prop[is.nan(conditional_prop)]= 0
          }
          
          result_df = data.frame(Level = levs, Value = conditional_prop)
          train_data[[paste0("impt_enc_",col_name,j, sep="_")]] = result_df$Value[match(train_data[[col_name]], result_df$Level)]
          test_data[[paste0("impt_enc_",col_name,j, sep="_")]] = result_df$Value[match(test_data[[col_name]], result_df$Level)]
          last_col=colnames(test_data)[ncol(test_data)]
          test_data[[last_col]][is.na(test_data[[last_col]])]= 0
        }
      }
    } 
    else { # case for one hot
      encoded_col = model.matrix(~train_data[[col_name]]-1 , data = train_data)
      #train_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("train_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      train_data = cbind(train_data, encoded_col) # add new encoded columns
      # same for test
      encoded_col = model.matrix(~test_data[[col_name]]-1 , data = test_data)
      #test_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("test_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      test_data = cbind(test_data, encoded_col) # add new encoded columns
    }
    
    
    train_data[[col_name]]= NULL
    test_data[[col_name]]= NULL 
    
  }
  
  
  
  return(list(train=train_data, test= test_data))
}






#------------------------

#install.packages("digest")


library(dplyr)
library(Matrix)


simple_hash = function(input, upper) {
  hash_val = (sum(utf8ToInt(input)) %% upper) + 1
  return(hash_val)
}

hash_encoding= function(data, threshold, num_hash_buckets  ) {
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col))
  encoded_data = data
  indicator_matrix = matrix(0, nrow = nrow(data), ncol = num_hash_buckets)
  colnames(indicator_matrix) = paste0("hash_", seq_len(num_hash_buckets))
  
  
  for (col_name in names(data)[categorical_cols]) {
    col_levels = length(unique(data[[col_name]]))
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    if (col_levels > threshold) {
      hash_encoded_col = sapply(as.vector(data[[col_name]]),simple_hash, upper=num_hash_buckets )
      #encoded_data[[paste0("hash_map_", col_name)]] = hash_encoded_col # this append the mapping
        
      for (i in 1:nrow(data)) {
        indicator_matrix[i, hash_encoded_col[i]] = 1
        }
      
    encoded_data[[col_name]]= NULL
    
    } 
    
    else {
      one_hot_encoded_cols = model.matrix(~data[[col_name]] - 1, data = data) # one hot encoding
      col_names = colnames(one_hot_encoded_cols) # estraggo i nomi delle colonne nella 01 matrix
      colnames(one_hot_encoded_cols) = gsub("data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), col_names) 
      encoded_data = cbind(encoded_data, one_hot_encoded_cols)
    }
    
   } 
  
  
  if (max(indicator_matrix)==1){ # bind the matrix iff there's something to be encoded
  encoded_data = cbind(encoded_data, indicator_matrix)
  }  
  
  
  return(encoded_data)
}


hash_encoding_tt= function(train_data, test_data, threshold, num_hash_buckets=500){
  train_enc= hash_encoding(train_data, threshold, num_hash_buckets = num_hash_buckets)
  test_enc= hash_encoding(test_data, threshold, num_hash_buckets = num_hash_buckets)
  
  return(list(train=train_enc, test= test_enc))
}




remove_encoding_tt= function(train_data, test_data, threshold){
  categorical_cols = sapply(train_data, function(col) is.factor(col) || is.character(col))# check whether a colummn is categorical , return a T F vector
  
  for (col_name in names(train_data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
    train_data[[col_name]]= factor(train_data[[col_name]]) # work with factors so we can use nlevels instead of unique
    col_levels = nlevels(train_data[[col_name]])
  
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    
    if (col_levels > threshold){
      train_data[[col_name]]=NULL
      test_data[[col_name]]=NULL
    }
    else{
      encoded_col = model.matrix(~train_data[[col_name]]-1 , data = train_data)
      #train_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("train_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      train_data = cbind(train_data, encoded_col) # add new encoded columns
      # same for test
      encoded_col = model.matrix(~test_data[[col_name]]-1 , data = test_data)
      #test_data[[col_name]]=NULL
      colnames(encoded_col) = gsub("test_data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
      test_data = cbind(test_data, encoded_col) # add new encoded columns
      
      
      train_data[[col_name]]= NULL
      test_data[[col_name]]= NULL 
      
    }
    
    
      
  }
    
  return(list(train=train_data, test=test_data))  
    
}


#-----------
one_hot_encoding= function(data, target){
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col)) # check whether a column is categorical , return a T F vector
  
  for (col_name in names(data)[categorical_cols]) {# loops only on categorical columns because of the T/F vector 
    if (col_name == target){ # avoids encoding the target when it's categorical 
      next 
    }
    
    data[[col_name]] = factor(data[[col_name]])
    levels(data[[col_name]]) = c(levels(data[[col_name]]), "Other")
    freqs= table(data[[col_name]])# first freq to get rare levels
    data[[col_name]][freqs[data[[col_name]]]<(0.01*length(data[[col_name]]))]= "Other" # if a certain has not at least a 1% frequency
    freqs= table(data[[col_name]]) # second freq to get new level counts
    levels(data[[col_name]])= droplevels(data[[col_name]], exclude= names(freqs[freqs==0])) # save new levels, excluding the collapsed ones
    
    if (freqs["Other"] == length(data[col_name])){ # case of id, drop the column if all levels are rare
      data[[col_name]] = NULL
      next
    }
    
    encoded_col = model.matrix(~data[[col_name]]-1 , data = data)
    data[[col_name]]=NULL
    colnames(encoded_col) = gsub("data\\[\\[col_name\\]\\]", paste0(col_name, sep="_"), colnames(encoded_col)) 
    data = cbind(data, encoded_col) # add new encoded columns
  } 
  
  
  
  return (data)
  
}
#-------------

dummy_encoding= function(data, target){
  
  c_data=copy(data)
  c_data[[target]]= NULL # remove the target
  dmy = dummyVars(" ~ .", data = c_data, fullRank = T) #dummy 
  dmy_data = data.frame(predict(dmy, newdata = c_data))
  cbind(dmy_data, data[[target]])# put the target back
  
  return(dmy_data)
  
  
  
  
  
}

leaf_encoding_train <- function(training, target, threshold) {
  data <- training 
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col)) # trovo categoriche di cui fare la codifica
  most_common_leaves <- list()
  output_table <- data.frame()
  
  for (col_name in names(data)[categorical_cols]){
    if (is.character(data[[col_name]])) {  # trasformo chr in factor 
      new_col <- factor(data[[col_name]])
      data[[col_name]] <- new_col
    }
    
    col_levels = length(unique(data[[col_name]]))
    if (col_name == target){ 
      next 
    }
    if (col_levels > threshold){
      
      # addestro decision tree
      predittore = data[[col_name]]
      target_col = data[[target]]
      if ((length(unique(target_col)) > 2) && (class(target_col) == "factor" || class(target_col) == "character")) { # classificazione multiclasse 
        model = ranger(target_col ~ ., data = data.frame(predittore, target_col), num.trees = 1)
        leaf_indices = getTerminalNodeIDs(model, dat = data.frame(predittore, target_col))
      } else {
        model = rpart(target_col ~ predittore, data = data)
        leaf_indices = rpart.predict.leaves(model, data, type = "where")
      }
      
      new_col_name <- paste(col_name, "LeafID", sep = "_")
      data[[new_col_name]] = as.factor(leaf_indices)  # creo la nuova colonna nel nuovo dataframe che contiene gli ID 
      
      # creo tabella codifica
      tmp = unique(data.frame(Livello = data[[col_name]], Codifica = data[[new_col_name]]))
      new_colonna <- paste(col_name, "_encoded", sep = "")
      colnames(tmp)[colnames(tmp) == "Livello"] <- col_name
      colnames(tmp)[colnames(tmp) == "Codifica"] <- new_colonna
      output_table = cbind.fill(output_table, tmp)
      rownames(output_table) <- NULL
      
      
      # codifica one hot della colonna ID 
      if (length(unique(data[[new_col_name]])) >1){
        one_hot_encoded_col = model.matrix(~data[[new_col_name]] - 1, data = data) # one hot encoding
        col_names = colnames(one_hot_encoded_col) # estraggo i nomi delle colonne nella 01 matrix
        colnames(one_hot_encoded_col) = gsub("data\\[\\[new_col_name\\]\\]", paste0("",col_name, sep="_"), col_names)
        data = cbind(data, one_hot_encoded_col)
        # View(data)
        
        # trovo nodo foglia più popoloso - serve per i nuovi livelli osservati durante la predizione test set 
        leaf_counts <- table(leaf_indices)
        most_common_leaf <- as.integer(names(leaf_counts)[which.max(leaf_counts)])
        most_common_leaves[[col_name]] <- most_common_leaf
        # print("cane")
        data[[new_col_name]] = NULL # per droppare colonna contenente LeafID 
      } else {
        data[[new_col_name]] = NULL  # la droppo perchè è costante
      }
    }  else {   # solo one hot perchè numero livelli < THR 
      
      encoded_data <- model.matrix(~ data[[col_name]] - 1, data = data)
      col_names <- colnames(encoded_data)
      colnames(encoded_data) <- gsub("data\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      data <- cbind(data, encoded_data)
    }
    
    data[[col_name]] = NULL  # per droppare colonna originale che è stata codificata
  }
  
  return(list(data = data, most_common_leaves = most_common_leaves, output_table = as.data.frame(output_table)))
}


leaf_encoding_test <- function(test_data, encoded_train, target, most_common_leaves, output_table){
  data = test_data
  encoded_train_cols = colnames(encoded_train)
  categorical_cols = sapply(data, function(col) is.factor(col) || is.character(col))
  for (col_name in names(data)[categorical_cols]){
    new_col_name <- paste(col_name, "_encoded", sep = "")
    if (col_name == target) {
      next
    }
    
    if (col_name %in% names(output_table)){ # questa colonna nel train era stata codificata con i leafID - ie superava la THR
      encoding_df = output_table[, c(col_name, new_col_name)]
      
      # check nuovi livelli 
      #new_levels = setdiff(data[[col_name]], encoding_df[[col_name]]) # elementi presenti nella colonna nel test ma non nel training
      #if (length(new_levels) > 0) {
      # new_levels_encoded <- rep(most_common_leaves[[col_name]], length(new_levels))
      #new_levels_df <- data.frame(col_name = new_levels, new_col_name = new_levels_encoded)
      #print(new_levels_df)
      #colnames(new_levels_df) <- colnames(encoding_df)
      #encoding_df <- rbind(encoding_df, new_levels_df)
      #}
      
      
      data = merge(data, encoding_df, by= col_name)
      for (column in colnames(data)) {
        if (column %in% colnames(output_table)) {
          na_indices <- is.na(data[[column]])
          if (any(na_indices)) {
            codifica <- output_table[[column]]
            data[[column]][na_indices] <- codifica[na_indices]
          }
        }
      }
      
      # codifica one hot della colonna ID 
      if (length(unique(data[[new_col_name]])) >1){
        one_hot_encoded_col = model.matrix(~data[[new_col_name]] - 1, data = data) # one hot encoding
        col_names = colnames(one_hot_encoded_col) # estraggo i nomi delle colonne nella 01 matrix
        colnames(one_hot_encoded_col) = gsub("data\\[\\[new_col_name\\]\\]", paste0("",col_name, sep="_"), col_names)
        data = cbind(data, one_hot_encoded_col)
      } 
      data[[new_col_name]] = NULL 
    } else { # fai solo one hot encodingdi quella colonna e prosegui
      one_hot_encoded_col = model.matrix(~data[[col_name]] - 1, data = data)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("data\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      data = cbind(data, one_hot_encoded_col)
    }
    
    data[[col_name]] = NULL
  }
  for (colonna in encoded_train_cols) {
    if (!(colonna %in% names(data))) {
      data[[colonna]] <- 0
    }
  }
  for (colonna in colnames(data)) {
    if (!(colonna %in% encoded_train_cols)){
      data[[colonna]] = NULL 
    }
  }
  print(length(encoded_train_cols) == length(colnames(data)))
  return(data)
  
}

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}


