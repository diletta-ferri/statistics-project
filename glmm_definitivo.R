glmm_encoding_wrapper = function(train, test, target, threshold) {
  if (is.factor(train[[target]]) && length(unique(train[[target]])) == 2) {
    return(glmm_encoding_binary(train, test, target, threshold))
  } else if (is.factor(train[[target]]) && length(unique(train[[target]])) > 2) {
    # return(glmm_encoding_multiclass(train, test, target, threshold))
    return("passa al prossimo dataset - multiclasse troppo lenta con glmm")
  } else {
    return(glmm_encoding_regression(train, test, target, threshold))
  }
}

glmm_encoding_regression = function(train, test, target, threshold){
  categorical_cols = sapply(train, function(col) is.factor(col) || is.character(col))# trovo categoriche di cui fare la codifica
  for (col_name in names(train)[categorical_cols]){
    if (is.character(train[[col_name]])) { 
      new_col <- factor(train[[col_name]])
      train[[col_name]] <- new_col
    }
    
    col_levels = length(unique(train[[col_name]]))
    
    if (col_name == target){ 
      next 
    }
    
    if (col_levels > threshold){
      category_means <- train %>%
        group_by(train[[col_name]]) %>%
        summarize(mean_target = mean(train[[target]]))
      
      # fit glmm
      formula_str = paste(target, "~ 1 + (1 |", col_name, ")")  
      glmm_formula = as.formula(formula_str)
      
      glmm_model = lmer(glmm_formula, data = train)
      random_intercepts = ranef(glmm_model)[[1]]
      # encoding
      encoding = category_means$mean_target + random_intercepts
      
      ## JOIN
      # trasformo encoding in un df per fare poi il join
      encoding_df <- data.frame(encoding)
      
      encoding_df[[col_name]] = row.names(encoding_df) 
      encoding_df[[col_name]] = as.character(encoding_df[[col_name]])
      train[[col_name]] = as.character(train[[col_name]])
      
      train = merge(train, encoding_df, by = col_name)
      test = merge(test, encoding_df, by = col_name)
      new_col_name = paste(col_name, "_encoded", sep = "")
      colnames(train)[colnames(train) == "X.Intercept."] = new_col_name
      colnames(test)[colnames(test) == "X.Intercept."] = new_col_name
      
      # check na
      test <- mutate(test, !!new_col_name := ifelse(is.na(test[[new_col_name]]), as.numeric(fixed), test[[new_col_name]]))
    } else {
      one_hot_encoded_col = model.matrix(~train[[col_name]] - 1, data = train)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("train\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      train = cbind(train, one_hot_encoded_col)
      
      one_hot_encoded_col = model.matrix(~test[[col_name]] - 1, data = test)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("test\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      test = cbind(test, one_hot_encoded_col)
    }
    
    train[[col_name]] = NULL
    test[[col_name]] = NULL
  }
  
  return(list(train = train, test = test))
}


glmm_encoding_binary = function(train, test, target, thr){  
  categorical_cols <- sapply(train, function(col) is.factor(col) || is.character(col)) # trovo colonne categoriche
  for (col_name in names(train)[categorical_cols]){   # ciclo sulle categoriche per codificarle
    cat_levels = unique(train[[col_name]])    
    if (col_name == target){   
      next
    } 
    if (length(cat_levels) > as.integer(thr)){
      predittore = train[[col_name]]
      model = glmer(factor(train[[target]]) ~ (1 + (1 | predittore)), data = train, family = binomial)
      fixed = fixef(model)
      
      codifica = data.frame(ranef(model)[[1]] + fixed)
      col_name_encoded = paste(col_name, "encoded", sep = "_")
      colnames(codifica)[colnames(codifica) == "X.Intercept."] = col_name_encoded
      codifica = rownames_to_column(codifica, var = col_name)
      
      train = left_join(train, codifica, by = col_name)
      test = left_join(test, codifica, by = col_name)
      temp_col_name_encoded = col_name_encoded  # Copia temporanea del nome della colonna
      train = mutate(train, !!temp_col_name_encoded := ifelse(train[[target]] == 1, train[[col_name_encoded]], train[[col_name_encoded]] * -1))
      test = mutate(test, !!temp_col_name_encoded := ifelse(test[[target]] == 1, test[[col_name_encoded]], test[[col_name_encoded]] * -1))
      
      # gestione dei na nel test dovuti ai nuovi livelli - codifica usando intercetta fissa
      test = mutate(test, !!temp_col_name_encoded := ifelse(is.na(test[[temp_col_name_encoded]]), as.numeric(fixed), test[[temp_col_name_encoded]]))
      
    } else {
      one_hot_encoded_col = model.matrix(~train[[col_name]] - 1, data = train)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("train\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      train = cbind(train, one_hot_encoded_col)
      
      one_hot_encoded_col = model.matrix(~test[[col_name]] - 1, data = test)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("test\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      test = cbind(test, one_hot_encoded_col)
    }
    
    train[[col_name]] = NULL
    test[[col_name]] = NULL
  }
  
  return(list(train = train, test = test))
} 



glmm_encoding_multiclass = function(train, test, target, threshold) {  # funziona ma il running time è elevatissimo
  
  target_levels <- unique(train[[target]])   
  categorical_cols <- sapply(train, function(col) is.factor(col) || is.character(col))
  for (col_name in names(train)[categorical_cols]){   
    cat_levels = unique(train[[col_name]])    
    if (col_name == target){  
      next
    }  
    if (length(cat_levels) > as.integer(threshold)){   
      encoding_results <- data.frame(col_name = character(0), target = character(0), encoded = numeric(0))  # creo un dataframe
      for (cat_level in cat_levels){   # per ogni livello della categorica e per ogni livello della target costruisco un modello diverso 
        for (target_level in target_levels){
          predittore = train[[col_name]]
          model <- glmer(factor(train[[target]] == target_level) ~ (1 + (1 | predittore )), data = train, family = binomial,  control = glmerControl(calc.derivs = FALSE))
          coef_values <- fixef(model)  
          random_effects <- ranef(model)[[1]]
          total_encoding <- as.numeric(coef_values[1]) + as.numeric(random_effects[[cat_level, "(Intercept)"]])  # calcolo encoding totale
          encoding_results <- rbind(encoding_results, data.frame(col_name = cat_level, target = target_level, encoded = total_encoding))
        }
      }
      encoding_df <- encoding_results[, c("col_name", "target", "encoded")]
      col_name_encoded = paste(col_name, "encoded", sep = "_")  
      target_col_name = target  
      encoding_df = rename(encoding_df, !!col_name := col_name, !!col_name_encoded := encoded, !!target_col_name := target)
      
      train = left_join(train, encoding_df, by = c(col_name, target))
      test = left_join(test, encoding_df, by = c(col_name, target))
      
      test <- mutate(test, !!col_name_encoded := ifelse(is.na(test[[col_name_encoded]]), as.numeric(fixed), test[[col_name_encoded]]))
    } else {
      one_hot_encoded_col = model.matrix(~train[[col_name]] - 1, data = train)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("train\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      train = cbind(train, one_hot_encoded_col)
      
      one_hot_encoded_col = model.matrix(~test[[col_name]] - 1, data = test)
      col_names = colnames(one_hot_encoded_col)
      colnames(one_hot_encoded_col) = gsub("test\\[\\[col_name\\]\\]", paste0("", col_name, sep="_"), col_names)
      test = cbind(test, one_hot_encoded_col)
    }
    train[[col_name]] = NULL
    test[[col_name]] = NULL 
  }
  
  return(list(train = train, test = test))
}




