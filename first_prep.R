first_prep = function(data) {
  col_names = colnames(data)
  
  for (col in col_names) {
    if (is.numeric(data[[col]])) {
      col_mean = mean(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- col_mean
    } else if (is.logical(data[[col]]) || length(unique(data[[col]])) == 2) {
      col_mode = names(sort(-table(data[[col]], useNA = "always")))[1]
      data[[col]][is.na(data[[col]])] <- as.logical(col_mode)
    } else if (is.factor(data[[col]]) || (is.character(data[[col]]) && length(unique(data[[col]])) > 2)) {
      levels = unique(data[[col]])
      levels(data[[col]]) <- c(levels(data[[col]]), "Missing")
      data[[col]][is.na(data[[col]])] <- "Missing"
    } else if (length(unique(data[[col]])) == 1) {
      data[[col]] = NULL
    }
  }
  
  return(data)
}
