library("OpenML")
library(farff)
library(data.table)
library("RWeka")
source("encoding_functions.R") # per chiamare funzioni da un altro script



#__________prove varie__________________________________
# set configuration per openML -  API key to read only
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f", arff.reader = "farff")
oml_dat = getOMLDataSet(data.id = 41211)
desc = oml_dat$desc
data = as.data.table(oml_dat$data)
View(data)
oml_dat[["desc"]]
desc$default.target.attribute
#________________________________________


megaf= function(data,target, encoder){ # le funzioni che chiamo in megaf ce le ho in uno script a parte
  
  # handle missing values and remove constant columns
  data = first_prep(data)
  
  # splitting
  test_ratio = 0.3 
  set.seed(123)
  split_indices = createDataPartition(data, p = test_ratio, list = FALSE)
  train_data = data[-split_indices, ]
  test_data = data[split_indices, ]
  
  
  for (thr in thresholds){ 
    # encoding. Per ora le funzioni sono uguali sia per train che test, TODO in corso
    if (encoder == "integer"){
      encoded_train = integer_encoding(train_data, thr)
      encoded_test = integer_encoding(test_data, thr)
    }
    
    if (encoder == "frequency"){
      encoded_train = frequency_encoding(train_data, thr)
      encoded_test = frequency_encoding(test_data, thr)
    }
    
    if (encoder == "impact"){
      encoded_train = impact_encoding(train_data, thr, target)
      encoded_test = impact_encoding(test_data, thr, target)
    }
    
    if (encoder == "hash"){
      encoded_train = hash_encoding(train_data, thr, target)
      encoded_test = hash_encoding(test_data, thr, target)
    }
    
    # removing costants again
    
    encoded_train = drop_cost(encoded_train)
    encoded_test = drop_cost(encoded_test)
    
    # final one hot encoding 
    
    ohe_train = dummyVars(" ~ .", data = encoded_train)
    ohe_test = dummyVars(" ~ .", data = encoded_test)    
    final_train = data.frame(predict(ohe_train, newdata = encoded_train))
    final_test = data.frame(predict(ohe_train, newdata = encoded_test))
  
    
    #TODO fit, predict and get_results
    
    
    
    
    }   
}



#_____________ da qui parte la pipeline__________________________ 


# i dataset commentati mi hanno dato problemi nel caricamento 
codes = list( Midwest_survey = 41446, 
  Traffic_violations = 42345,
  airlines = 42493,
  ames_housing = 41211,
  avocado_sales = 41210,
  # beer = 42494, --> ci mette troppo a caricare, forse è pesantissimo
  churn = 41283,
  click_prediction_small = 41220,
  delays_zurich = 42495,
  employee_salaries = 41445,
  flight_delays = 41251,
  hpc_job_scheduling = 41212,
  kdd98 = 42343,
  medical_charges = 41444 ,
  nyc_taxi = 42208,
  okcupid = 41278,
  open_payments = 41442,
  particulate_matter_ukair = 42207,
  porto_seguro = 42206,
  # road_safety_driver_sex = 41447,--> da errore
  seattlecrime6 = 42496,
  # sf_police_incidents = 42344, --> da errore
  video_game_sales = 41216,
  wine_reviews = 41437
)


#auc_res= data.frame()
#aunu_res= data.frame()
#rmse_res= data.frame()

encodings= c("integer", "impact", ...)
thresholds = c(10,25,125)
for (i in 1:length(codici)){
  data_name = names(codici[i])
  id=codici[[data_name]]
  get_data=getOMLDataSet(data.id = id)
  data= get_data$data # per avere i dati tabulari
  target=get_data$target.features # le target si possono prendere cosi 
  
  for (encoder in encodings){
    result = megaf(data,target,encoder) 
    # TODO ASSEGNAZIONE RISULTATI 
    # l'idea sarebbe avere come risultato di ciascuna chiamata di megaf un vettore o lista del tipo:
    #--> (data= paste0(data_name,encoder) thr1= 0.90, thr2= 0.85, thr3= 0.77). QUsta poi andrebbe inserita in una dei 3 dataframe sopra in base al tipo di target
  }
}



