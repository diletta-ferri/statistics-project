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
    encoded_data = glmm_encoding_wrapper(train_set, test_set, target, thr)
    if(class(encoded_data) == "character"){
      next # smetto di iterare sulle thr
    }
    encoded_train = encoded_data$train
    encoded_test = encoded_data$test 
  }
  if (encoder == "leaf"){
    encoded_data = leaf_encoding_train(train_set, target, thr)
    encoded_train = encoded_data$data
    foglie_comuni = encoded_data$most_common_leaves
    tabella_codifica = encoding_data$output_table
    encoded_test = leaf_encoding_test(test_set, encoded_train, target, foglie_comuni, tabella_codifica)
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



#_____________ da qui parte la pipeline__________________________ 


# i dataset commentati mi hanno dato problemi nel caricamento 
codes = list( Midwest_survey = 41446, 
  Traffic_violations = 42345,
  airlines = 42493,
  ames_housing = 41211,
  avocado_sales = 41210,
  # beer = 42494, --> funziona ma ci mette troppo a caricare il dataset
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




encodings= c("integer", "impact", "frequency", "hash", "onehot", "dummy", "remove", "none")
thresholds = c(10,25,125)

for (i in 1:length(codes)){ # itero su codici i.e. sui dataset
  data_name = names(codes[i])
  id=codici[[data_name]]
  get_data=getOMLDataSet(data.id = id)
  data= get_data$data # per avere i dati tabulari
  target=get_data$target.features # le target si possono prendere cosi 
  
  data_prep = first_prep(data)
  
  # splitting
  tt=tt_split(data_prep, target, 0.3)
  train_data= tt$train
  test_data= tt$test
 
  for (encoder in encodings){# per ogni dataset, ogni encoding
    if (encoder=="none"){# none fa parte delle control conditions, l'ho messo fuori perchè non fa nulla e non ha bisgno di threshold. Di conseguenza andrebbe gestito a parte.
      encoded_train= train_data
      encoded_test= test_data
      # mettere modello
      next
      
    }
    if (encoder=="onehot"){# anche lui va messo fuori perchè non ha threshold
      # in questo caso lavoro pre-splitting perchè diventava un casino gestire i vari livelli delle categoriche 
      #e si rischiava di avere shape diverse tra train e test
      encoded_data= one_hot_encoding(data_prep)
      encoded_data_tt= tt_split(encoded_data, target, 0.3)
      encoded_train= encoded_data_tt$train
      encoded_test= encoded_data_tt$test
      #mettere modello
      next
      
    }
    if (encoder=="dummy"){# stesso discorso di onehot
      encoded_data= dummy_encoding(data_prep, target)
      encoded_data_tt= tt_split(encoded_data, target, 0.3)
      encoded_train= encoded_data_tt$train
      encoded_test= encoded_data_tt$test
      #mettere modello
      next
    }
    for (thr in thresholds){# per ogni encoding 
      encoded_data= megaf(data, target, encoder, thr)
      encoded_train= encoded_data$train
      encoded_test= encoded_data$test
      # mettere modello
      
    }
  }
}

#____________________________________



