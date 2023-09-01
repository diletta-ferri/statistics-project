library("OpenML")
library(farff)
library(data.table)
source("miscellaneous_code.R")
source("encoding_functions.R")

setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f", arff.reader = "farff")


codes = list( Midwest_survey = 41446, 
              Traffic_violations = 42345, 
              airlines = 42493, 
              ames_housing = 41211,
              avocado_sales = 41210, 
              beer = 42494,
              churn = 41283,  
              click_prediction_small = 41220, 
              delays_zurich = 42495, 
              employee_salaries = 41445, 
              flight_delays = 41251, 
              hpc_job_scheduling = 41212, 
              kdd98 = 42343, 
              medical_charges = 41444 ,
              nyc_taxi = 42208, 
              cupid = 41278,
              open_payments = 41442, 
              particulate_matter_ukair = 42207, 
              porto_seguro = 42206, 
              road_safety_driver_sex = 41447, 
              seattlecrime6 = 42496,
              sf_police_incidents = 42344, 
              video_game_sales = 41216,
              wine_reviews = 41437 
)



encodings = c("integer", "impact", "frequency", "hash", "onehot", "dummy", "remove", "leaf", "glmm")
thresholds = c(10,25,125)
results = list()
merged_results = data.frame()
for (i in 1:length(codes)){ # iterazione su codici i.e. sui dataset
  data_name = names(codes[i])
  id=codes[[data_name]]
  get_data=getOMLDataSet(data.id = id)
  data= get_data$data # per avere i dati tabulari
  target=get_data$target.features # estrazione target
  
  data_prep = first_prep(data)
  tt=tt_split(data_prep, target, 0.3)
  train_data= tt$train
  test_data= tt$test

  for (encoder in encodings){   # per ogni dataset, ogni encoding
    if (encoder=="onehot"){
    encoded_data= one_hot_encoding(data_prep,target)
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    dataset_risultati= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, dataset_risultati, encoder)
    next
    
    }
    if (encoder=="dummy"){ 
    encoded_data= dummy_encoding(data_prep, target)
    encoded_data_tt= tt_split(encoded_data, target, 0.3)
    encoded_train= encoded_data_tt$train
    encoded_test= encoded_data_tt$test
    dataset_risultati= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, dataset_risultati, encoder)
    next
    }
    for (thr in thresholds){   # per ogni encoding 
    encoded_data= get_encoded_data(data_prep, target, encoder, thr)
    encoded_train= encoded_data$train
    encoded_test= encoded_data$test
    encandthr = paste0(encoder, thr) #nome colonna per i risultati
    dataset_risultati= NEURALNETWORK(tipo_problema, target, encoded_train, encoded_test, dataset_risultati, encandthr)
    
    
    }
  }
  
dataset_risultati$dataset_id = data_name
merged_results = bind_rows(merged_results, dataset_risultati)
}

  
