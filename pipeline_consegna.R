library("OpenML")
library(farff)
library(data.table)


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



encodings= c("integer", "impact", "frequency", "hash", "onehot", "dummy", "remove", "none")
thresholds = c(10,25,125)

for (i in 1:length(codes)){ # itero su codici i.e. sui dataset
  data_name = names(codes[i])
  id=codes[[data_name]]
  get_data=getOMLDataSet(data.id = id)
  data= get_data$data # per avere i dati tabulari
  target=get_data$target.features # le target si possono prendere cosi 
  
  data_prep = first_prep(data)
  tt=tt_split(data_prep, target, 0.3)
  train_data= tt$train
  test_data= tt$test

  
