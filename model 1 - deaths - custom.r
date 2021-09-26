rm(list = ls())

getwd()


library(Synth)
library(readxl)

dataset <- read_excel("data for model 1.xlsx")
dataset <- as.data.frame(dataset)

treatment_date <- 320

dataset1 <- dataset[ which(dataset$date >= 100 & dataset$date < 397), ] # from April 2020 to January 2021 (January 30th included)


dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", "population",
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 200:treatment_date,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",250:treatment_date,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 15, # Georgia      
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,14,17,18,19,20,21,22,23,24, 26,27,28, 29), # excluded 4,16,25 because no data for stringency index
  #Time period for establishing synthetic control, pre treatment
  time.optimize.ssr = 100:treatment_date, 
  #Entire time period
  time.plot = 100:388)

synth.out <- synth(data.prep.obj = dataprep.out) 
#summary(synth.out)
path.plot(synth.res = synth.out, dataprep.res = dataprep.out, 
          Ylab = "New deaths per million people", Xlab = "", 
          Ylim = c(0, 15), Legend = c("Georgia",
                                      "Synthetic Georgia"), Legend.position = "topleft")
abline(v=treatment_date,lty="dotted",lwd=5, col="red")
text(255,12,"2020 Parliamentary Elections in Georgia? (November 15, 2020)", cex = 1.2)

synth_tab <- synth.tab(dataprep.res = dataprep.out,
                       synth.res = synth.out)
print(synth_tab)

?path.plot

library(tidyverse)
gaps = (dataprep.out$Y1plot-(dataprep.out$Y0plot %*% synth.out$solution.w))
gaps[1:100]
gaps
options(scipen = 10)

synth.tables = synth.tab(dataprep.res = dataprep.out,
                         synth.res = synth.out)
print(synth.tables)
synth.tables$tab.pred[1:13,]
synth.tables$tab.w

gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out, 
          Ylab = "Gap in new deaths per million people", Xlab= "Days since January 1,2020",
          Ylim = c(-5,10), Main = NA)
abline(v=treatment_date-1,lty="dotted",lwd=5, col="red")
text(250,8,"2020 Parliamentary Elections in Georgia? (November 15, 2020)", cex = 1.2)

gaps



######## test for placebo

country_names <- c("Albania", "Algeria", "Angola","Bahrain", "Barbados", "Bhutan", "Botswana", "Bulgaria", 
   "Cambodia", "Cameroon", "Congo", "Estonia", "Ethiopia", "Haiti", "Hungary", "Kazakhstan", "Kosovo",
   "Laos", "Panama", "Timor", "Latvia", "Nicaragua", "Tunisia", "Uzbekistan", "Vietnam", "Georgia")


store <- matrix(NA,length(100:388),26)
colnames(store) <- country_names


### ----------------------- from here placebo tests for each control --------------------------------------------------------------------------- 

## Albania
  dataprep.out <- dataprep(
    foo = dataset1, 
    predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                   "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
    predictors.op = 'mean',  
    time.predictors.prior = 100:244,  
    special.predictors = list(
      list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
      list('population_density',200:treatment_date ,c("mean"))),
    dependent = 'new_deaths_smoothed_per_million', 
    unit.variable = 'country_number', 
    unit.names.variable = 'country_name', 
    time.variable = 'date', 
    treatment.identifier = 1, 
    controls.identifier = c(2,3,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
    time.optimize.ssr = 100:treatment_date, 
    time.plot = 100:388)
# run synth
synth.out <- synth(
data.prep.obj = dataprep.out,
method = "BFGS"
)
# store gaps
store[,1] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Algeria
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 2, 
  controls.identifier = c(1,3,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,2] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Angola
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 3, 
  controls.identifier = c(1,2,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,3] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Bahrain
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 5, 
  controls.identifier = c(1,2,3,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,4] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Barbados
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 6, 
  controls.identifier = c(1,2,3,5,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,5] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Bhutan
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 7, 
  controls.identifier = c(1,2,3,5,6,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,6] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Botswana
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 8, 
  controls.identifier = c(1,2,3,5,6,7,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,7] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Bulgaria
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 9, 
  controls.identifier = c(1,2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,8] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Cambodia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 10, 
  controls.identifier = c(1,2,3,5,6,7,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,9] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Cameroon
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 11, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,12,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,10] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Congo
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 12, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,13,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,11] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Estonia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 13, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,14,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,12] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)




## Ethiopia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 14, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,17,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,13] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Haiti
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 17, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,18,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,14] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Hungary
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 18, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,19,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,15] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Kazakhstan
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 19, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,20,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,16] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Kosovo
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 20, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,21,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,17] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Laos
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 21, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,22,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,18] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Panama
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 22, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,23,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,19] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)




## Timor
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 23, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,24, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,20] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Latvia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 24, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,23, 26,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,21] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Nicaragua
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 26, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,23, 24,27,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,22] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)




## Tunisia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 27, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,23, 24,26,28, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,23] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)



## Uzbekistan
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 28, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,23, 24,26,27, 29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,24] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


## Vietnam
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 29, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,15,14,17,18,19,20,21,22,23, 24,26,27, 28), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,25] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)




## Georgia
dataprep.out <- dataprep(
  foo = dataset1, 
  predictors = c('new_cases_smoothed_per_million', 'stringency_index',"population_density", 
                 "diabetes_prevalence", "hospital_beds_per_thousand", "life_expectancy", "gdp_per_capita"), 
  predictors.op = 'mean',  
  time.predictors.prior = 100:244,  
  special.predictors = list(
    list("new_deaths_smoothed_per_million",200:treatment_date,c("mean")),  
    list('population_density',200:treatment_date ,c("mean"))),
  dependent = 'new_deaths_smoothed_per_million', 
  unit.variable = 'country_number', 
  unit.names.variable = 'country_name', 
  time.variable = 'date', 
  treatment.identifier = 15, 
  controls.identifier = c(1,2,3,5,6,7,8,9,10,11,12,13,14,17,18,19,20,21,22,23,24,26,27,28,29), 
  time.optimize.ssr = 100:treatment_date, 
  time.plot = 100:388)
# run synth
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS"
)
# store gaps
store[,26] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

## ---------------------------------------end here placebo tests for each control unit-------------------------------------------------------






### ----------------------------------------- plotting gaps start here --------------------------------------------------
data <- store
rownames(data) <- 100:388 # Days since January 2020, 1

gap.start     <- 1
gap.end       <- nrow(data)
days <- seq(as.Date("2020-04-09"), as.Date("2021-01-22"), by="days", format = "%m-%Y")
gap.end.pre  <- which(rownames(data)==toString(treatment_date))


# Exclude countries with 5 times higher the mean squared prediction error (MSPE) than Georgia
# if you want to anyway plot those countries with 5 times higher MSPE than Georgia, just ignore the following 4 lines  
mse        <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
geo.mse <- as.numeric(mse[26])
data <- data[,mse<5*geo.mse]
Cex.set <- .75



# plotting
plot(days,data[gap.start:gap.end,which(colnames(data)=="Georgia")],
     type="l",lwd=2,col="black",ylim=c(-20,20), xlab="", ylab="Gap in new deaths per million people",
     xaxs="i",xaxt="n",yaxs="i")

axis.Date(1, at=seq(as.Date("2020-04-09"), as.Date("2021-01-30"), by="months"), format = "%d-%b-%Y")
#plot(days,data1[gap.start:gap.end1,which(colnames(data)=="Georgia")], cex.axis=1.1,cex.lab=1.3,
#     type="l",lwd=4,col="black",ylim=c(-20,20), xlab="Days since January 1, 2020", ylab="Gap in new deaths per million people",
#     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(days,data[gap.start:gap.end,i],col="gray",lwd=2) }
# for (i in 1:ncol(data1)) { lines(days,data1[gap.start:gap.end1,i],col="gray") }

lines(days,data[gap.start:gap.end,which(colnames(data)=="Georgia")],lwd=4,col="darkgreen")
# lines(days,data[gap.start:gap.end,which(colnames(data)=="Bulgaria")],lwd=4,col="darkred")

abline(v=as.Date("2020-11-15"),lty="dotted",lwd=2, col="black")
#abline(h=0,lty="dashed",lwd=5)
#?abline
legend("topleft",legend=c("Georgia","Control countries"),
       lty=c(1,1),col=c("darkgreen","gray"),lwd=c(10,10),cex=1)

#arrows(as.Date("2020-10-20"),-15,as.Date("2020-10-27"),-15,col="black",length=.1)
text(as.Date("2020-09-20"),-15,"2020 Parliamentary Elections in Georgia", cex = 1)
# ?text
#abline(v=400)
#abline(v=1997)
#abline(h=-2)
#abline(h=2)
### ----------------------------------------- plotting gaps end here --------------------------------------------------











### ----------------------------------------- MSPE ratio start here --------------------------------------------------
post_t <- as.data.frame(data)
post_t <- post_t[205:288,]      # post treatment period


library(openxlsx)
write.xlsx(post_t, "post_treatment_gaps.xlsx")

(sum(post_t$Georgia))*3.7 -> how_many_people_died_cause_elections

pre_t <- as.data.frame(data)
pre_t <- pre_t[1:205,]        # pre treatment period

pre_mse        <- apply(pre_t^2,2,mean)
post_mse <- apply(post_t^2,2,mean)


round(post_mse/pre_mse)   ### post treatment MSPE/pre treatment MSPE
mspe_ratio <- round(post_mse/pre_mse)
class(mspe_ratio)
mspe_ratio
sort(mspe_ratio, decreasing = T)
mspe_ratio <- sort(mspe_ratio, decreasing = T)
plot(mspe_ratio, main="Post-treatment MSPE/Pre-treatment MSPE ratio", type = "h", lwd=10, xaxt="n", xlab="", ylab = "", las=1)
axis(1, at=1:21, labels = c("Georgia", "Bulgaria", "Panama", "Tunisia","Algeria",  "Uzbekistan", 
                            "Barbados", "Botswana",  "Albania","Estonia", "Ethiopia","Angola",  
                            "Vietnam", "Laos", "Bhutan", "Timor", "Cambodia", "Cameroon", 
                            "Congo", "Haiti", "Nicaragua"), las=3)

# as no other country has higher ratio than Georgia, p-value would be 1/21.
1/21


### ----------------------------------------- MSPE ratio ends here --------------------------------------------------

