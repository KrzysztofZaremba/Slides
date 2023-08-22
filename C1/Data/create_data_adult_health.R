###Create_data
library(tidyverse)

load("Data/Data_health_survey.Rda")
Data_health_survey=Data_health_survey%>%
  filter(what_is_your_usual_weight_without_clothes_and_shoes!=999)

save(Data_health_survey, file="Data/Data_health_survey.Rda")
