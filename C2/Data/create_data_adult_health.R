###Create_data
library(tidyverse)

load("Data/Data_health_survey.Rda")
Data_health_survey=Data_health_survey%>%
  filter(what_is_your_usual_weight_without_clothes_and_shoes!=999)

Data_health_survey$male=as.numeric(Data_health_survey$name_is_male==1)
Data_health_survey$weight=Data_health_survey$what_is_your_usual_weight_without_clothes_and_shoes
Data_health_survey$location_type=Data_health_survey$domain
Data_health_survey$diabetes=as.numeric(Data_health_survey$has_a_doctor_ever_told_you_that_you_have_diabetes_or_high_blood_sugar==1)

Data_health_survey$Mother_diabetes=as.numeric(Data_health_survey$does_your_family_member_have_or_have_had_diabetes_or_high_blood_sugar_mother)
Data_health_survey$Mother_diabetes=as.numeric(Data_health_survey$Mother_diabetes==1)

Data_health_survey$Difficulty_walking=Data_health_survey$do_you_have_difficulty_walking_or_climbing_steps

Data_health_survey<- Data_health_survey %>%
  mutate(Difficulty_walking = recode(Difficulty_walking, 
                             `1` = "No difficulty", 
                             `2` = "Some difficulty", 
                             `3` = "A lot of difficulty",
                             `4` = "Impossible"))

Data_health_survey<- Data_health_survey %>%
  mutate(gender = recode(name_is_male, 
                                     `1` = "Male", 
                                     `2` = "Female"))

Data_health_survey<- Data_health_survey %>%
  mutate(location_type = recode(domain, 
                         `1` = "Urban", 
                         `2` = "Rural"))

Data_health_survey$Cigarettes=Data_health_survey$on_average_how_many_cigarettes_do_you_currently_smoke_per_day

Health_data=Data_health_survey[,c("age","gender","weight","location_type","diabetes","Mother_diabetes","Difficulty_walking")]

Health_data$weight=round(Data_health_survey$weight+runif(nrow(Health_data),0,1),4)
  
save(Data_health_survey, file="Data/Data_health_survey.Rda")
save(Health_data, file="Data/Health_data.Rda")
write.csv2(Health_data, file="Data/Health_data.csv")
