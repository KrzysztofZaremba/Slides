library(tidyverse)

##data from keggle

stations_rsinaica <- read.csv("C:/Users/kzysi/Desktop/Polution_data_mexico/stations_rsinaica.csv")

##Pick only stations from mexico city
Relevant_stations_codes=stations_rsinaica$station_id[stations_rsinaica$network_code=="CMX"]
rm(stations_rsinaica)

## load data 
stations_daily <- read.csv("C:/Users/kzysi/Desktop/Polution_data_mexico/stations_daily.csv")

##keep only stations from CMX
stations_daily=stations_daily%>%
  filter(station_id%in%Relevant_stations_codes)

Daily_CMX=stations_daily%>%
  group_by(datetime) %>%
  summarise(across(-station_id, ~mean(., na.rm = TRUE)))%>%
  filter(as.Date(datetime)>=as.Date("2017-01-01"))
Daily_CMX$datetime=as.Date(Daily_CMX$datetime)

save(Daily_CMX, file="Daily_CMX_means.Rda")

ggplot(Daily_CMX, aes(x = datetime, y = TMP)) +
  geom_line() +
  labs(
    x = "Average Temperature",
    y = "Number of Bike Trips"
  ) +
  theme_minimal()


ggplot(Daily_CMX, aes(x = TMP, y = NOx)) +
  geom_point() +
  labs(
    x = "Date and Time",
    y = "PM2.5 Level",
    title = "PM2.5 Levels Over Time"
  ) +
  theme_minimal()

###Biking data
data.2023.09.13 <- read.csv("C:/Users/kzysi/Desktop/Eco_bici_data/data-2023-09-13.csv")
Bikes_CMX=data.2023.09.13%>%
  group_by(fecha_retiro)%>%
  summarize(Trips=sum(viajes, na.rm=TRUE))

Data_BP=merge(Bikes_CMX, Daily_CMX, by.x="fecha_retiro", by.y="datetime")
Data_BP$fecha_retiro=as.Date(Data_BP$fecha_retiro)
Data_BP=Data_BP[Data_BP$fecha_retiro<as.Date("2020-01-01"),]
Data_BP$day_of_week<- weekdays(Data_BP$fecha_retiro)
Data_BP$is_weekend <- ifelse(Data_BP$day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

##keep only weekdays?

Data_BP=Data_BP[Data_BP$is_weekend=="Weekday",]

names(Data_BP)[1]="Date"
Data_BP$TMP=round(Data_BP$TMP,2)
Data_BP$PM2.5=round(Data_BP$PM2.5,2)

ggplot(Data_BP, aes(x = fecha_retiro, y = Trips)) +
  geom_line() +
  labs(
    x = "Date and Time",
    y = "PM2.5 Level",
    title = "PM2.5 Levels Over Time"
  ) +
  theme_minimal()


ggplot(Data_BP, aes(x = TMP, y = Trips)) +
  geom_point() +
  labs(
    x = "Date and Time",
    y = "PM2.5 Level",
    title = "PM2.5 Levels Over Time"
  ) +
  theme_minimal()+
  ylim(0,NA)

save(Data_BP, file="Daily_CMX_means2.Rda")

a=lm(Trips~ TMP, data=Data_BP)
summary(a)
