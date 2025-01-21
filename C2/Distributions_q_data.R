### female weight
load("Data/Health_data.Rda")
Female_weights=Health_data[Health_data$gender=="Female",]
Female_weights$Variable="Female weights"
Female_weights=Female_weights[,c("Variable","weight")]
names(Female_weights)=c("Variable","Value")

### Salaries
load("C:/Users/kzysi/Dropbox/Itam_teaching/Data/ENOE_salaries.Rda")
cd$Variable="Salaries"
cd=cd[,c("Variable","Income")]
names(cd)=c("Variable","Value")
#cut the top 5% - avove 95 percentile
cd=cd[cd$Value<quantile(cd$Value,0.95, na.rm=TRUE),]


### Airbnb prices
load("C:/Users/kzysi/Dropbox/Itam_teaching/Markdowns/Intro_to_r/listings.Rda")
listings$Variable="Airbnb prices"
listings=listings[,c("Variable","price")]
#clean price variable, keep only numbers and .
listings$price=as.numeric(gsub("[^0-9.]", "", listings$price))
names(listings)=c("Variable","Value")
#cut the top 5% - avove 95 percentile
listings=listings[listings$Value<quantile(listings$Value,0.95, na.rm=TRUE),]

###Birthdates
sinac2012DatosAbiertos <- read.csv("C:/Users/kzysi/Dropbox/Birth_Mexico/Data/sinac2012DatosAbiertos.csv")
table(sinac2012DatosAbiertos$fecha_nacimiento_nac_vivo)
sinac2012DatosAbiertos$Variable="Birthdates"
sinac2012DatosAbiertos=sinac2012DatosAbiertos[,c("Variable","fecha_nacimiento_nac_vivo")]
names(sinac2012DatosAbiertos)=c("Variable","Value")

sinac2013DatosAbiertos <- read.csv("C:/Users/kzysi/Dropbox/Birth_Mexico/Data/sinac2013DatosAbiertos.csv")
table(sinac2013DatosAbiertos$fecha_nacimiento_nac_vivo)
sinac2013DatosAbiertos$Variable="Birthdates"
sinac2013DatosAbiertos=sinac2013DatosAbiertos[,c("Variable","fecha_nacimiento_nac_vivo")]
names(sinac2013DatosAbiertos)=c("Variable","Value")


sinac2014DatosAbiertos <- read.csv("C:/Users/kzysi/Dropbox/Birth_Mexico/Data/sinac2014DatosAbiertos.csv")
table(sinac2014DatosAbiertos$fecha_nacimiento_nac_vivo)
sinac2014DatosAbiertos$Variable="Birthdates"
sinac2014DatosAbiertos=sinac2014DatosAbiertos[,c("Variable","fecha_nacimiento_nac_vivo")]
names(sinac2014DatosAbiertos)=c("Variable","Value")


births=rbind(sinac2012DatosAbiertos,sinac2013DatosAbiertos,sinac2014DatosAbiertos)

births <- births %>%
  mutate(
    Value =  yday(as.Date(Value, format="%d/%m/%Y")))


## merge all datasets together
Distributions=rbind(births,cd,Female_weights,listings)

library(tidyverse)
#make a histogram

ggplot(Distributions, aes(x = Value, fill = Variable)) +
  geom_histogram(color = "white") + # Adjust binwidth as needed
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),  # Remove facet background
    strip.text = element_blank(),        # Remove facet labels
    axis.text = element_text(size = 10)  # Adjust axis text size if needed
  ) +
  scale_x_continuous(labels = scales::label_number()) + # Force non-scientific x-axis
  scale_y_continuous(labels = scales::label_number()) + # Force non-scientific y-axis
  facet_wrap(~ Variable, scales = "free", ncol=2)


ggsave("Distributions_q_data.png", width = 12, height = 8, units = "in", dpi = 300)
        