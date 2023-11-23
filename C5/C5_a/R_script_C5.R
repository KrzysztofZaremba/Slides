#### Loading the data 
### If you want to replicate this, you can download the data here: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html

Nacimientos_2021 <- read.csv("C:/Users/kzysi/Dropbox/Birth_Mexico/Data/Nacimientos_2021.csv")
Nacimientos_2021 = Nacimientos_2021[,c("PESO","EDADGESTACIONAL","EDAD","NUMEROEMBARAZOS","HIJOSNACIDOSMUERTOS",
                                       "TOTALCONSULTAS")]
Nacimientos_2021 <- Nacimientos_2021[complete.cases(Nacimientos_2021), ]
Nacimientos_2021=Nacimientos_2021[sample(1:nrow(Nacimientos_2021),50000),]
Nacimientos_2021=Nacimientos_2021%>%
  filter(PESO!=9999)%>%
  filter(EDADGESTACIONAL!=99)%>%
  filter(EDAD!=999)%>%
  filter(TOTALCONSULTAS!=99)%>%
  filter(NUMEROEMBARAZOS!=99)%>%
  filter(HIJOSNACIDOSMUERTOS!=99)

#### Suppose we want to predict baby's weight

#### Running regression with different set of predictors and comparing the models (R^2/adjusted R^2)
model1=lm(PESO~EDAD+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS, data=Nacimientos_2021)
summary(model1)

model2=lm(PESO~EDADGESTACIONAL+EDAD+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS, data=Nacimientos_2021)
summary(model2)

model2a=lm(PESO~EDADGESTACIONAL+EDAD+I(EDAD^2)+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS, data=Nacimientos_2021)
summary(model2a)

model3=lm(PESO~EDADGESTACIONAL+EDAD+I(EDAD^2)+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS+TOTALCONSULTAS, data=Nacimientos_2021)
summary(model3)


#### Interpretations
#1 Intercept
#2 One more week of gestational age
#3 Age, non linear relationship
#4 number of pregnancy
#5 death sons. Why negative?
#6 one more visit. Why negative?

#### Standardizing variables
model4=lm(scale(PESO)~scale(EDADGESTACIONAL)+scale(EDAD)+scale(I(EDAD^2))+scale(NUMEROEMBARAZOS)+scale(HIJOSNACIDOSMUERTOS)+scale(TOTALCONSULTAS), data=Nacimientos_2021)
summary(model4)

model4=lm(PESO~scale(EDADGESTACIONAL)+scale(EDAD)+scale(I(EDAD^2))+scale(NUMEROEMBARAZOS)+scale(HIJOSNACIDOSMUERTOS)+scale(TOTALCONSULTAS), data=Nacimientos_2021)
summary(model4)
##what has the largest effect in terms of standard deviations? 1. gestational age, 2. numer of doctors visits

#### Diagnostics

### Plot residuals against predictions (it does not ma)
##if you use standarized model, than the predictions will be in terms of standard deviations and averages...
Nacimientos_2021$yhat=model4$fitted.values

## Better to use non-standarized model
Nacimientos_2021$yhat=model3$fitted.values
Nacimientos_2021$residuals=model3$residuals

library(tidyverse)
ggplot(data=Nacimientos_2021)+
  geom_point(aes(x=yhat, y=residuals), alpha=0.7)

####
model3=lm(PESO~EDADGESTACIONAL+EDAD+I(EDAD^2)+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS+TOTALCONSULTAS+I(NUMEROEMBARAZOS^2), data=Nacimientos_2021)
summary(model3)

Nacimientos_2021$yhat=model3$fitted.values
Nacimientos_2021$residuals=model3$residuals

library(tidyverse)
ggplot(data=Nacimientos_2021)+
  geom_point(aes(x=yhat, y=residuals), alpha=0.7)

###We have some non-linearity
model5=lm(PESO~EDADGESTACIONAL+I(EDADGESTACIONAL^2)+EDAD+I(EDAD^2)+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS+TOTALCONSULTAS, data=Nacimientos_2021)
summary(model5)


Nacimientos_2021$yhat=model5$fitted.values
Nacimientos_2021$residuals=model5$residuals


ggplot(data=Nacimientos_2021)+
  geom_point(aes(x=yhat, y=residuals), alpha=0.7)


###add one more?

model5=lm(PESO~EDADGESTACIONAL+I(EDADGESTACIONAL^2)+I(EDADGESTACIONAL^3)+EDAD+I(EDAD^2)+NUMEROEMBARAZOS+HIJOSNACIDOSMUERTOS+TOTALCONSULTAS, data=Nacimientos_2021)
summary(model5)

Nacimientos_2021$yhat=model5$fitted.values
Nacimientos_2021$residuals=model5$residuals

###what is the standard error of variable yhat?


ggplot(data=Nacimientos_2021)+
  geom_point(aes(x=yhat, y=residuals), alpha=0.7)


#### 
#What else do you notice?
#Variance is not constant. Is that a problem?

### Do qq plot
ggplot(Nacimientos_2021) +
  geom_qq(aes(sample = scale(residuals))) +
  geom_abline(color = "red") +
  coord_fixed() +
  theme_minimal()

#### Looking at predictions of the model 


