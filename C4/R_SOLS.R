table(Accidents$delegacion_cierre)

M_accidents=Accidents[Accidents$delegacion_cierre=="MIGUEL HIDALGO",]

install.packages("tidyverse")
library(tidyverse)
ggplot(data=M_accidents, aes(x=NOx, y=N_acc))+
  geom_point()

###fit a regression
model1=lm(N_acc~NOx, data=M_accidents)
summary(model1)

b1=cov(M_accidents$N_acc, M_accidents$NOx)/var(M_accidents$NOx)
M_accidents$yhat=predict(model1)

ggplot(data=M_accidents, aes(x=NOx, y=yhat))+
  geom_point()

ggplot(data=M_accidents, aes(x=NOx, y=yhat))+
  geom_point()+
  geom_point(aes(x=NOx, y=N_acc), color="grey")

count(M_accidents)

M_accidents$date[which.max(M_accidents$N_acc)]

####
M_accidents$residuals=M_accidents$N_acc-M_accidents$yhat

ggplot(data=M_accidents, aes(x=yhat, y=residuals))+
  geom_point()

ggplot(data=M_accidents)+ ###what data I use
  geom_qq(aes(sample=scale(residuals)))+ #I want to use qq plot, my sample is normalized residuals
  geom_abline(color="red")+  ## 45 degrees line
  coord_fixed() ### same scales on y and x axis

b1=381.923
SEb1=83.895

Tstat=(b1-300)/SEb1

pt(q=Tstat, df=330, lower.tail=FALSE)

confint(model1, level=0.95) ### confidence intervals


####manual way 
b1-SEb1*qt(0.975, 330)
b1+SEb1*qt(0.975, 330)

## Predict the response, if the pollution increases by 0.1
b0=35.071

y_pred=b0+b1*0.1

new_data=data.frame(NOx=0.1)


##prediction
predict(model1, newdata =new_data, interval="confidence" ,level=0.95)
predict(model1, newdata =new_data, interval="prediction" ,level=0.95)


###
anova(model1)
