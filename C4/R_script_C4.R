M_accidents=Accidents[Accidents$delegacion_cierre=="MIGUEL HIDALGO",]

install.packages("tidyverse")
library(tidyverse)




## Create a histogram of accidents
hist(M_accidents$N_acc)

## create a histogram of temperatures
hist(M_accidents$NOx)

## plot number of accidents in that neighborhood by date.
ggplot(data=M_accidents, aes(x=date, y=N_acc))+
  geom_line()

ggplot(data=M_accidents, aes(x=date, y=NOx))+
  geom_line()

##Find the day with the highest number of accidents
M_accidents$date[which.max(M_accidents$N_acc)]

## Create a scatterplot of number of accideents vs NOx
ggplot(data=M_accidents, aes(x=NOx, y=N_acc))+
  geom_point()


## Find beta 0 and beta 1
model1=lm(N_acc~NOx, 
          data=M_accidents)
summary(model1)

##interpret them

##manual b1
cov(M_accidents$N_acc, M_accidents$NOx)/var(M_accidents$NOx)

##find yhat
M_accidents$yhat=predict(model1)

##plot yhat
ggplot(data=M_accidents, aes(x=NOx, y=yhat))+
  geom_point()

####WHY IT NEVER INCREASES BY 381?

###with actual y_i
ggplot(data=M_accidents, aes(x=NOx, y=yhat))+
  geom_point()+
  geom_point(aes(x=NOx, y=N_acc), color="grey")


## Find residuals, check they are the same as y-yhat
M_accidents$residuals=M_accidents$N_acc-M_accidents$yhat
model1$residuals==M_accidents$residuals

## Evaluate residuals

##let's plot them against yhat
ggplot(data=M_accidents, aes(x=yhat, y=residuals))+
  geom_point()

##let's normalize them and do a qqplot
ggplot(M_accidents) +
  geom_qq(aes(sample = scale(residuals))) +
  geom_abline(color = "red") +
  coord_fixed() 


# Find mean squared error - sigma

sigmasq=sum(model1$residuals^2)/(model1$df.residual)
sqrt(sigmasq)

## test for beta 1 being larger than 300
b1=as.numeric(coef(model1)["NOx"])
SEb1=sqrt(diag(vcov(model1)))[2]
Ttest=(b1-300)/SEb1
dfdm=model1$df.residual

##find p-value
pt(q=Ttest, df=dfdm, lower.tail = FALSE)


## Find confidence interval of betas
confint(model1, level = 0.95)

##which is same as:
b1-SEb1*qt(0.975, dfdm)
b1+SEb1*qt(0.975, dfdm)

qt(0.995, 29)
## Predict the response if no rises to 0.1
b0=as.numeric(coef(model1)["(Intercept)"])
b0+b1*0.1

## Find mean response and new observation conf intervals
# New value of x for which you want to predict
x_new <- 0.01

# Create a data frame with the new value of x
new_data <- data.frame(NOx = x_new)


predict(model1, newdata = new_data, interval = "confidence", level=0.95)
predict(model1, newdata = new_data, interval = "prediction", level=0.95)
?predict

### what is the probability that a number of accidents on a new day with NOx=0.05 number of is lower than 100?
### Wow we can make 

e1=1
e2=1/(dfdm+2)
e3=(0.1-mean(M_accidents$NOx))^2/(var(M_accidents$NOx)*(dfdm+1))

se_pred=sqrt(sigmasq*(e1+e2+e3))
pred=b0+b1*0.05

1 - pnorm(50, mean = pred, sd = se_pred)

## Make anova table
anova(model1)