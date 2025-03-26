######


#Practice1: Create a vector with values from 2 to 6. Square it, then add 200, then divide by -5. tell me the 3rd element of the new vector
x=c(2:6)
x=((x^2)+200)/(-5)
x[3]

#Practice 2: Generate 1000 values from normal distribution with mean 3 and sd 2
# Then generate 1000 values from normal distribution with mean 20 and sd 6
# Add the two vectors together.
# What should be the mean of the new distribution?
# what should be the standrd deviation ?

x=rnorm(1000,3,2)
y=rnorm(1000,20,6)
z=x+y
mean(z)
sd(z)

##practice with data AIRBNB

##load data into R
listings=airbnb
##tell them about what do we have in the data

##number of observations
nrow(listings)

##number of separate hosts?
listings$host_id
##

length(unique(listings$host_id))

###What do we have in the data?


##how many listings we have in each neighborhood
table(listings$neighbourhood)
table(listings$neighbourhood_cleansed)

##order by most frequent
sort(table(listings$neighbourhood_cleansed),decreasing=TRUE)

##practice - what is the most common name of the host?
sort(table(listings$host_name),decreasing=TRUE)

##on average, how many bedrooms in an airbnb?
mean(listings$bedrooms)

##NA - no obserations. Top calculate we need to remove NA
mean(listings$bedrooms,na.rm=TRUE)

##what is max?
max(listings$bedrooms,na.rm=TRUE) #let's find it

##what types of rooms are most common?
table(listings$room_type)

##what's the average score?
mean(listings$review_scores_rating,na.rm=TRUE)

##what's the standard deviation
sd(listings$review_scores_rating,na.rm=TRUE)


##pick neighborhood
sort(table(listings$neighbourhood_cleansed),decreasing=TRUE)

##filter data to keep only airbnbs from this neighborhood
new_data=listings[listings$neighbourhood_cleansed=="Álvaro Obregón",]


##summary info about price
#mean, median, standard deviation
mean(new_data$price)
median(new_data$price)
sd(new_data$price)

###plot distribution 
hist(new_data$price)

#is it right skewed left skewed etc?

##average location scoree
mean(new_data$review_scores_location,na.rm=TRUE)

### scatter plot between prices and review scores
plot(new_data$price,new_data$review_scores_location)

##calculate correlation
cor(new_data$price,new_data$review_scores_location, , use = "complete.obs")

## calculate average price depending on the number of bedrooms and put it on a barplot
library(tidyverse)
new_data %>% 
  group_by(bedrooms) %>% 
  summarise(mean_price=mean(price,na.rm=TRUE))

##put it on a barplot
new_data %>% 
  group_by(bedrooms) %>% 
  summarise(mean_price=mean(price,na.rm=TRUE)) %>% 
  ggplot(aes(x=bedrooms,y=mean_price))+
  geom_bar(stat="identity")

##create a variable, location better than median, location worse then median
new_data$location_better_than_median=new_data$review_scores_location>median(new_data$review_scores_location,na.rm=TRUE)

##calculate mean price for those two
new_data %>% 
  group_by(location_better_than_median) %>% 
  summarise(mean_price=mean(price,na.rm=TRUE))

##visualize it with confidence intervals:
x=new_data %>% 
  group_by(location_better_than_median) %>% 
  summarise(mean_price=mean(price,na.rm=TRUE),sd_price=sd(price,na.rm=TRUE),n=n())

  ggplot(x,aes(x=location_better_than_median,y=mean_price))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_price-sd_price/sqrt(n),ymax=mean_price+sd_price/sqrt(n)),width=0.2)


## we have population data here! choose randomly 100 observations,
sample=new_data[sample(1:nrow(new_data),100),]

#reminder: 
mean(new_data$price)
sd(new_data$price)

m=mean(sample$price,na.rm=TRUE)
sd=sd(sample$price,na.rm=TRUE)
conf_level=0.95
n=nrow(sample)
alpha_by_two=(1-conf_level)/2
#critical value
z=qnorm(alpha_by_two)
z=abs(z)

#what if I use student t with 99 degrees of freedom
t=abs(qt(alpha_by_two,n-1))

##calculate confidence interval for the mean price

Upper=m+z*sd/sqrt(n)
Lower=m-z*sd/sqrt(n)

##calculate confidence interval for the mean price
t.test(sample$price, conf.level = 0.95, mu=0) ##this one uses student t!


#find top 5 most affordable places with rating 4.5 or higher and at least 100 reviews 

#what determines the price?  divide into high and low prices and try to vizualize differences

#availability last 30 days
