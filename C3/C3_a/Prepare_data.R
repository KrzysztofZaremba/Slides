library(tidyverse)

listings <- read.csv("C:/Users/kzysi/Dropbox/Itam_teaching/Markdowns/C3/C3_a/listings.csv", comment.char="#")

listings=listings%>%
  select("id","review_scores_cleanliness","price")%>%
  filter(review_scores_cleanliness!="")%>%
  mutate(price = as.numeric(gsub("\\$", "", gsub(",", "", price))))


listings$review_scores_cleanliness=as.numeric(listings$review_scores_cleanliness)
listings$clean=listings$review_scores_cleanliness>4.5


ggplot(listings[listings$price<10000,], aes(x = price)) +
  geom_histogram() +
  labs(x = "Clean Variable", y = "Price") +
  ggtitle("Price by Clean Variable")+
  facet_wrap(~clean)


sample_clean_1 <- listings %>%
  filter(clean == 1) %>%
  sample_n(100, replace = FALSE)

# Take a sample of 100 observations where clean = 0
sample_clean_0 <- listings %>%
  filter(clean == 0) %>%
  sample_n(100, replace = FALSE)


Sample_list=rbind(sample_clean_0,sample_clean_1)

ggplot(Sample_list, aes(x = price)) +
  geom_histogram() +
  labs(x = "Clean Variable", y = "Price") +
  ggtitle("Price by Clean Variable")+
  facet_wrap(~clean)



t.test(sample_clean_1$price, sample_clean_0$price)

save(Sample_list,file="sample_listing.Rda")

###create a loop. Take 5000 samples of 100 units of clean apartments. Calculate the mean 

cl_list=listings[listings$clean==TRUE & listings$price<10000,]
true_average=mean(cl_list$price, na.rm=TRUE)

##in a loop take 5000 samples of 100 units
sample_means=rep(NA,10000)
for (i in 1:10000){
  sample_clean_1 <- cl_list %>%
    sample_n(100, replace = FALSE)
  sample_means[i]=mean(sample_clean_1$price, na.rm=TRUE)
}

##Show true histogram of prices

estimated_mean <- 1245
conf_interval_lower <- 1086.46
conf_interval_upper <- 1404.22

x_min <- min(min(cl_list$price), conf_interval_lower)
x_max <- max(max(cl_list$price), conf_interval_upper)

# First histogram: True Histogram of Prices
p1 <- ggplot(cl_list, aes(x = price)) +
  geom_histogram(fill = "lightblue", bins = 50) +
  labs(x = "Price", y = "Frequency") +
  ggtitle("Population Distribution") +
  geom_vline(xintercept = true_average, color = "red", linetype = "dashed", 
             size = 1, show.legend = TRUE) +
  annotate("text", x = true_average, y = 1000, label = paste("True Avg:", round(true_average)), 
           vjust = -0.8, hjust = 1, color = "red") +
  xlim(0, 5000)+theme_minimal()

# Second histogram: Sample Means with estimated mean and confidence interval
p2 <- ggplot() +
  geom_rect(aes(xmin = conf_interval_lower, xmax = conf_interval_upper, ymin = -Inf, ymax = Inf), 
            fill = "orange", alpha = 0.5) + # Increased transparency
  geom_histogram(data=data.frame(sample_means), aes(x = sample_means), fill = "lightblue", bins = 300) +
  labs(x = "Price", y = "Frequency") +
  ggtitle("Sampling Distribution of the Mean and Our Estimates") +
  geom_vline(xintercept = estimated_mean, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = estimated_mean, y = 600, label = paste("Est. Mean:", estimated_mean), 
           vjust = -0.5, hjust = 1, color = "black") +
  annotate("text", x = 3000, y = 600, 
           label = paste("Conf Interval: [", conf_interval_lower, ",", conf_interval_upper, "]"), 
           vjust = -1, color = "orange") +
  xlim(0, 5000)+
  annotate("text", x = mean(c(conf_interval_lower, conf_interval_upper)), y = -Inf, 
           label = "Green band represents 95% Confidence Interval", vjust = 2, color = "green")+theme_minimal()

# Combine the plots
grid.arrange(p1, p2, ncol = 1)

##save the plot
ggsave("C:/Users/kzysi/Dropbox/Itam_teaching/Markdowns/C3/C3_b/airbnb_true.png", width = 10, height = 10, dpi = 300, bg="white")
