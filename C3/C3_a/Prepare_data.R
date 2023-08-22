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

