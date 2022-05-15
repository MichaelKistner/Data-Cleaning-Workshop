# Load libraries
library(tidyverse)
library(fst)

# Load data
contribs <- read_fst("Data/Individual Contributions (Merged).fst")
load("Data/House Tweets.Rda")

# Create summarized version of each dataset where each row is an individual
# representative to get total $ raised and total # tweets for each rep
contribs_summarized <- group_by(contribs,
                               name, party_affil, office_state, district) %>%
  summarize(total_raised = sum(transaction_amount))

tweets_summarized <- group_by(house_tweets,
                              MemberName, MemberState, MemberDistrict) %>%
  summarize(NumberTweets = n()) 

# Merge datasets together using state and district
tweets_summarized <- tweets_summarized %>%
  setNames(c("name", "office_state", "district", "n_tweets")) %>%
  mutate(district = str_pad(district, 2, side = "left", pad = "0"))

combined_df <- left_join(contribs_summarized,
                         tweets_summarized,
                         by = c("office_state", "district")) %>%
  filter(!(name.y %in% filter_vec))

# Compare number of tweets with amount of money raised
ggplot(combined_df, aes(x = log(n_tweets), 
                        y = log(total_raised))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# Create version of dataset where each row represents a member-day combination
tweets_by_day <- house_tweets %>%
  filter(!(MemberName %in% filter_vec)) %>%
  group_by(Date, MemberName, MemberState, MemberDistrict) %>%
  summarize(NumberTweets = n())
  
tweets_by_day <- expand_grid(Date = unique(tweets_by_day$Date),
                    MemberName = unique(tweets_by_day$MemberName)) %>%
  left_join(tweets_by_day) %>%
  setNames(c("date", "name", "office_state", "district", "n_tweets"))  %>%
  mutate(district = str_pad(district, 2, side = "left", pad = "0"))


