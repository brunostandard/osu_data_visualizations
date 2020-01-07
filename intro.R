library(tidyverse)
theme_set(theme_bw())
user <- read_csv("../osu_data_mining/user.csv")
user2 <- read_csv("../osu_data_mining/user2.csv")
user3 <- read_csv("../osu_data_mining/user3.csv")
seconds_to_years <- 1/(60*60*24*365.25)
users <- rbind(rbind(user,user2),user3) %>%
  filter(pp_rank < 150000) %>% # filter out the low ranks
  filter(pp_raw != 0) %>% # filter out the low ranks
  arrange(pp_raw) # sort by pp_raw
  

ggplot(data = users, map = aes(pp_rank, pp_raw)) +
  geom_point() + 
  ggtitle("Distribution of pp_raw in pp_rank")
# it's interesting. The difference is crazy. 

#ggplot(data = users, map = aes(pp_rank, playcount)) +
#  geom_point(alpha = 0.01) + 
#  ggtitle("Rank vs Playcount")

ggplot(data = users, map = aes(pp_rank, playcount, group = cut_width(pp_rank,3500))) +
  geom_boxplot() + 
  ggtitle("Rank vs Playcount")

# Playcount will therefore be important. Not sure if it's the greatest factor in a person's pp_rank (and hence pp_raw)

ggplot(data = users, map = aes(playcount, pp_raw)) +
  geom_point(alpha = 0.01) + 
  ggtitle("playcount vs pp_raw")

ggplot(data = users, map = aes(playcount, pp_raw)) +
  geom_bin2d(bins = 50) + 
  ggtitle("playcount vs pp_raw")

# both of these plots are bit skewed. Recall that the distribution of pp_raw among the ranks is skewed.
# there's a decent way to make pp_raw a function of pp_rank. We could also just take the log of pp_raw
# when we are making these plots. 
ggplot(data = users, map = aes(x = pp_rank)) +
  geom_point(aes(y = pp_raw), color = "red") + 
  geom_point(aes(y = log(pp_raw)), color = "blue")
# we'll just have to remember to take the exponential down the road when we make models. 

# now doing the heatmap again with log(pp_raw) as our y data series. 
ggplot(data = users, map = aes(x = playcount, y = log(pp_raw))) +
  geom_bin2d(bins = 50) + 
  ggtitle("playcount vs log(pp_raw)")
# We no longer have the localized hot spot. 

# veterans vs new playres. I think new players have the advantage. They are more adept to maps that offer more pp. 
# Veterans had maps that offered little reward yet difficult
ggplot(data = users, map = aes(x = pp_rank, y = join_date, group = cut_width(pp_rank,3500))) +
  geom_boxplot()
# clearly, older players are more likely to be at the upper ranks by a small margin. I don't see a strong enough
# relationship for me to consider a player's join_date as an indicator of player strength. There's also the factor
# of people having multiple accounts. The more common problem is player's become absent for long periods of time. 
# For example, my own account has been untouched for almost two years. 

# If players become absent, why not consider the amount of time they actually played. 
ggplot(data = users, map = aes(x = pp_rank, y = total_seconds_played/60, group = cut_width(pp_rank,3500))) +
  geom_boxplot() +
  ggtitle("pp_rank vs hours played.")
# A decent relationship I'd say.

ggplot(data = users, map = aes(x = log(pp_raw), y = total_seconds_played/60)) +
  geom_bin2d(bins = 50) +
  ggtitle("log(pp_raw) vs hours played.")

# just by cross comparing these two graphs, we can safely assume that there is some linear relationship between 
# play time and a player's strength. 


# Now what about the number of completed maps. Completing a map is central to gaining more performance points (pp). 
# Not everyone however is farming maps for the sake of performance. 
users <- users %>% 
  mutate(total_ranked_maps = count_rank_ss + count_rank_ssh + count_rank_s + count_rank_sh + count_rank_a, 
    ss_count = count_rank_ssh + count_rank_ss,
    s_count = count_rank_s + count_rank_sh)

users %>% 
  filter(total_ranked_maps < 20000) %>% # filtering out crazy numbers to make plots easier to look at
  ggplot(map = aes(pp_raw, total_ranked_maps, group = cut_width(pp_raw,500))) +
  geom_boxplot()
# Another weak correlation. 
# I would say that the total number of maps completed is a pointless number to consider since it doesn't account
# for the number of attempts a player has made. A player could easily make 100+ attempts to complete a map. Even
# when completed, that map may not even offer any fruit (pp) for their labor. 

# To get an idea of all the attempts a player makes, we can compare the difference from ranked_score and total_score. 
# ranked_score considers the points from all ranked (completed) plays. Total score is all the points a player accumulates
# just by playing. 
ggplot(data = users, map = aes(x = pp_rank, y = log(total_score - ranked_score), group = cut_width(pp_rank,3500))) +
  geom_boxplot()

ggplot(data = users, map = aes(x = log(pp_raw), log(total_score - ranked_score))) +
  geom_bin2d(bins = 50) +
  ggtitle("log(pp_raw) vs log(total_score - ranked_score)")

# As for count300, count 100, and count 50, I don't think it's important. They are pretty much encompassed by ranked
# and total score. 
# Accuracy is another poor way to look at a player's strength. Top players do have high accuracy, though. 
users %>% filter(accuracy >= 85) %>%
  ggplot(map = aes(x = pp_rank, y = accuracy, group = cut_width(pp_rank,4000))) +
  geom_boxplot()
# There is just so much variance to consider accuracy.


# I'd like to see though consistency. I believe a player's growth is dependent on how they play relative to the amount
# they've started playing. That is where join_date failed. An account could be made in 2008 but they haven't played
# consitently. Maybe this account had decent activity during 2015 but stopped playing consitently the year after. 

# (Opinion) I believe performance points inflates over time. The top ranked player base are reaching the >13k range 
# easily. I recall a time where >8K was only achievable by one player. I don't doubt that players have gotten better.
# In fact, the distribution of performance points is similar to what we see now. It's a smoother distribution. 
# The players have changed. Former top-players are still considered high-tier by most. We are just seeing more people
# being competitive with the game. 

# Let F() be a function of (total_score - ranked_score). 
# Consistency can be thought of the amount of time played divided by the amount time an account has been alive
# e.g. total_seconds_played / (join_date-1970)
date_of_recording <- as.POSIXct("2019-11-01 14:00:00")

users <- users %>%
  mutate(total_seconds_alive = as.numeric(date_of_recording) - as.numeric(join_date), 
         consistency = total_seconds_played / total_seconds_alive )
         
ggplot(data = users, map = aes(x = pp_rank, y = consistency, group = cut_width(pp_rank,3500))) +
  geom_boxplot()

# Very consistent players must be playing a lot of maps (i.e. a high playcount)
ggplot(users, aes(playcount, consistency, color = pp_rank)) + 
  geom_point(alpha = 0.01)

ggplot(data = users, map = aes(x = pp_raw, y = consistency)) +
  geom_bin2d(bins = 50)

# It isn't what I expected, but there's many ways interpret this.
# 1. Not all players are playing for the sake of practicing. Players can pretty much go on auto-pilot when playing
# 2. Realistically, no player is going to have a consistency of 50%. It would imply that they literally spend half
#   their time playing osu!. 
# 3. I didn't account of sleep wich accounts fo say 31.25% of one's life. 
# 4. People have school, work, and other obligations to attend to. They may play Osu! often, but that time may
#    not be enough to get meaningul value. A practice session must take at least 1 hour (6% of their waking day).
#    Besides, one plays to have fun. 
# 5. Older players are less likely to play consistently (for many reasons). As shown below 

year_in_seconds <- 60*60*24*365.25 
temp_lm <- lm(consistency ~ join_date, users)
temp_lm$coefficients
users %>% 
  mutate( model = as.numeric(join_date) * temp_lm$coefficients['join_date'] + temp_lm$coefficients['(Intercept)'] ) %>%
  ggplot(map = aes(x = join_date)) +
  geom_point(map = aes(y = consistency), alpha = 0.1) + 
  geom_point(map = aes(y = model), size = 2, alpha = 0.1, color = "red")

# Is there a way to account for this? In particular, I would say a 5 year account with a consistency of 5% to be 
# "more impressive" than a 2 year account with a consistency of 9%. IMO, new players have an easier time indulging 
# Osu!. I can use a tan() function to offset the consistency values relative to a player's join_date. 

# first look at the distribution of consistency
users %>% filter(consistency >= 0.0178571428571429) %>% # filtering out very inactive players
  ggplot(map = aes(x = consistency)) + 
  geom_histogram(bins = 100) + 
  geom_vline(xintercept = mean(users$consistency), color = "red", size = 2, alpha = 0.5)
# and join_date

# Let's break players into the year quarter they were "born" in. Since the oldest players date the 1st quarter of
# of 2008, and the newest players date the 4th quarter of 2019, we have a total span of 48 quarters.
# For convenience, let's say the quarter = 1 is "1st quarter of 2008" and the quarter = 48 is 
# "4th quarter of 2019". Then,
quarter_in_seconds <-.25*365.25*24*60*60
users <- users %>%
  mutate(quarter = as.integer(48 - total_seconds_alive / quarter_in_seconds))


# Now I want to see the consistency of old vs new players based on their quarterly births. 
# as we saw before, those born closer to quarter 48 (i.e. around the 4th quarter of 2019) have more
# consistent play time
users %>%
  group_by(quarter) %>%
  summarise(count = length(consistency), 
            mean = mean(consistency),
            cymin = min(consistency),
            cymax = max(consistency),
            var = var(consistency), 
            wt = count/4208) %>% # where 4208 is the max number of people in one quarter. from quarter 32.  
    ggplot(map = aes(x = quarter, y = mean*wt)) + 
  geom_point() + 
  geom_crossbar(map = aes(ymin = cymin*wt, ymax = cymax*wt))

# I think what we have is good enough. It's altruistic to suggest older inactive players are more consistent
# than young and very active players. 



# Let's look at correlations. 
# we're using: consistency, total_seconds_played, playcount, pp_raw, total_ranked_maps
# first check their distributions
# consistency ~ a skewed normal
# total_seconds_played ~ a skewed normal
# playcount ~ a skewed normal
# pp_raw ~ vaguely a skewed normal
# total_ranked_maps ~ pretty normal
# may need to filter outliers, but let's do that later. 
users %>%
  filter(total_ranked_maps <= 10000) %>%
  ggplot(aes(total_ranked_maps)) +
  geom_histogram(bins = 100)

data1 <- users %>%
  select(pp_raw, pp_rank, consistency, total_seconds_played, playcount, total_ranked_maps)

M <- cor(data1)
M
model_v1 <- lm(pp_raw ~ consistency + total_seconds_played + playcount + total_ranked_maps, data1)
coeff <- model_v1$coefficients
data1 %>%
  mutate(pp_raw_model = consistency*coeff["consistency"] + total_seconds_played*coeff["total_seconds_played"] +
           playcount*coeff["playcount"] + total_ranked_maps*coeff["total_ranked_maps"] + coeff["(Intercept)"]) %>%
  ggplot(map = aes(x = pp_rank)) +
  geom_point(map = aes(y = pp_raw), color = "blue") +
  geom_point(map = aes(y = pp_raw_model), color = "red", alpha = 0.01)
######################## model 2 ####

data2 <- data1 %>%
  filter(total_ranked_maps <= 5000,
         pp_raw <= 10000,
         playcount <= 1.25e+05, 
         total_seconds_played <= .5e+07, # being harsh here for exp model
         consistency <= .065
  )

count(data2)/count(data1)

ggplot(data2,aes(total_seconds_played)) + 
  geom_histogram(bins = 100)

M2 <- cor(data2)
M2
model_v2 <- lm(pp_raw ~ consistency + total_seconds_played + playcount + total_ranked_maps, data2)
coeff2 <- model_v2$coefficients
users %>%
  mutate(pp_raw_model = consistency*coeff2["consistency"] + total_seconds_played*coeff2["total_seconds_played"] +
           playcount*coeff2["playcount"] + total_ranked_maps*coeff2["total_ranked_maps"] + coeff2["(Intercept)"]) %>%
  ggplot(map = aes(x = pp_rank)) +
  geom_point(map = aes(y = pp_raw), color = "blue") +
  geom_point(map = aes(y = pp_raw_model), color = "red", alpha = 0.01)
############# model 3 (FAILED) ####
model_v3 <- lm(log(pp_raw) ~ consistency + total_seconds_played + playcount + total_ranked_maps, data2)
coeff3 <- model_v3$coefficients
temp <- users %>%
  mutate(pp_raw_model = exp(consistency*coeff3["consistency"] + total_seconds_played*coeff3["total_seconds_played"] +
           playcount*coeff3["playcount"] + total_ranked_maps*coeff3["total_ranked_maps"] + coeff3["(Intercept)"])) %>%
  arrange(desc(pp_raw_model)) %>%
  select(pp_raw_model, consistency, total_seconds_played, playcount, total_ranked_maps)
view(temp)
############## comparing models ####
users %>%
  mutate(pp_raw_model_2 = consistency*coeff2["consistency"] + total_seconds_played*coeff2["total_seconds_played"] +
           playcount*coeff2["playcount"] + total_ranked_maps*coeff2["total_ranked_maps"] + coeff2["(Intercept)"],
         pp_raw_model_1 = consistency*coeff["consistency"] + total_seconds_played*coeff["total_seconds_played"] +
           playcount*coeff["playcount"] + total_ranked_maps*coeff["total_ranked_maps"] + coeff["(Intercept)"]) %>%
  select(pp_raw, pp_raw_model_1, pp_raw_model_2) %>%
  mutate(ls_1 = abs(pp_raw - pp_raw_model_1),     # ls is for "least - squares". It's the error. 
         ls_2 = abs(pp_raw - pp_raw_model_2)) %>% # ls is for "least - squares". It's the error. 
  summarise(sum_1 = sum(ls_1), sum2 = sum(ls_2),
            avg_1 = mean(ls_1), avg_2 = mean(ls_2),
            sd_1 = sd(ls_1), sd_2 = sd(ls_2))

users %>% arrange(desc(consistency)) %>%
  select(username, join_date, consistency) %>%
  arrange(join_date) %>%
  head()
#