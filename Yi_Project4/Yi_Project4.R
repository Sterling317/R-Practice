#Junchen Yi  Date: 16/10/2023 Class:ALY6000 70407
# install.packages("janitor")
# install.packages("dplyr")
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(janitor)

#Exploring
players <- read.csv("all_players.csv")

# Filtered data for all male players
players <- players %>%
  filter(players$Gender == 'M')

#clean names
players <- players %>%
  clean_names()

#Removing some columns
players <- players[, -(14:39)]
players <- players %>%
  select(-url, -gk, -x)

#Count the number of people in each position
count_position <- table(players$position)
count_pos_df <- data.frame(Name = names(count_position), Count = as.vector(count_position))
ggplot(count_pos_df, aes(x = Name, y = Count)) +
  geom_bar(aes(x = Name), stat = "identity", fill = "blue", width = 0.75) +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3)+
  xlab("Player Position") +
  ylab("Total number of each position on the list")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Preferred foot: left foot vs. righy foot
foot_prefer <- table(players$preferred_foot)
foot_prefer_df <- data.frame(Foot = names(foot_prefer), F_Count = as.vector(foot_prefer))
ggplot(foot_prefer_df, aes(x = Foot, y = F_Count)) +
  geom_bar(aes(x = Foot), stat = "identity", fill = "blue", width = 0.75) +
  geom_text(aes(label = F_Count), vjust = -0.5, color = "black", size = 3)+
  xlab("Player preferred foot") +
  ylab("Number of players of each type") 

#Expanding
#how many players do not have weak foot (answer = 19)
colnames(players)
players$weak_foot
no_week_foot <- players[players$weak_foot == 5, ]
#Select those players overall value > 80
no_week_foot <- no_week_foot[no_week_foot$overall > 80, ]
row_count <- nrow(no_week_foot)
row_count

#Visualization
# top 10 players with the highest ability values
top_ten_players <- players %>%
  arrange(desc(overall)) %>%
  slice(1:10)
typeof(top_ten_players$overall)
ggplot(top_ten_players, aes(x = name, y = overall, color = name)) +
  scale_y_continuous(limits = c(0, 95), breaks = seq(0, 95, by = 10))+
  geom_bar(stat = "identity", fill = "blue", width = 0.75) +
  geom_text(aes(label = overall), vjust = -0.5, color = "black", size = 4)+
  xlab("Player Names") +
  ylab("Overall stat")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relationship between a player's age and ability value
#Scatter plot
player_distribution <- ggplot(players, aes(x = age, y = overall, color = overall)) +
  geom_point() +
  labs(title = "Age vs. Overall Value")
player_distribution  

#What percentage of the top 50 overall players ratings are for each club?
top_50_players <- players %>%
  arrange(desc(overall)) %>%
  slice(1:50)

top_50_category <- top_50_players %>%
  count(club)

sum_of_other <- sum(top_50_category$n[top_50_category$n == 1])
top_50_category <- top_50_category[top_50_category$n != 1, ]
new_row <- data.frame(
  club = "Others",
  n = sum_of_other
)
top_50_category <- rbind(top_50_category, new_row)
top_50_category <- top_50_category

# Pie chart
pie(top_50_category$n, labels = paste(top_50_category$club, " (", round(top_50_category$n/sum(top_50_category$n)*100, 1), "%)", ")"),
    main = "Proportion for each club(top 50 players)", cex = 0.8)

# Clears all objects in the global environment
# rm(list = ls())
