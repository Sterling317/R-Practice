#Junchen Yi  Date: 23/10/2023 Class:ALY6000 70407
library(ggplot2)
library(dplyr)
#1.
ball <- read.csv("ball-dataset.csv")

#2.
freq_color <- table(ball$color)
freq_color <- data.frame(color = names(freq_color), counts = as.vector(freq_color))

#3.
freq_label <- table(ball$label)
freq_label <- data.frame(label = names(freq_label), counts = as.vector(freq_label))

#4.
color_mapping <- c("blue", "green", "red", "yellow")
ggplot(freq_color, aes(x = color, y = counts, fill = color_mapping)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_identity()+
  labs(title = "Color Counts Of Balls") +
  xlab("Color") +
  ylab("Count")

#5.
ggplot(freq_label, aes(x = label, y = counts, fill = label)) +
  geom_bar(stat = "identity", width = 0.75) +
  labs(title = "Label Counts Of Balls") +
  xlab("Label") +
  ylab("Count")

#6.
total_balls <- nrow(ball)
green_balls <- sum(ball$color == "green")
prob6_result <- green_balls / total_balls

#7.
red_balls <- sum(ball$color == "red")
blue_balls <- sum(ball$color == "blue")
prob7_result <- (blue_balls + red_balls) / total_balls

#8.
label_a <- sum(ball$label == "A")
label_c <- sum(ball$label == "C")
prob8_result <- (label_a + label_c) / total_balls

#9.
yellow_d_balls <- sum(ball$label == "D" & ball$color == "yellow")
prob9_result <- yellow_d_balls / total_balls

#10.
# yellow_ball <- sum(ball$color == "yellow")
# label_d <- sum(ball$label == "D")
# yellow_rate <- yellow_ball / total_balls
# d_rate <- label_d / total_balls
d_or_yellow <- sum(ball$label == "D" | ball$color == "yellow")
prob10_result <- d_or_yellow / total_balls

#11.
prob11_result <- (blue_balls / total_balls) * (red_balls / (total_balls - 1))

#12.
prob12_result <- (green_balls / total_balls) * ((green_balls - 1) / (total_balls - 1)) * 
  ((green_balls - 2) / (total_balls - 2)) * ((green_balls - 3) / (total_balls - 3))

#13.
label_b <- sum(ball$label == "B")
prob13_result <- (red_balls / total_balls) * (label_b / total_balls)

#14.
factorial <- function(n){
  if(n < 0){
    return(-1)
  } else if(n == 0){
    return(1)
  } else {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    return(result)
  }
}
factorial(-5)
factorial(3)
factorial(0)
factorial(6)

#18.
coin <- c("H", "T")

# Use expand.grid to create all possible combinations of outcomes
coin_outcomes <- expand.grid(Flip1 = coin, Flip2 = coin,
                                Flip3 = coin, Flip4 = coin)

#19.
probability_heads <- 0.6
probability_tails <- 0.4

# Calculate the number of "H" in each row
coin_outcomes$number_of_heads <- rowSums(coin_outcomes == "H")
# Calculate the probability of each row
coin_outcomes <- coin_outcomes %>%
  mutate(
    probability = case_when(
      number_of_heads == 4 ~ 4 * 0.25,
      number_of_heads == 3 ~ 3 * 0.25,
      number_of_heads == 2 ~ 2 * 0.25,
      number_of_heads == 1 ~ 1 * 0.25,
      number_of_heads == 0 ~ 0 * 0.25,
      TRUE ~ NA 
    )
  )

#
# row_prob <- numeric(nrow(coin_outcomes))
# 
# for (i in 1:nrow(coin_outcomes)) {
#   if(i == 4) {
#     row_prob[i] <- 1
#   } else if(i == 3){
#     row_prob[i] <- 0.75
#   } else if(i == 2){
#     row_prob[i] <- 0.5
#   } else if(i == 1){
#     row_prob[i] <- 0.25
#   } else if(i == 0){
#     row_prob[i] <- 0
#   } 
# }
# coin_outcomes$probability <- row_prob


#20.
# In the following code, I have calculated the probability of each case,
# so I will not show the code here
# The probability of 0 heads: 0.0256
# The probability of 1 heads: 0.1536
# The probability of 2 heads: 0.3456
# The probability of 3 heads: 0.3456
# The probability of 4 heads: 0.1296

#21.
n_flips <- 4  # Number of coin flips
k_heads <- 3  # Number of heads desired
p <- 0.6      # Probability of heads

# Calculate the probability
prob21_result <- choose(n_flips, k_heads) * p^k_heads * (1 - p)^(n_flips - k_heads)

#22.
# probability of two heads
# probability_two_heads <- (probability_heads^2) * (probability_tails^2)
probability_two_heads <- choose(4, 2) * 0.6^2 * (1 - p)^(4 - 2)
# probability of four heads
probability_four_heads<- (probability_heads^4)
prob22_result <-  probability_two_heads + probability_four_heads

#23.
prob23_result <- 1 - probability_four_heads

#24.
probability_of_zero <- choose(4, 0) * 0.6^0 * (1 - p)^(4 - 0)
probability_of_one <- choose(4, 1) * 0.6^1 * (1 - p)^(4 - 1)
probability_of_three <- choose(4, 3) * 0.6^3 * (1 - p)^(4 - 3)

overall_outcome <- data.frame(Heads = c("0", "1", "2", "3", "4"),
                              Probability = c(probability_of_zero, 
                                              probability_of_one,
                                              probability_two_heads,
                                              probability_of_three,
                                              probability_four_heads))
ggplot(overall_outcome, aes(x = Heads, y = Probability)) +
  geom_bar(stat = "identity", width = 0.9, fill = "lightblue") +
  labs(title = "Probability Distibution of Heads for 4 flips") +
  xlab("Number of Heads") +
  ylab("Probability")+
  theme_minimal()

#25.
t = 10 # total number of games, didn't use
k_home = 5 # win 5 games at home
k_away = 5 # win 5 games away
home_prob = 0.75
away_prob = 0.5
# probability of winning 5 games at home
home_result <- choose(5, k_home) * (home_prob ^ k_home) * ((1 - home_prob) ^ (5 - k_home))
# probability of winning 5 games away
away_result <- choose(5, k_away) * (away_prob ^ k_away) * ((1 - away_prob) ^ (5 - k_away))
prob25_result <- home_result * away_result

#26.
# probability of winning one game at home
# one_game_home <- choose(5, 1) * (home_prob ^ 1) * ((1 - home_prob) ^ (5 - 1))
# one_game_away <- choose(5, 1) * (away_prob ^ 1) * ((1 - away_prob) ^ (5 - 1))
zero_game_home <- choose(5, 0) * (home_prob ^ 0) * ((1 - home_prob) ^ (5 - 0))
zero_game_away <- choose(5, 0) * (away_prob ^ 0) * ((1 - away_prob) ^ (5 - 0))
win_zero_game <- zero_game_home * zero_game_away
prob26_result <- 1 - win_zero_game

#27.
n_total_games <- 5
k_home_games <- 3
k_away_games <- 2
# Calculate the number of ways to pick three home games out of five
home_combination <- choose(n_total_games, k_home_games)
away_combination <- choose(n_total_games, k_away_games)
prob27_result <- home_combination * away_combination
