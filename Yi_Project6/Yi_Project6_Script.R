#Junchen Yi  Date: 27/10/2023 Class:ALY6000 70407

library(palmerpenguins)
library(e1071)
library(ggplot2)
library(tidyverse)


#1.
p <- 0.65
k <- 5
n <- 7
prob1_result <- choose(n, k) * p^k * (1 - p) ^ (n - k)

#2.
num_win_game <- seq(0, 7)
each_probability <- dbinom(num_win_game, size = n, prob = p)
prob2_result <- tibble(win_games = num_win_game, probability = each_probability)

#3.
prob3_result <- pbinom(4, size = 7, prob = 0.65)

#4.
#probability of win 5 games - 3 games
five_probability <- pbinom(5, size = n, prob = p)
two_probability  <- pbinom(2, size = n, prob = p)
prob4_result <- five_probability - two_probability

#5.
four_probability <- pbinom(4, size = n, prob = p)
prob5_result <-  1 - four_probability

#6.expected value
prob6_result <- n * p

#7.
prob7_result <- n * p * (1 - p)

#8.
set.seed(10)
red_sox_wins <- sample(prob2_result$win_games, 1000, replace = TRUE, prob = prob2_result$probability)

#9.
prob9_result <- mean(red_sox_wins)

#10.
prob10_result <- var(red_sox_wins)

#11.
lambda <- 7
num_calls <- 6
prob11_result <- dpois(num_calls, lambda)

#12. use cumulative distribution function (CDF)
lambda_8 <- 7 * 8
prob12_result <- ppois(40, lambda_8)

#13.
#Total Average Rate 
total_lambda <- lambda_8 * 5
num_calls <- 274
less_275_p <- ppois(num_calls, total_lambda)
prob13_result <- 1 - less_275_p

#14.
total_lambda <- lambda_8 * 4
prob14_result <- 1 - ppois(num_calls, total_lambda)

#15 
#an event that has a 90% probability of being less than or equal to this value 
#in the Poisson distribution
prob15_result <- qpois(1 - 0.1, lambda_8)

#16.
set.seed(15)
random_calls <- rpois(1000, lambda_8)

#17.
prob17_result <- mean(random_calls)

#18.
prob18_result <- var(random_calls)

#19.
sd <- 100
low_lifespan <- 1800
high_lifespan <- 2200
mean_lifespan <- 2000
# Calsulate the z-score
z1 <- (low_lifespan - mean_lifespan) / sd
z2 <- (high_lifespan - mean_lifespan) / sd
#cumulative probability
lower_p <- pnorm(z1)
high_p <- pnorm(z2)
between_p <- high_p - lower_p
prob19_result <- between_p

#20.
z_lower_2500 <- (2500 - mean_lifespan) / sd
cumulative_p <- pnorm(z_lower_2500)
prob20_result <- 1 - cumulative_p

#21.
hours_10_percent <- qnorm(0.1, mean = mean_lifespan, sd = sd)
prob21_result <- ceiling(hours_10_percent)

#22.
set.seed(25)
pop_lifespan <- rnorm(10000, mean = mean_lifespan, sd = sd)

#23.
prob23_result <- mean(pop_lifespan)

#24.
prob24_result <- sd(pop_lifespan)

#25.
set.seed(1)
size <- 100
samples_num <-1000
sample_mean <- numeric(1000) #Creates a numerical vector with 1000 length
for (i in 1:samples_num) {
  sample_mean[i] <- mean(sample(pop_lifespan, size = size, replace = FALSE))
}

#26.
hist(sample_mean, main = "Histogram of Sample Means", xlab = "Sample Mean", ylab = "Frequency")

#27.
prob27_result <- mean(sample_mean)

#28.
adelie <- subset(penguins, species == "Adelie")
ggplot(adelie, aes(x=flipper_length_mm)) +
  geom_histogram(aes(y=..density..), binwidth=5, fill="skyblue", color="black") +
  geom_density(color="blue") +
  labs(title="Distribution of Flipper Length of Adélie Penguin",
       x="Flipper Length (mm)", y="Density") +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro_wilk <- shapiro.test(adelie$flipper_length_mm)

# Calculate skewness and kurtosis
skewness <- skewness(adelie$flipper_length_mm, na.rm = TRUE)
kurtosis <- kurtosis(adelie$flipper_length_mm, na.rm = TRUE)

shapiro_wilk$p.value # 0.7200466
skewness # 0.08560929
kurtosis # 0.2382734

#29.
gentoo <- subset(penguins, species == "Gentoo")
ggplot(gentoo, aes(x = flipper_length_mm, y = bill_depth_mm, color = bill_depth_mm)) +
  geom_point() +
  ggtitle(" Flipper Length vs. Beak Depth ") +
  xlab("Flipper Length (mm)") +
  ylab("Beak Depth (mm)")

#Pearson's correlation coefficient
cor_test <- cor.test(gentoo$flipper_length_mm, gentoo$bill_depth_mm)
cor_result <- cor_test$estimate
cor_result #0.7065634 
