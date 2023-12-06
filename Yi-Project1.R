#Name: Junchen Yi. Date: 24/09/2023. Class: ALY6000
cat("\014") # clears console
rm(list = ls()) # clears global environment 
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session



#library(pacman)

library(pacman)
p_load(testthat)
test_file("project1_tests.R")

# 1.
123 * 453
5^2 * 40
TRUE & FALSE
TRUE | FALSE
75 %% 10
75 / 10

# 2.
first_vector <- c(17, 12, -33, 5)
first_vector

# 3.
counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)
counting_by_fives

# 4.
second_vector <- seq(from = 10, to = 30, by = 2)
second_vector

# 5.
counting_by_fives_with_seq <- seq(from = 5, to = 35, by = 5)
counting_by_fives_with_seq

# 6.
third_vector <- rep(first_vector, times = 10)
third_vector

# 7.
rep_vector <- rep(0, times = 20)
rep_vector

# 8.
fourth_vector <- 10 : 1
fourth_vector

# 9.
counting_vector <- 5 : 15
counting_vector

# 10.
grades <- c(96, 100, 85, 92, 81, 72)
grades

# 11.
bonus_points_added <- grades + 3
bonus_points_added

# 12.
one_to_one_hundred <- seq(1, 100)
one_to_one_hundred

# 13.
reverse_numbers <- seq(100, -100, by = -3)
reverse_numbers

# 14.
second_vector + 20 # It means each element in second_voctors is added by 20
second_vector * 20 # It means each element in second_voctors is times by 20
second_vector >= 20 # Determine whether each element in second_vector is greater than or equal to 20
second_vector != 20 # Determines whether each element in the array is not equal to 20. 
#If it is not equal to 20, the result is true, otherwise it is false

# 15.
one_to_one_hundred <- seq(1, 100)
total <- sum (one_to_one_hundred)
total

# 16.
average_value <- mean(one_to_one_hundred)
average_value

# 17.
median_value <- median(one_to_one_hundred)
median_value

# 18.
max_value <- max(one_to_one_hundred)
max_value

# 19.
min_value <- min(one_to_one_hundred)
min_value

# 20.
first_value <- second_vector[1]
first_value

# 21.
first_three_values <- second_vector[1 : 3]
first_three_values

# 22.
vector_from_brackets <- second_vector[c(1, 5, 10, 11)]
vector_from_brackets

# 23.
logical_indices <- c(FALSE, TRUE, FALSE, TRUE)
# Extract elements from 'first_vector' using the logical indices
vector_from_boolean_brackets <- first_vector[logical_indices]
vector_from_boolean_brackets
# logical_indices is a logical vector that specifies which elements to extract from first_vector.
# The TRUE value represents the element to be extracted, while the FALSE value represents the element to be excluded.
# Therefore, when assigning the variable vector_from_boolean_brackets, since the values of 12 and 5 correspond to TRUE in logical_indices, the final output is 12 5

# 24.
second_vector
second_vector >= 20
# Checks each element in second_vector, returning TRUE if the element is greater than 20 and FALSE if it is less than 20

# 25.
ages_vector <- seq(from = 10, to = 30, by = 2)
# Create a sequence of numbers representing age, starting from 10 and increasing by 2 with each element.
ages_vector

# 26.
ages_vector [ages_vector >= 20]
# Indicates that ages_vector filters a number of eligible elements on the basis of the original, 
#each of which is greater than or equal to 20

# 27.
grades_Filter <- grades >= 85
lowest_grades_removed <- grades[grades_Filter]
lowest_grades_removed

# 28.
#middle_grades_removed <- grades[-3 : -4]
index_remove <- c(3, 4)
middle_grades_removed <- grades[-index_remove]
middle_grades_removed

# 29.
fifth_vector <- second_vector[-c(5, 10)]
fifth_vector

# 30.
set.seed(5)
random_vector <- runif(n=10, min = 0, max = 1000)
random_vector
# set.seed () is used to set a random number seed, a specific seed can produce a specific random sequence, 
#the main purpose of this function is to make the simulation repeatable. runif() function 
#generates a vector of 10 random numbers from a uniform distribution between 0 and 1000.
#Because the seed is set to 5, if we run this code again with the same seed, we will get the same set of random numbers.

# 31.
sum_vector <- sum(random_vector)
sum_vector

# 32.
cumsum_vector <- cumsum(random_vector)

# 33.
mean_vector <- mean(random_vector)

# 34.
sd_vector <- sd(random_vector)

# 35.
round_vector <- round(random_vector)

# 36.
sort_vector <- sort(random_vector)

# 37.
set.seed(5)
random_vector <- rnorm(n=1000, mean = 50, sd = 15)
random_vector
#This vector contains 1000 random numbers generated from a normal distribution with a mean of 50 and a standard deviation of 15.
#Since we set the seed to 5, if we run this code again with the same seed, we will get the same set of random numbers.

# 38.
hist(random_vector)
# X-axis: The X-axis of the histogram represents the range of values in a random vector and is divided into different intervals.
# Y-axis: The Y-axis represents the frequency or count of values falling between each bin. 
#         It represents how many values in the random vector fall into each interval.

# 40.
p_load(tidyverse)

# 41.
first_dataframe <- read_csv("ds_salaries.csv")
first_dataframe

# 42.
head(first_dataframe)
# Running head(first_dataframe) will display the first 6 rows of the first_dataframe (by default) and the value of each of its columns.
head(first_dataframe, n = 7)
# n = 7 means this function used to view the first 7 rows of the data frame "first_dataframe".
names(first_dataframe)
# The function names(first_dataframe) function is used to retrieve the names of the columns, which are variables of the data frame.
smaller_dataframe <- select(first_dataframe, job_title, salary_in_usd)
# Create a new data frame called smaller_dataframe by selecting specific three columns from the original data frame first_dataframe
smaller_dataframe
# Display contents of the new dataFrame
better_smaller_dataframe <- arrange(smaller_dataframe, 
                                    desc(salary_in_usd))
better_smaller_dataframe
# The new dataframe will contain the same columns as the smaller_dataframe, and the rows will be sorted in descending order according to the salary_in_usd column, 
#with the rows with the highest salaries at the top

better_smaller_dataframe <- filter(smaller_dataframe, salary_in_usd > 80000)
better_smaller_dataframe
# The better_smaller_dataframe filters the rows in the smaller_dataframe by keeping only those rows with values greater than 80000 in the salary_in_usd column.

better_smaller_dataframe <-
  mutate(smaller_dataframe, salary_in_euros = salary_in_usd * .94)
better_smaller_dataframe
# The mutate() function is used to create a new dataframe and convert 
# the value of the "salary_in_usd" column to the euro "salary_in_euros" column,
#and multiply each value in the column by 0.94.

better_smaller_dataframe <- slice(smaller_dataframe, 1, 1, 2, 3, 4, 10, 1)
better_smaller_dataframe
# create a new data frame named better_smaller_dataframe by selecting specific rows from the original data frame.

ggplot(better_smaller_dataframe) +
  geom_col(mapping = aes(x = job_title, y = salary_in_usd), fill =
             "blue") +
  xlab("Job Title") +
  ylab("Salary in US Dollars") +
  labs(title = "Comparison of Jobs ") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
#The code generates a bar chart with the x-axis representing the job title and the y-axis representing the dollar salary.
#The bars are filled in blue. In addition, it rotates the labels by 50 degrees and aligns them horizontally to the right.  
  
  theme(axis.text.x = element_text(angle = 50, hjust = 1))