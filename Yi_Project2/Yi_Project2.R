cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

# Load the dplyr package
library(dplyr)
library(pacman)
p_load(tidyverse)

#Assignment Part 1 
#1.
data_2015 <- read.csv("2015.csv")
head(data_2015)

#2.
names(data_2015)

#3.
# Use the View() function to view the data set in a separate tab


#4.
#Use the glimpse function to view your data set in another configuration.
glimpse(data_2015)

#5.
p_load(janitor) # install the janitor package
data_2015 <- clean_names(data_2015) # given a data frame to make the names more R friendly
data_2015

#6.
# Create a new data frame 'happy_df' with four selected columns
# %>% is the pipe operator, Pass the output of the previous line of code to the next line of code as input
happy_df <- data_2015 %>% select(country, region, happiness_score, freedom)
happy_df

#7.
# Slice the first 10 rows and store them as top_ten_df
top_ten_df <- happy_df %>%
  slice(1:10)
top_ten_df

#8.
# Filter the data for freedom values under 0.20 and store in no_freedom_df
no_freedom_df <- happy_df %>%
  filter(freedom < 0.20)
no_freedom_df

#9.
# Arrange the data in descending order by "freedom" and store in best_freedom_df
best_freedom_df <- happy_df %>%
  arrange(desc(freedom))
best_freedom_df

#10.
data_2015 <- data_2015%>%
  mutate(gff_stat = family + freedom + generosity)

data_2015

#11.
happy_summary <- happy_df %>%
  summarise(
    mean_happiness = mean(happiness_score),
    max_happiness = max(happiness_score),
    mean_freedom = mean(freedom),
    max_freedom = max(freedom)
  )
happy_summary

#12.
regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarise(
    country_count = n(), #Counting the number of occurrences for each country
    mean_happiness = mean(happiness_score),
    mean_freedom = mean(freedom)
  )
regional_stats_df

#13.
#Filter the data to select Western European and Sub-Saharan African countries
#First step: Filter two kinks of countries from dataset data_2015
#Check that each value of "region" on the left of the symbol appears in the set of values on the right.
# Filter rows with "Region" equal to "Western Europe"
filtered_region_WE <- data_2015 %>%
  filter(region %in% c("Western Europe"))
# Filter rows with "Region" equal to "Sub-Saharan Africa"
filtered_region_SSA <- data_2015 %>%
  filter(region %in% c("Sub-Saharan Africa"))
filtered_region_SSA
#Second step:Order data and select 10 countries for each region
#Order data for "Western Europe"
ordered_data_WE <- filtered_region_WE %>%
  arrange(happiness_score)

#Order data for "Sub-Saharan Africa"
ordered_data_SSA <- filtered_region_SSA %>%
  arrange(happiness_score)

countries_for_SSA <- tail(ordered_data_SSA,10)
countries_for_WE <- head(ordered_data_WE,10)

#Third step:Calculate average GDP
gdp_SSA <- mean(countries_for_SSA$economy_gdp_per_capita)
gdp_WE <- mean(countries_for_WE$economy_gdp_per_capita)

# Print dataframe
gdp_df <- data.frame(
  Region = c("europe_gdp", "africa_gdp"),
  Average_GDP_Per_Capita = c(gdp_WE, gdp_SSA)
)
gdp_df

#14.
#
scatterplot <- ggplot(regional_stats_df, aes(x = mean_happiness, 
                                             y =mean_freedom,color=region)) +geom_point()+
  geom_segment(aes(x = min(mean_happiness), y = min(mean_freedom),
                   xend = max(mean_happiness), yend = max(mean_freedom)),
               linetype = "solid", color = "black")
scatterplot


#Assignment Part 2
#1.
baseball <- read_csv("baseball.csv")

#3.
data_class <- class(baseball)
#data_class

#4.
age_stats_df<-baseball %>% group_by(Age) %>%
  summarise(count=n(),HR=mean(HR),H=mean(H),R=mean(R))
age_stats_df

#5.
baseball <- baseball %>%
  filter(AB != 0)
baseball

#6.
# Add a new column for batting average (BA)
baseball <- baseball %>%
  mutate(BA = H / AB)
baseball

#7.
# Modify the "BA" column to round values to three decimal places
baseball <- baseball %>%
  mutate(BA = round(BA, digits = 3))
baseball

#8.
# Add a new column for on-base percentage (OBP)
baseball <- baseball %>%
  mutate(OBP = (H + BB) / (AB + BB))
baseball

#9.
baseball <- baseball %>%
  mutate(OBP = round(OBP, digits = 3))

#10.
# Arrange the dataset in increasing order of strikeouts (SO)
sorted_data <- baseball %>%
  arrange(desc(SO))

# Select the top 10 players with the most strikeouts
strikeout_artist <- head(sorted_data, 10)
strikeout_artist

#11.
scatterplot <- ggplot(baseball, aes(x = HR, y = RBI)) +
  geom_point() +
  labs(
    x = "Number of Home Runs (HRs)",
    y = "Number of RBIs (RBIs)",
  )
scatterplot

#12.
eligible_df <- baseball %>%
  filter(AB >= 300 | G >= 100)
eligible_df

#13.
#Create a histogram of batting average
histogram <- ggplot(eligible_df, aes(x = BA)) +
  geom_histogram(binwidth = 0.025, color = "blue", fill = "green") +
  labs(
    x = "Batting Average (BA)",
    y = "Count",
    title = "Histogram of Batting Average for Eligible Players"
  )

# Display
histogram

#14.
eligible_df <- eligible_df |>
  mutate(HR_Rank =rank(-1 * HR, ties.method = "min"))
eligible_df

#15.
eligible_df <- eligible_df %>%
  mutate(RBI_Rank = rank(-1*RBI, ties.method = "min"),OBP_Rank = rank(-1*OBP, ties.method = "min"))
eligible_df

#16.
eligible_df <- eligible_df %>%
  mutate(TotalRank = HR_Rank + RBI_Rank + OBP_Rank)
eligible_df

#17.
ascending_data <- eligible_df %>%
  arrange(TotalRank)

mvp_candidates <- head(ascending_data, 20)

#18.
# Selecting specific columns from mvp_candidates
mvp_candidates_abbreviated <- mvp_candidates %>%
  select(First, Last, HR_Rank, RBI_Rank, OBP_Rank, TotalRank)
mvp_candidates_abbreviated