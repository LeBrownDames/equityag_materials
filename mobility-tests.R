
# title: "Lesson 3 Workflow and Statistical Tests in R
# author: Leo Brown
# date: "2025-06-05"


## Load package
library(arrow) # For reading parquet files
library(tidyverse) # For data manipulation and vizualization


## Read in data
merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)


## Boxplot of data
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

# Remove anomolies 
merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"), ]

# Re-plot the boxplot to confirm the isssue is resolved
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

# Boxplot to explore total population by state
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

# SOME NOTES
  # Boxplot uses the formula syntax: y ~ group
  # The left of the ~ is plotted on the y-axis
  # The right is the grouping variable on the x-axis.
  # Can always check a function by putting ?(boxplot i.e)


## Calculating mean upward mobility for California and Arizona - 2 Methods

# 1. Traditional R method - Create two new datasets by filtering the tows based on the state abbreviations
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

# Calculate the average upward mobility rate in 2010 for each state using the mean() function
print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))
print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

# Store means in new data frame so that they are easier to use later 
upward_mobility_means_2010 <- data.frame(State = c ("CA", "AZ"), 
                                         up_2010_mean = c(upward_mean_2010_ca, upward_mean_2010_az))

# 2. Tidyverse Method (concise and Scaleable) - Group the full dataset by state abbreviation, then calculate the mean upward mobility rate for each group.
state_group <- merged_data %>%
  group_by(STATE_ABBR_2010SVI)

state_mob_means <- state_group %>%
  summarize(up_mean = mean(upward_mobility_rate_2010, na.rm = TRUE))

# Remove any rows where the syaye abbreviation is missing
state_mob_means <- state_mob_means %>%
  filter(!is.na(STATE_ABBR_2010SVI))

# Pipe Operator
# NOTE: The pipe operator %>% sends the result from the left-hand side to the first argument of the function on the right-hand side. It makes the code more readable and helps us chain multiple commands together.
upward <- merged_data %>% 
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))

# Remove rows where the state is NA
upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))


## Adding Error Bars with ggplot2 - Visualize means & st.d
upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))

#NOTE ON PREVIOUS STEP
# The standard error (SE), which tells us how spread out the data is. It’s calculated by taking the standard deviation divided by the square root of the sample size. This gives us a more precise picture of variation across counties within each state.

# Drop the NA
upward_stats <- upward_stats %>% filter(!is.na(STATE_ABBR_2010SVI))

# Basic ggplot with Error Bars
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point()

# Add error bars using geom_errorbar()
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se, 
                              ymax = up_means +up_se), 
                width = 0.3) # makes thinner error lines

# Cleaned up by state instead of by county and state
upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))

# Drop the NA
upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))                  

# Redo the Graph
ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se),
                width = 0.3)

# More tidyverse tricks
merged <- merged_data # making my own editable copy of merged_data, keeping the origional unchanged

# Selecting columns - select() removes columns with underscores and keeps those that start with "upward"
merged %>%
  dplyr:: select(!contains("_"), starts_with ("upward"))

# Reording columns - relocate() puts columns containing "STATE" after the 2020 upward mobility rate column
merged <- merged %>% 
  dplyr:: relocate(contains("STATE"), .after = upward_mobility_rate_2020)

# Create and place a unique identifier
unique_id <- merged %>%
  dplyr:: group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  dplyr:: mutate(uniqueid = row_number(), .before = contains("_"))

## Pivoting (wide<->long) - THIS IS NOT A CODE BUT AN EXAMPLE

# Wide to long

# merged_long <- unique_id %>%
  # tidyr::pivot_longer(contains("_"),
    # names_to = c("upward"),
    #names_sep =("_"))


#this is general synthax, but doesn't make sense for what we're doing right now

# Long to wide

# merged_wide <- _merged_long %>%
  # tidyr::pivot_wider(names_from = c("upward),
    # values_from = "value",
    #names_sep = "_")


#again, as we didn't change, muted code

# Summarize Across Multiple Columns
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"),
                          list(~mean(.x, na.rm = TRUE), 
                               ~sd(.x, na.rm = TRUE)))) 

# Rename columns when summarizing
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}")) 

## Advanced: Modeling and Nesting

library(dplyr)

# modeling
upward_models <- merged %>%
  filter(!is.na(STATE_ABBR_2010SVI),
         !is.na(upward_mobility_rate_2010),
         !is.na(POP2010)) %>%
  group_by(STATE_ABBR_2010SVI) %>% 
  summarise(model = list(lm(upward_mobility_rate_2010 ~ POP2010)))

# Nesting
merged <- nest_by(state_group)


## Running a basic statistical test: Student's t-test
merged <- merged_data

# Statistical testing: t-test - compares two groups to see if their averages are statistically different (are groups different enough that it's unlikely that this happened by chance?)

# t-test of upward mobility between CA and AZ
az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California", ]

# see how many rows and columns are in a data.frame with the dim() command
dim(merged_data)

# This returns all columns for the third row in merged_data:
merged_data[3,]

# Or the third column
merged_data[,3]

# Now get rows where the state is Arizona or California
print(nrow(az))
print(nrow(ca))

# Now run a t test to compare upward mobility
t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)

# What you’ll get in the output:
  # t-statistic: A number that tells how far apart the group means are, relative to the variability in the data
  # df (degrees of freedom): Related to the sample size
  # p-value: The key number for interpreting the test—if it’s below your threshold (commonly 0.05), the result is statistically significant
  # Confidence interval: The estimated range in which the true difference in means likely falls
  # Means of x and y: The average upward mobility in AZ and CA.

#Hypothesis in Stats
# Null hypothesis (H₀): No effect/difference
# Alternative hypothesis (H₁): There is a difference/effect.

## ANOVA: Multiple Group Comparisons - analysis of variance, comparing multiple groups at once
aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

# Mobility rate explained by county
mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

# looking at explaination
summary(object = mobility_rate_az_aov)

#SOME NOTES
  # df Degrees of freedom (how many groups)
  # Sum Sq (sum of squares) / Mean Sq (mean squares) Measures of variation (how spread out the data is). It is used to calculated our test statistic here: F- value
  # F value The test statistic (like the “t” in a t-test, higher = more evidence against null). Calculated as the ratio of the between-group variance to the within-group variance
  # Pr(>F) This is the p-value — again, if it’s small, the differences between counties are real.

#Save output to a file - use sink
sink(file = "output/az_mobility_annova.txt")
summary(object = mobility_rate_az_aov)
sink()

## Linear Regression

# look at the data
summary(merged_data[, 1:10], 6)

# vizualize
plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

# Log transformation to make a linear relationship
merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

# Plot again
plot(x = merged$upward_mobility_rate_2010, 
     y = merged$logpop, 
     xlab = "Upward Mobility", 
     ylab = "log10(Population)")

# Run a regression
mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)

# The results
summary(mobility_v_pop)

# Save regression results
sink(file = "output/mobility-pop-regression.txt")
summary(mobility_v_pop)
sink()


## Multivarite Regression: Let's add state

# Dummy variable to compare arizona to california in effort to link mobility rates to one state
merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

# Add az as a predictor and run the new model
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az
                           , data = merged)
summary(mobility_v_pop_state)

# Save the results
sink(file = "output/mobility-pop-state-regression.txt")
summary(mobility_v_pop)
sink()


## Student Activity!

# Part 1. Variable exploration
boxplot(formula = E_UNEMP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

## Part 1 Questions
  # 1. What did this graph show? - It shows the unemployment rates by county in Arizona and California
  #  2. Why did you choose this variable? - Because I was curious to see how unemployment rates would differ between the two staets


## Part 2. ANOVA: Multiple Group Comparisons - analysis of variance, comparing multiple groups at once
aov(formula = E_UNEMP_2010SVI ~ COUNTY_2010SVI, data = az)

# Mobility rate explained by county
unemp_rate_az_aov <- aov(formula = E_UNEMP_2010SVI ~ COUNTY_2010SVI, data = az)

# looking at explaination
summary(object = unemp_rate_az_aov)

## Part 2 Questions
  # 1. What did you find in the analysis? - I found, given that p=2.2e-06, that there is in fact a high indication that unemployment rates differ significantly across counties


## Part 3. Another Regression

# 2020 data and more
aov(formula = upward_mobility_rate_2020 + E_UNEMP_2010SVI ~ COUNTY_2010SVI, data = az)

# Mobility rate explained by county
mobility_unemp_rate_az_aov <- aov(formula = upward_mobility_rate_2020 + E_UNEMP_2010SVI ~ COUNTY_2010SVI, data = az)

# looking at explaination
summary(object = mobility_unemp_rate_az_aov)

# How much variation is due to county level differences
eta_sq <- 470918 / (470918 + 13300615)
print(eta_sq)


## Part 4. Reflect and Extend
  # I feel like I need more time to practice and explore the functions that create regressions to be able to replicate this excercise in a research setting.
  # The energy and support from the internship has been great. So has ChatGPT
  # I could use more explanations of some functions to be able to really understand some of what I wrote, but for the most part, I was following along quite well.




  

