#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS: https://usa.ipums.org/usa/index.shtml
# Author: Yutong Wu, Xinxuan Lin, Yujie Wu, Yao He
# Data: 2 November 2020
# Contact: yutongtree.wu@mail.utoronto.ca; fanstina.lin@mail.utoronto.ca; yujie.wu@mail.utoronto.ca; yaobb.he@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/yeonkiii/Documents/uoft/STA304/PS3")
raw_data2 <- read_dta("usa_00003.dta.gz")


# Add the labels
raw_data2 <- labelled::to_factor(raw_data2)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data2 <- 
  raw_data2 %>% 
  select(region,
         sex, 
         age, 
         race, 
         citizen,
         ftotinc)


#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# remove all not citizen observations
reduced_data2 <- 
  reduced_data2 %>%
  filter(citizen != "not a citizen") %>% 
  filter(citizen != "n/a")

# remove age below 16
reduced_data2$age <- as.numeric(reduced_data2$age)
reduced_data2 <- 
  reduced_data2 %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>% 
  filter(age >= 16)


# match census variables' name to survey data
# change sex to gender
reduced_data2 <-
  reduced_data2 %>% 
  rename(gender = sex) %>% 
  mutate(gender = ifelse(gender == "male", "Male", "Female"))

# change race to race_ethnicity
reduced_data2 <-
  reduced_data2 %>% 
  rename(race_ethnicity = race)
reduced_data2$race_ethnicity <- as.character(reduced_data2$race_ethnicity)
unique(reduced_data2$race_ethnicity)

reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "white"] <- "White"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "black/african american/negro"] <- "Black, or African American"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "other race, nec"] <- "Some other race"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "two major races"] <- "Some other race"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "three or more major races"] <- "Some other race"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "chinese"] <- "Asian (Chinese)"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "other asian or pacific islander"] <- "Asian (Other)"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "japanese"] <- "Asian (Japanese)"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "american indian or alaska native"] <- "American Indian or Alaska Native"


# change region to census_region
reduced_data2 <-
  reduced_data2 %>% 
  rename(census_region = region) 
# change division into region name
reduced_data2$census_region <- as.character(reduced_data2$census_region)

  
reduced_data2$census_region[reduced_data2$census_region == "new england division"] <- "Northeast"
reduced_data2$census_region[reduced_data2$census_region == "middle atlantic division"] <- "Northeast"
reduced_data2$census_region[reduced_data2$census_region == "east north central div"] <- "Midwest"
reduced_data2$census_region[reduced_data2$census_region == "west north central div"] <- "Midwest"
reduced_data2$census_region[reduced_data2$census_region == "south atlantic division"] <- "South"  
reduced_data2$census_region[reduced_data2$census_region == "east south central div"] <- "South"
reduced_data2$census_region[reduced_data2$census_region == "west south central div"] <- "South"
reduced_data2$census_region[reduced_data2$census_region == "mountain division"] <- "West"
reduced_data2$census_region[reduced_data2$census_region == "pacific division"] <- "West"
  

# change income to interval
reduced_data2$ftotinc <- as.numeric(reduced_data2$ftotinc)

reduced_data2$ftotinc[reduced_data2$ftotinc >= 100000 & reduced_data2$ftotinc <= 124999] <- "$100,000 to $124,999"
reduced_data2$ftotinc[reduced_data2$ftotinc <= 14999] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 1500] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 2500] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 200] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 150] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 20] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc == 250] <- "Less than $14,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 15000 & reduced_data2$ftotinc <= 19999] <- "$15,000 to $19,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 20000 & reduced_data2$ftotinc <= 24999] <- "$20,000 to $24,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 25000 & reduced_data2$ftotinc <= 29999] <- "$25,000 to $29,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 30000 & reduced_data2$ftotinc <= 34999] <- "$30,000 to $34,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 35000 & reduced_data2$ftotinc <= 39999] <- "$35,000 to $39,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 40000 & reduced_data2$ftotinc <= 44999] <- "$40,000 to $44,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 45000 & reduced_data2$ftotinc <= 49999] <- "$45,000 to $49,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 50000 & reduced_data2$ftotinc <= 54999] <- "$50,000 to $54,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 55000 & reduced_data2$ftotinc <= 59999] <- "$55,000 to $59,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 60000 & reduced_data2$ftotinc <= 64999] <- "$60,000 to $64,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 65000 & reduced_data2$ftotinc <= 69999] <- "$65,000 to $69,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 70000 & reduced_data2$ftotinc <= 74999] <- "$70,000 to $74,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 75000 & reduced_data2$ftotinc <= 79999] <- "$75,000 to $79,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 80000 & reduced_data2$ftotinc <= 84999] <- "$80,000 to $84,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 85000 & reduced_data2$ftotinc <= 89999] <- "$85,000 to $89,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 90000 & reduced_data2$ftotinc <= 94999] <- "$90,000 to $94,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 95000 & reduced_data2$ftotinc <= 99999] <- "$95,000 to $99,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 125000 & reduced_data2$ftotinc <= 149999] <- "$125,000 to $149,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 150000 & reduced_data2$ftotinc <= 174999] <- "$150,000 to $174,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 175000 & reduced_data2$ftotinc <= 199999] <- "$175,000 to $199,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 200000 & reduced_data2$ftotinc <= 249999] <- "$20,0000 to $249,999"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 250000] <- "$250,000 and above"
reduced_data2$ftotinc[reduced_data2$ftotinc >= 1e+06] <- "$250,000 and above"

  
# change ftotinc to household_income
reduced_data2 <-
  reduced_data2 %>% 
  rename(household_income = ftotinc)
 

# group
reduced_data2 <- 
  reduced_data2 %>%
  count(age, gender, household_income,race_ethnicity,census_region) %>%
  group_by(age, gender, household_income,race_ethnicity,census_region) 
 
# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data2, "census_data.csv")



