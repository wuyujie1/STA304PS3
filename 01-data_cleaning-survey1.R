#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape ‘Full Data Set'[https://www.voterstudygroup.org/publication/nationscape-data-set.]
# Author: Yutong Wu, Xinxuan Lin, Yujie Wu, Yao He
# Data: 2 November 2020
# Contact: yutongtree.wu@mail.utoronto.ca; fanstina.lin@mail.utoronto.ca; yujie.wu@mail.utoronto.ca; yaobb.he@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/yeonkiii/Documents/uoft/STA304/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(
         vote_2020, 
         gender,
         census_region,
         race_ethnicity,
         household_income,
         education,
         age)

#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
reduced_data<-
  reduced_data %>%
  filter(!is.na(household_income))


reduced_data<-
  reduced_data %>%
  filter(vote_2020 != "I would not vote") %>% 
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>% 
  mutate(vote_biden = 
         ifelse(vote_2020=="Joe Biden", 1, 0))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

