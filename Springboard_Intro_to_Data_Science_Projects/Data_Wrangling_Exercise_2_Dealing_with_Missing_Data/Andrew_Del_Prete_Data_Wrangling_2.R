## Load necessary packages.
library(dplyr)
library(tidyr)

## Set working directory. Load sample file.
setwd ("~/Desktop/Springboard/Projects/DATA_WRANGLING_2")
titanic <- read.csv(file = "titanic_original.csv")

## Examine data
View(titanic)
str(titanic)

## Finds unique embarked value
embarked_unique <- titanic %>% 
  distinct(embarked,.keep_all = FALSE)
embarked_unique 

## Replacing blank embark elements with S for Southampton
titanic$embarked <- sub(pattern = "^$", "S", titanic$embarked)

## Checking unique embarked values again
embarked_unique <- titanic %>% 
  distinct(embarked,.keep_all = FALSE)
embarked_unique 

## Checking for number of rows with age missing
titanic %>% 
  filter(is.na(age)) %>% 
  NROW()

## Replacing empty age elements with the mean age
titanic$age <- replace(titanic$age, is.na(titanic$age), mean(titanic$age, na.rm = TRUE))

## Checking for number of rows with age missing again
titanic %>% 
  filter(is.na(age)) %>% 
  NROW()
  
## Checking for number of rows with boat missing
titanic %>% 
  filter(grepl("^$", boat)) %>% 
  NROW()

## Replacing rows with boat missing a value to have "None"
titanic$boat <- sub(pattern = "^$", "None", titanic$boat)

## Checking for number of rows with boat missing
titanic %>% 
  filter(grepl("^$", boat)) %>% 
  NROW()

## Add row for has_cabin_number
titanic <- titanic %>% 
  mutate(has_cabin_number = if_else(grepl("^$", titanic$cabin), 0, 1))

## Print out clean data as csv
View(titanic)
write.csv(titanic, "titanic_clean.csv")