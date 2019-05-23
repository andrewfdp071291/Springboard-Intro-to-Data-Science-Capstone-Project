#Load necessary packages.
library(dplyr)
library(tidyr)

# Set working directory. Load sample file.
setwd ("~/Desktop/Springboard/Projects/DATA_WRANGLING_1")
original <- read.csv(file = "refine_original.csv")

# Examine data
original
str(original)

# Find all unique company name spellings
distinct(original, company, .keep_all = FALSE)
count(distinct(original, company, .keep_all = FALSE))

# 1 - Remove name mispellings and sorting by brand name then product code
original_brand_names_fixed <- original %>% 
  mutate(company = replace(company, grepl("ak", original$company, ignore.case = TRUE),"akzo")) %>%
  mutate(company = replace(company, grepl("ps", original$company, ignore.case = TRUE),"phillips")) %>%
  mutate(company = replace(company, grepl("van", original$company, ignore.case = TRUE),"van houten")) %>% 
  mutate(company = replace(company, grepl("un", original$company, ignore.case = TRUE),"unilever")) %>% 
  arrange(company, Product.code...number)

# Checking again for unique company name spellings
original
original_brand_names_fixed
distinct(original_brand_names_fixed, company, .keep_all = FALSE)
count(distinct(original_brand_names_fixed, company, .keep_all = FALSE))

# 2 - Separate product code and number
original_split_product_codes <- original_brand_names_fixed %>% 
  separate(Product.code...number, c("product_code", "product_number"), sep = "-")
original_split_product_codes 

# 3 - Add product categories
original_product_categories <- original_split_product_codes %>% 
  mutate(product_category = 
           ifelse (product_code %in% "p", "Smartphone",
           ifelse (product_code %in% "v", "TV",
           ifelse (product_code %in%  "x", "Laptop",
           ifelse (product_code %in%  "q", "Tablet", "NA"))))) %>% 
  select(company, product_category, product_code, product_number, address, city, country, name)
original_product_categories
     
# 4 - Add full address for geocoding
original_full_address <- original_product_categories %>%
  unite("full_address","address", "city", "country", sep = ", ", remove = FALSE)
original_full_address

# 5 - Create dummy variables for company and product category
original_dummy_variables <- original_full_address %>%
  mutate(company_azko = 
           ifelse (company %in% "akzo", 1, 0)) %>%
  mutate(company_philips = 
           ifelse (company %in% "phillips", 1, 0)) %>%
  mutate(company_van_houten = 
         ifelse (company %in% "van houten", 1, 0)) %>%
  mutate(company_unilever = 
           ifelse (company %in% "unilever", 1, 0)) %>%
  mutate(product_smartphone = 
         ifelse (product_category %in% "Smartphone", 1, 0)) %>%
  mutate(product_tv = 
           ifelse (product_category %in% "TV", 1, 0)) %>%
  mutate(product_laptop = 
           ifelse (product_category %in% "Laptop", 1, 0)) %>%
  mutate(product_tablet = 
           ifelse (product_category %in% "Tablet", 1, 0))

original_dummy_variables

# print out clean data as csv
write.csv(original_dummy_variables, "refine_clean.csv")
