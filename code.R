library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)


master_Data <- read_excel("data_raw/List of all local levels.xlsx")

#Theme: Accountability
##Sub-theme: irregularity data
fisc_irregularity <- master_Data %>% 
  select(number = `S.N`,
         local = `Local Level`,
         type = Type,
         province = Province,
         district = District,
         aud = `Audit Amount`,
         irr = Fiscal_irregularities,
         per = Fiscal_irregularities_pcg,
         previous_year = Carryover_from_last_year,
         tot_income = Total_income,
         unused = Unused_amount,
         Internal_income = Internal_income,
         Total_income = Total_income,
         Total_expenditure = Total_expenditure,
         Capital_expenditure = Capital_expenditure
         ) %>% 
  mutate(percentage = (irr/aud)*100) %>% 
  mutate(quantile = ntile(percentage, 5)) %>% 
  mutate(`irregularity score` = 6 - quantile) %>%
  mutate(quantile = NULL) %>%
  mutate(resource_used = 1 - (unused/(previous_year+tot_income))) %>% 
  mutate(`resource used score` = ntile(resource_used, 5)) %>% 
  mutate(`internal_rev_capacity` = (Internal_income/Total_income)) %>% 
  mutate(`Score for Internal Revenue Generating Capacity` = ntile(internal_rev_capacity, 5)) %>%
  mutate(Capital_exp_share = Capital_expenditure/Total_expenditure) %>% 
  mutate(`Score for Capital Expenditure as share of total expenditure` = ntile(Capital_exp_share, 5)) %>% 
  filter(province == "Madhesh")


#Done*****************************************
##Resource use out of overall available resources
##Revenue generating capacity of local levels
## Capital expenditure as share of total expenditure

#Remaining***********
#merge population data to get per-capita scores
#convert the scores in 0,1 terms for transparency scores
#Find the shape for for local levels of MAdhesh province
#Create vizualization

