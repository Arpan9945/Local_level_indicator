library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(sf)
library(fuzzyjoin)

#initial data
master_Data <- read_excel("List of all local levels.xlsx")
transp <- read_excel("Madhesh-Research-01.xlsx")


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


transp <- transp %>% 
  mutate(Quality_score = case_when(
    Quality == "BEST"|Quality == "HIGH"| Quality == "High" ~ 1,
    TRUE ~ 0)) %>% 
  mutate(redbook =case_when(
    `Budget-redbook(81/82)` == "Partly"|`Budget-redbook(81/82)` == "YES"|
      `Budget-redbook(81/82)` == "YES (Shortcut)"|`Budget-redbook(81/82)` == "YES (shortcut)"|
      `Budget-redbook(81/82)` == "YES" ~ 1,
    TRUE ~ 0)) %>% 
  mutate(planspoli = case_when(
    `Budget-planspolicies(81/82)` == "YES"| `Budget-planspolicies(81/82)` == "Yes"|
      `Budget-planspolicies(81/82)` == "YES (for formality only too shorcut)" ~ 1,
    TRUE ~ 0)) %>% 
  mutate(annual_report = case_when(
    `AnnualReport(80/81)` == "YES" ~1,
    TRUE ~ 0
  )) %>% 
  mutate(arthikain = case_when(
    `Arthik Ain` == "YES" ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(total_transparency_score = arthikain + annual_report + planspoli + redbook + Quality_score)

data1 <- inner_join(transp, fisc_irregularity, by = c("Local Level" = "local", "District" = "district"))

#merging with the shape file
Madhesh_shape <- read_sf("G://local_level_gis//local_level.shp")

Madhesh_shape <- Madhesh_shape %>% 
  filter(PROVINCE == 2)

data1 <- data1 %>% 
  mutate(District = str_to_upper(District)) %>% 
  mutate(`Local Level` = case_when(
    `Local Level` == "Janakpurdham" ~ "Janakpur",
    `Local Level` == "kamala" ~ "Kamala",
    TRUE ~ `Local Level`)) %>% 
  mutate(District = case_when(
    District == "DHANUSA" ~ "DHANUSHA",
    TRUE ~ District))


merged_data_shape <- stringdist_left_join(Madhesh_shape, data1,
                                          by= c("DISTRICT" = "District", "UNIT_NAME" = "Local Level"),
                                          method = "jw",
                                          max_dist = 0.1,
                                          distance_col = "dist")

#checking if they are matched properly
shape_try <- merged_data_shape %>% 
  st_drop_geometry()
unmatched <- anti_join(shape_try, data1, by = c("DISTRICT" = "District", "UNIT_NAME" = "Local Level"))

merged_data_shape <- merged_data_shape %>% 
  mutate(composite_key = paste(UNIT_NAME, DISTRICT_C)) %>% 
  mutate(UNIT_NAME.dist = ifelse(is.na(UNIT_NAME.dist), 0, UNIT_NAME.dist)) %>% 
  group_by(composite_key) %>% 
  filter(UNIT_NAME.dist == min(UNIT_NAME.dist)) %>% 
  ungroup()


#To see which obs is repeated in the shape file
composite_key_counts <- merged_data_shape %>%
  group_by(composite_key) %>%
  summarise(count = n()) %>%
  filter(count > 1)




#Done*****************************************
##Resource use out of overall available resources
##Revenue generating capacity of local levels
## Capital expenditure as share of total expenditure
#convert the scores in 0,1 terms for transparency scores

#Remaining***********
#merge population data to get per-capita scores ()
#Find the shape for for local levels of MAdhesh province
#Create vizualization

