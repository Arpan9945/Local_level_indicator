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

data1 <- left_join(transp, fisc_irregularity, by = c("Local Level" = "local", "District" = "district"))

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

#To see which obs is repeated in the shape file -- not necessary now
composite_key_counts <- merged_data_shape %>%
  group_by(composite_key) %>%
  summarise(count = n()) %>%
  filter(count > 1)

#Remove all the unnecessary dataframes created
rm(Madhesh_shape, shape_try, unmatched, composite_key_counts)

#------------------------------Results------------------------------------------

#1. For transparency- Seeing the result for individual component
transp %>% group_by(total_transparency_score) %>% 
  count(Type) %>% 
  ungroup()
transp %>% count(redbook)
transp %>% count(planspoli)
transp %>%count(annual_report)
transp %>% count(arthikain)
transp %>% count(Quality_score)

rm(transp_long)
transp_long <- transp %>% 
  pivot_longer(cols = c(redbook, planspoli, annual_report, arthikain, Quality_score),
               names_to = "Variable", values_to = "Value")

#To see the numbers of uploaded documents
count_data_transp <- transp %>% 
  select(redbook, planspoli, annual_report, arthikain) %>% 
  rename("Red Book" = redbook,
         "Plans and Policies" = planspoli,
         "Annual Report"= annual_report ,
         "Financial Act" = arthikain) %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>% 
  mutate(Value = factor(Value, levels = c(0,1), labels = c("Not-Uploaded", "Uploaded")))%>% 
  group_by(Category, Value) %>%
  rename("Document Status" = Value) %>% 
  summarise(Count = n(), .groups = "drop")

#Plotting Document Status bar-graph
ggplot(count_data_transp, aes(x = Category, y = Count, fill = as.factor(`Document Status`))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Not-Uploaded" = "#1C0F0F","Uploaded" = "#A3A3A3"))+ 
  labs(title = "Number and Status of Document Uploads by Local Governments",
       x = "Document Type", y = "Number of Local Levels", fill = "`Document Status`") +
  theme_minimal()

merged_data_shape <- merged_data_shape %>% 
  mutate(total_transparency_score = as.factor(total_transparency_score))

#Plotting transparency score in map
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = total_transparency_score)) +
  scale_fill_manual(name = "Score",
                    values = c("0" = "#cecece",
                               "1" = "#9d9d9d", 
                               "2" = "#858585", 
                               "3" = "#545454", 
                               "4" = "#232323", 
                               "5" = "#FDE725"),
                    na.value = "#ffffff") +
  theme_minimal() +
  ggtitle("Composite Index of Transparency")





#----------------------------Visualization--------------------------------------

#GGPLOT
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = `total_transparency_score`))+
  scale_fill_manual(values = c("#858585", "#6d6d6d","#545454","#3c3c3c","#232323"), na.value = "#0b0b0b")
  scale_fill_viridis_c(option = "D", na.value = "transparent")+
  theme_minimal()+
  ggtitle("Theme: Resource Use")

#different color choice

merged_data_shape1 <- merged_data_shape %>%
  mutate(score_category = cut(total_transparency_score, 
                              breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                              labels = c("Score: 0", "Score: 1", "Score: 2", "Score: 3", "Score: 4", "Score: 5")))


ggplot(data = merged_data_shape1) +
  geom_sf(aes(fill = score_category))+
  scale_fill_manual(values = c(
    "Score: 1" = "#858585",      
    "Score: 2" = "#6d6d6d",
    "Score: 3" = "#545454",    
    "Score: 4" = "#3c3c3c",    
    "Score: 5" = "#232323"     
  ), na.value = "#0b0b0b") + 
  theme_minimal()+
  ggtitle("Theme: Resource Use")






#Done*****************************************
##Resource use out of overall available resources
##Revenue generating capacity of local levels
## Capital expenditure as share of total expenditure
#convert the scores in 0,1 terms for transparency scores

#Remaining***********
#merge population data to get per-capita scores ()
#Find the shape for for local levels of MAdhesh province
#Create vizualization

