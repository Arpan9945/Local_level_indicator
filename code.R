library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(sf)
library(fuzzyjoin)
library(writexl)

#initial data
master_Data <- read_excel("List of all local levels.xlsx")
transp <- read_excel("Madhesh-Research-01.xlsx")


#------------------------DATA PREPERATION---------------------------------------
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
  mutate(nagarikwadapatra = case_when(
    nagarikwadapatra == "YES" |nagarikwadapatra == "Yes" ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(Quality_nw = case_when(
    Quality_nw == "Extremely High"| Quality_nw == "HIGH"| Quality_nw == "HIgh"| 
      Quality_nw == "High"| Quality_nw == "high" ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(total_transparency_score = arthikain + annual_report + planspoli + redbook + Quality_score,
         total_public_service_delivery = nagarikwadapatra + Quality_nw)

data1 <- left_join(transp, fisc_irregularity, by = c("Local Level" = "local", "District" = "district"))
#rm(fisc_irregularity, transp)

#merging with the shape file
Madhesh_shape <- read_sf("local_level_gis//local_level.shp")

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

#--------------------1. For transparency----------------------------------------
# Seeing the result for individual component
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

merged_data_shape %>% count()



#--------------------2. For Accountability--------------------------------------
#2. For Accountability

colnames(merged_data_shape)

#1. first lets see the top five
top5irregular <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,Fiscal_irregularities_pcg, total_transparency_score) %>% 
  filter(UNIT_TYPE == "Gaunpalika") %>% 
  arrange(desc(Fiscal_irregularities_pcg)) %>% 
  slice_head(n = 5)

top5irregular <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,Fiscal_irregularities_pcg, total_transparency_score) %>% 
  filter(UNIT_TYPE == "Nagarpalika" | UNIT_TYPE == "Upamahanagarpalika" | UNIT_TYPE == "Mahanagarpalika") %>% 
  arrange(desc(Fiscal_irregularities_pcg)) %>% 
  slice_head(n = 5)

rm(top5irregular)

#The mean national average is 2.82 and median value is 2.135. Lets see district-wise how many local levels have 
mean_value <- master_Data %>%
  summarise(mean_fiscal_irregularities = mean(Fiscal_irregularities_pcg),
            median_fiscal_irregularities = median(Fiscal_irregularities_pcg), na.rm = TRUE)

mean_value <- merged_data_shape%>% 
  select(DISTRICT, UNIT_TYPE, Fiscal_irregularities_pcg)

total_value <- mean_value %>%
  filter(UNIT_TYPE != "National Park" & UNIT_TYPE != "Wildlife Reserve" ) %>% 
  mutate(UNIT_TYPE = case_when(
    UNIT_TYPE %in% c("Mahanagarpalika", "Upamahanagarpalika", "Nagarpalika") ~ "Municipality",
    UNIT_TYPE %in% "Gaunpalika" ~ "Rural_municipality",
    TRUE ~ UNIT_TYPE
  )) %>% 
  group_by(DISTRICT, UNIT_TYPE) %>% 
  summarise(Total= n(), .groups = "drop") %>% 
  st_drop_geometry()

mean_value <- mean_value %>% 
  filter(Fiscal_irregularities_pcg > 2.83) %>% 
  mutate(UNIT_TYPE = case_when(
    UNIT_TYPE %in% c("Mahanagarpalika", "Upamahanagarpalika", "Nagarpalika") ~ "Municipality",
    UNIT_TYPE %in% "Gaunpalika" ~ "Rural_municipality",
    TRUE ~ UNIT_TYPE
  )) %>% 
  group_by(DISTRICT, UNIT_TYPE) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  st_drop_geometry()

final_value <- full_join(mean_value, total_value, by = c("DISTRICT", "UNIT_TYPE"))

#drop temp
rm(mean_value, total_value)



plot_data <- final_value %>% 
  mutate(UNIT_TYPE =factor(UNIT_TYPE, levels = c("Municipality", "Rural_municipality")))



# Create the plot (Plot: accountability 2)
ggplot(final_value, aes(x = DISTRICT, fill = UNIT_TYPE)) +
  # Background bars for Total (separate for each UNIT_TYPE)
  geom_bar(aes(y = Total, fill = UNIT_TYPE), stat = "identity", 
           position = position_dodge(width = 0.7), alpha = 0.3) +  
  # Foreground bars for Count (separate for each UNIT_TYPE)
  geom_bar(aes(y = Count, fill = UNIT_TYPE), stat = "identity", 
           position = position_dodge(width = 0.7), color = "black") +  
  # Total values on top (one for each UNIT_TYPE)
  geom_text(aes(y = Total, label = Total), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, color = "black", size = 4) +  
  # Count values inside bars (one for each UNIT_TYPE)
  geom_text(aes(y = Count, label = Count), 
            position = position_dodge(width = 0.7), 
            vjust = 1.5, color = "white", size = 4) +  
  # Custom fill colors and labels
  scale_fill_manual(values = c("Rural_municipality" = "#1C0F0F", "Municipality" = "#A3A3A3"),
                    labels = c("Municipality" = "Municipality, metro \n and sub-metro",
                               "Rural_municipality" = "Rural Municipality")) +
  theme_minimal() +
  labs(
    title = "Number of Local Levels with Percentage Fiscal Irregularities Above National Average (District-wise)",
    x = "District",
    y = "Number of Local Levels",
    fill = "Municipality Type",
    caption = "Lighter bars represent total local levels.\nDarker bars represent local levels with fiscal irregularities above the national average.\nWhite numbers indicate these counts."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting accountability it in the map (Plot: accountability 1)
merged_data_shape <- merged_data_shape %>%
  mutate(`irregularity score` = ifelse(is.na(`irregularity score`), 0, `irregularity score`))
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = as.factor(`irregularity score`))) +
  scale_fill_manual(name = "Score",
                    values = c("0" = "#ffffff",
                               "1" = "#cecece",
                               "2" = "#9d9d9d", 
                               "3" = "#6d6d6d", 
                               "4" = "#3c3c3c", 
                               "5" = "#0b0b0b"),
                    labels = c("1" = "Score: 1", "2" = "Score: 2",
                               "3" = "Score: 3", "4" = "Score: 4", "5" = "Score: 5",
                               "0" = "Conservation area \nor missing audit reports"))+
  theme_minimal() +
  ggtitle("Composite Index of Accountability")+
  labs(caption = "Higher the score, the better the local level is in terms of accountability.")





#--------------------3. For Resource Use --------------------------------------

#3.1 Utilization of available funds
top5iresourceused <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,resource_used) %>%
  mutate(unused = 1-resource_used) %>% 
  filter(UNIT_TYPE == "Gaunpalika") %>% 
  arrange(desc(unused)) %>% 
  slice_head(n = 5)

top5iresourceused <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,resource_used) %>% 
  mutate(unused = 1-resource_used) %>% 
  filter(UNIT_TYPE == "Nagarpalika" | UNIT_TYPE == "Upamahanagarpalika" | UNIT_TYPE == "Mahanagarpalika") %>% 
  arrange(desc(resource_used)) %>% 
  slice_head(n = 5)

rm(top5iresourceused)
# Plotting in map
merged_data_shape <- merged_data_shape %>%
  mutate(`resource used score` = ifelse(is.na(`resource used score`), 0, `resource used score`))
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = as.factor(`resource used score`))) +
  scale_fill_manual(name = "Score",
                    values = c("0" = "#ffffff",
                               "1" = "#cecece",
                               "2" = "#9d9d9d", 
                               "3" = "#6d6d6d", 
                               "4" = "#3c3c3c", 
                               "5" = "#0b0b0b"),
                    labels = c("1" = "Score: 1", "2" = "Score: 2",
                               "3" = "Score: 3", "4" = "Score: 4", "5" = "Score: 5",
                               "0" = "Conservation area \nor missing audit reports"))+
  theme_minimal() +
  ggtitle("Index for Funds used as a share of total available funds")+
  labs(caption = "Higher the score, the better the local level is in terms of resource use.")


#3.2 Internal Revenue Generation
top5irevgen <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME, `internal_rev_capacity`) %>%
  filter(UNIT_TYPE == "Gaunpalika") %>% 
  arrange(`internal_rev_capacity`) %>% 
  slice_head(n = 5)

top5irevgen <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,`internal_rev_capacity`) %>% 
  filter(UNIT_TYPE == "Nagarpalika" | UNIT_TYPE == "Upamahanagarpalika" | UNIT_TYPE == "Mahanagarpalika") %>% 
  arrange(`internal_rev_capacity`) %>% 
  slice_head(n = 5)

rm(top5irevgen)

merged_data_shape <- merged_data_shape %>%
  mutate(`Score for Internal Revenue Generating Capacity` = ifelse(is.na(`Score for Internal Revenue Generating Capacity`), 0, 
                                        `Score for Internal Revenue Generating Capacity`))
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = as.factor(`Score for Internal Revenue Generating Capacity`))) +
  scale_fill_manual(name = "Score",
                    values = c("0" = "#ffffff",
                               "1" = "#cecece",
                               "2" = "#9d9d9d", 
                               "3" = "#6d6d6d", 
                               "4" = "#3c3c3c", 
                               "5" = "#0b0b0b"),
                    labels = c("1" = "Score: 1", "2" = "Score: 2",
                               "3" = "Score: 3", "4" = "Score: 4", "5" = "Score: 5",
                               "0" = "Conservation area \nor missing audit reports"))+
  theme_minimal() +
  ggtitle("Index for Internal Revenue Generating Capacity as a share of total Income")+
  labs(caption = "Higher the score, the better the local level is in terms of Revenue Generation.")

# 3.3 Capital Expenditure

top5capexp <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME, Capital_exp_share) %>%
  filter(UNIT_TYPE == "Gaunpalika") %>% 
  arrange(Capital_exp_share) %>% 
  slice_head(n = 5)

top5capexp <- merged_data_shape %>% 
  select(DISTRICT, UNIT_TYPE, UNIT_NAME,Capital_exp_share) %>% 
  filter(UNIT_TYPE == "Nagarpalika" | UNIT_TYPE == "Upamahanagarpalika" | UNIT_TYPE == "Mahanagarpalika") %>% 
  arrange(Capital_exp_share) %>% 
  slice_head(n = 5)

rm(top5capexp)

merged_data_shape <- merged_data_shape %>%
  mutate(`Score for Capital Expenditure as share of total expenditure` = ifelse(is.na(`Score for Capital Expenditure as share of total expenditure`), 0, 
                                                                   `Score for Capital Expenditure as share of total expenditure`))
ggplot(data = merged_data_shape) +
  geom_sf(aes(fill = as.factor(`Score for Capital Expenditure as share of total expenditure`))) +
  scale_fill_manual(name = "Score",
                    values = c("0" = "#ffffff",
                               "1" = "#cecece",
                               "2" = "#9d9d9d", 
                               "3" = "#6d6d6d", 
                               "4" = "#3c3c3c", 
                               "5" = "#0b0b0b"),
                    labels = c("1" = "Score: 1", "2" = "Score: 2",
                               "3" = "Score: 3", "4" = "Score: 4", "5" = "Score: 5",
                               "0" = "Conservation area \nor missing audit reports"))+
  theme_minimal() +
  ggtitle("Score for Capital Expenditure as a share of total Expenditure")+
  labs(caption = "Higher the score, the better the local level is in terms of Capital Expenditure.")




#--------------------4. Public Service Delivery---------------------------------

#No. of municipalities who have uploaded nagarik wadapatra in their website out of total

# Create the plot (Plot: Public Service delivery 1)



mean_value <- merged_data_shape%>% 
  select(DISTRICT, UNIT_TYPE, nagarikwadapatra)

total_value <- mean_value %>%
  filter(UNIT_TYPE != "National Park" & UNIT_TYPE != "Wildlife Reserve" ) %>% 
  mutate(UNIT_TYPE = case_when(
    UNIT_TYPE %in% c("Mahanagarpalika", "Upamahanagarpalika", "Nagarpalika") ~ "Municipality",
    UNIT_TYPE %in% "Gaunpalika" ~ "Rural_municipality",
    TRUE ~ UNIT_TYPE
  )) %>% 
  group_by(DISTRICT, UNIT_TYPE) %>% 
  summarise(Total= n(), .groups = "drop") %>% 
  st_drop_geometry()

actual_value <- mean_value %>% 
  filter(nagarikwadapatra > 0) %>% 
  mutate(UNIT_TYPE = case_when(
    UNIT_TYPE %in% c("Mahanagarpalika", "Upamahanagarpalika", "Nagarpalika") ~ "Municipality",
    UNIT_TYPE %in% "Gaunpalika" ~ "Rural_municipality",
    TRUE ~ UNIT_TYPE
  )) %>% 
  group_by(DISTRICT, UNIT_TYPE) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  st_drop_geometry()

final_value <- full_join(actual_value, total_value, by = c("DISTRICT", "UNIT_TYPE"))

ggplot(final_value, aes(x = DISTRICT, fill = UNIT_TYPE)) +
  # Background bars for Total (separate for each UNIT_TYPE)
  geom_bar(aes(y = Total, fill = UNIT_TYPE), stat = "identity", 
           position = position_dodge(width = 0.7), alpha = 0.3) +  
  # Foreground bars for Count (separate for each UNIT_TYPE)
  geom_bar(aes(y = Count, fill = UNIT_TYPE), stat = "identity", 
           position = position_dodge(width = 0.7), color = "black") +  
  # Total values on top (one for each UNIT_TYPE)
  geom_text(aes(y = Total, label = Total), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, color = "black", size = 4) +  
  # Count values inside bars (one for each UNIT_TYPE)
  geom_text(aes(y = Count, label = Count), 
            position = position_dodge(width = 0.7), 
            vjust = 1.5, color = "white", size = 4) +  
  # Custom fill colors and labels
  scale_fill_manual(values = c("Rural_municipality" = "#1C0F0F", "Municipality" = "#A3A3A3"),
                    labels = c("Municipality" = "Municipality, metro \n and sub-metro",
                               "Rural_municipality" = "Rural Municipality")) +
  theme_minimal() +
  labs(
    title = "Number of Local Levels that have uploaded Nagarik Wadapatra (District-wise)",
    x = "District",
    y = "Number of Local Levels",
    fill = "Municipality Type",
    caption = "Lighter bars represent total local levels.\nDarker bars represent local levels that have uploaded the nagarik wadapatra\nWhite numbers indicate these counts."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------Annex--------------------------------------

finaldataframe<- merged_data_shape %>% 
  select("DISTRICT", "UNIT_TYPE", "UNIT_NAME", "total_transparency_score", "total_public_service_delivery",
         "Score for Capital Expenditure as share of total expenditure", 
         "irregularity score" , "resource used score", "Score for Internal Revenue Generating Capacity",
         "Score for Capital Expenditure as share of total expenditure") %>% 
  st_drop_geometry() %>% 
  mutate(`Complaints score` = 0)

write_xlsx(finaldataframe, "Annex.xlsx")




#Done*****************************************
##Resource use out of overall available resources
##Revenue generating capacity of local levels
## Capital expenditure as share of total expenditure
#convert the scores in 0,1 terms for transparency scores

#Remaining***********
#merge population data to get per-capita scores ()
#Find the shape for for local levels of MAdhesh province
#Create vizualization

