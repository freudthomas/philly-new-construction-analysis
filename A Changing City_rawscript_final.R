#--------------------------
# Freud Thomas 
# GAFL 531 
# A Changing City
# Final Project Script Raw Script
#---------------------------

#----------------
# Final App Link
#----------------

# final app can be found here : https://freudthomas.shinyapps.io/a_changing_city/

#------------------
# load libraries 
#------------------
library(tidyverse)
library(lubridate)
library(scales)
library(leaflet)
library(rgdal)
library(tidycensus)
library(data.table)
library(DT)

# -----------------
# Data Processing 
#------------------

# https://www.opendataphilly.org/dataset/licenses-and-inspections-building-permits
permits_data_raw <- read_csv("permits_data.csv")

dim(permits_data)

permits_data <- permits_data_raw %>% 
  mutate(permitissue_year = year(permitissuedate),
         permitissue_month = month(permitissuedate)) %>% 
  filter(permitissue_year %in% c(2019,2020,2021))



# create month and year column
permits_data$permitissue_month_year <- format(as.Date(permits_data$permitissuedate), "%m-%Y")

#saveRDS(permits_data, file)

#--------------------------------
# Plot 1 - Permits issued by year 
#--------------------------------


permits_data_counts <- permits_data %>% 
  group_by(permitissue_month_year) %>% 
  summarise(count = n()) 

#permits_data_counts$permitissue_month_year <- format(as.Date(permits_data_counts$permitissue_month_year), "%m-%Y")

# convert column to date for plotting 
permits_data_counts$permitissue_month_year2 <- parse_date_time(permits_data_counts$permitissue_month_year, "my")

permit_data_plot_permits_over_time <-  permits_data_counts %>% 
  ggplot(mapping = aes(x = permitissue_month_year2, y = count, group = 1)) +
  geom_line(color = "#003e82") +
  geom_area(fill = "#0b6cd4", alpha=.09) +
 # geom_point(size=1, color="#69b3a2")+
  theme_classic() + 
  labs(x = '',
       y = "Total Permits Issued") + 
  theme(panel.grid.major.y = element_line( size=.1, color="grey")) 

permit_data_plot_permits_over_time

#-------------------------
# Plot 2 - Top 5 Permits 
#-------------------------
  
permit_description_table <- table(permits_data$permitdescription)

permit_description_table_df <- as.data.frame(permit_description_table)

permit_description_table_df_top_5 <- permit_description_table_df %>% 
  arrange(desc(Freq)) %>% 
  slice(1:5) # Choosing to graph the top 5 permits 

# Chnage X axis labels to title case for ease of reading 
permit_description_table_df_top_5$Var1_title <- str_to_title(permit_description_table_df_top_5$Var1)

# create plot 

top5_permits_plot <- permit_description_table_df_top_5   %>% 
  ggplot() +
  geom_bar(mapping = aes( x= reorder(Var1_title,-Freq), y = Freq), stat = "identity", fill = "#084c96") + 
  theme_classic() +
  labs(x = "Permit Description",
       y = "Count") + 
  theme(panel.grid.major.y = element_line( size=.1, color="grey")) 
  

top5_permits_plot 


#-----------------------------------------
# Residential construction Data Processing
#-----------------------------------------


# Excluding commercial properties 
permits_data <-  permits_data %>% 
  filter(commercialorresidential != "COMMERCIAL" | is.na(commercialorresidential))
  

table(permits_data$typeofwork)

permits_data <-  permits_data %>% 
  filter( !(permitdescription %in% c("FIRE SUPPRESSION PERMIT","SIGN PERMIT", "MECHANICAL / FUEL GAS PERMIT", "SUPPRESSION PERMIT", "MECHANICAL PERMIT",
                                     "DEMOLITION PERMIT", "ADMINISTRATIVE PERMIT", "PRELIMINARY APPROVAL", "PLUMBING PERMIT", "ELECTRICAL PERMIT" )))
permits_data <-  permits_data %>% 
  filter(typeofwork %in% c("NEWCON", "ENTIRE", "NEW CONSTRUCTION", "NEW CONSTRUCTION, ADDITION, GFA CHANGE", 
                           "NEW CONSTRUCTION (STAND ALONE)"))

# Test for coverage 
sum(str_count(permits_data$approvedscopeofwork, "FAMILY"), na.rm = TRUE) / dim(permits_data)[1]
#66% of the observation mention the word "Family" 
# The approved scope of work variable usually mentioned "For use as a multi/single-family household living" indicating the
# new construction if for residential properties. 

permits_data_coverage <- permits_data %>%  
  filter(!(str_detect(approvedscopeofwork, "FAMILY")))
# for the observations without the word family --- a look at the data showed that many of these permits were also for the construction activity  
# residential properties 



# Zip codes
table(permits_data$zip)

# Using REGEX to turn zip codes to 5 digit zip codes 
# replace anything after "-" with blank 
permits_data$zip_code <- str_replace(permits_data$zip, pattern = "\\-[^-]*$", "")


#------------------------------------------------
# Function 1 - Find Top 5 of Categorical Variable 
#------------------------------------------------

top_5_categorical <- function(x){
  top_5_table <- table(x)
  top_5_table_df <- as.data.frame(top_5_table)
  top_5_table_result <- top_5_table_df %>% 
    arrange(desc(Freq)) %>% 
    slice(1:5)
  return(top_5_table_result)
}

#---------------------------------
# Plot 3 - Top 5 Zip Codes Permits 
#---------------------------------

top5_permits_res_zip_code <- top_5_categorical(permits_data$zip_code)

top5_permits_zip_code <- top5_permits_res_zip_code %>% 
  ggplot() +
  geom_bar(mapping = aes( x= reorder(x,-Freq), y = Freq), stat = "identity", fill = "#084c96") + 
  theme_classic() +
  labs(x = "Zip code",
       y = "Count") + 
  theme(panel.grid.major.y = element_line( size=.1, color="grey")) 


top5_permits_zip_code


#----------------------------------
# Geographic Analysis 1 - Permits
#----------------------------------

permits_data_dotmap <- permits_data %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   weight = .5,
                   radius = .3,
                   color = "#084c96",
                   fillOpacity = .7,
                   popup = paste("Address:", permits_data$address)) %>% 
  setView(lng = -75.112 , lat = 40.001, zoom = 11.4)
permits_data_dotmap

#----------------------------------------------
# Property Assessment Data Merge and Processing 
#----------------------------------------------

#https://www.opendataphilly.org/dataset/opa-property-assessments
opa_assessment <- read_csv("opa_properties_public.csv") 

opa_assessment <- opa_assessment %>% 
  select(objectid,assessment_date,building_code_description, category_code_description, census_tract,
         location,market_value,market_value_date, parcel_number,recording_date,registry_number,
         sale_date,sale_price,year_built,zip_code,lat,lng)

length(unique(permits_data$opa_account_num)) 
# OPA account number identifies the property to the owner, but many permits can go under the same OPA number
# if the property consists of many units 

length(unique(opa_assessment$parcel_number)) #parcel_number is unique ID


# inner join merged permits data with property assessment data 
permits_assessment_merge <- inner_join(permits_data, opa_assessment, by= c("opa_account_num" = "parcel_number"))

# remove observations unlikely to be residential properties 
permits_assessment_merge <- permits_assessment_merge %>%  
  filter(!(str_detect(approvedscopeofwork, "HOTEL|INDUSTRIAL"))) %>% 
  filter(!(category_code_description == "Commercial"))

summary(permits_assessment_merge$market_value)
summary(permits_assessment_merge$sale_price)

# Sale price and market value variables can be used to estimate the value of the address but there are issues
# Many of the sale prices are low (i.e $1 or $10). It is not know if this due to data errors or the fact that 
# the property is being transferred and is sold at very low price. 
# To find an estimated value, I find the parallel maxima which choose the highest value between market value or sale price.
# This is also helpful because many of the construction are based on vacant land that may have not have a reliable estimate value
# of the building that is to be constructed. 

permits_assessment_merge <- permits_assessment_merge %>% 
  mutate(estimated_value_pmax = pmax(sale_price,market_value, na.rm = TRUE))

#saveRDS(permits_assessment_merge, "permits_assessment_merge_appfile.RDS")


summary(permits_assessment_merge$estimated_value_pmax)


top_5_categorical(permits_assessment_merge$opa_account_num) #come back to this{are the people with the most permits, highest value?}


#---------------------------------------------------
# Geographic Analysis 2 - Permits by estimated value 
#---------------------------------------------------


permits_data_dotmap_value <- permits_assessment_merge %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~lng.x,
                   lat = ~lat.x,
                   weight = .5,
                   radius = ~  sqrt(estimated_value_pmax)/650,
                   color = "#084c96",
                   fillOpacity = .2,
                   popup = paste("Address:", permits_assessment_merge$address,
                                 "Value:",permits_assessment_merge$estimated_value_pmax, 
                                 "Scope:",permits_assessment_merge$approvedscopeofwork)) %>% 
  setView(lng = -75.112 , lat = 40.001, zoom = 11.4)

permits_data_dotmap_value

#sqrt(estimated_value_pmax) / logn( 2, estimated_value_pmax ) * 0.5

#----------------------------------
# Data table permits and assessment 
#----------------------------------


#-----------------------------------------------
#  Data table permits and assessment -- Reactive
#----------------------------------------------- 

# This section of code defines a reactive that will allow us to filter
# the data based on party affiliation
# A reactive takes input from your ui and can store that information in 
# memory so that it can be passed to render functions
# permits_assessment_merge_FILT <- reactive({
#   if (input$permit_value_merge_DT != "All")
#   {
#     permits_assessment_merge <- filter(permits_assessment_merge, zip_code.x == input$permit_value_merge_DT)
#   }
#   else {
#     permits_assessment_merge
#   }
# })

#-------------------
# Data Table
#-------------------

# Creating a  data table
# output$permits_data_merge_datatable <- DT::renderDataTable(DT::datatable({
#   # Choosing which column to show
#   data <- permits_assessment_merge[, c("opa_account_num","address","opa_owner","permitissue_month_year","zip_code.x","estimated_value_pmax")]
#   
#   # Filter data based on zip code 
#   if (input$permit_value_merge_DT != "All") {
#     data <- data[data$zip_code.x == input$permit_value_merge_DT, ]
#   }
#   data
# },
# # renaming columns in table
# colnames = c("opa_account_num","address","opa_owner","permitissue_month_year","zip_code.x","estimated_value_pmax")))




# ---------------------------
# Census Data 
#----------------------------

census_api_key("6d70df4d118161b05728fd0c54e6ae0507758614", install = TRUE)

#ACS 5 year estimate Data from the 2016-2020 5-year ACS
# Load variables 
# acs_vars <- load_variables(2020, "acs5", cache = TRUE)
# View(acs_vars)

#------ Median income by ZCTA in Philly which is roughly equal to ZIP CODEs

med_hh_income_zip_philly <- get_acs(
  geography = "zcta",
  variables = c(median_income ="B19013_001"),
  year = 2020,
  output = "wide") %>% 
filter(str_detect(GEOID,"^191"))

#----- Median Home Value 

med_home_value_zip_philly <- get_acs(
  geography = "zcta",
  variables = c(median_home_value="B25077_001"),
  year = 2020,
  output = "wide") %>% 
  filter(str_detect(GEOID,"^191"))

#-------- Poverty Status 

poverty_status_zip_philly <- get_acs(
  geography = "zcta",
  variables = c(poverty_status_available ="B17001_001", below_poverty_line = "B17001_002"),
  year = 2020,
  output = "wide") %>% 
  filter(str_detect(GEOID,"^191")) %>% 
  mutate(poverty_rate_perc =  (below_poverty_lineE / poverty_status_availableE) *100)

#---------- Unemployment Rate 
# B23025003 -- civilian labor force, excludes members of the Armed Forces 

unemployment_rate_zip_philly <- get_acs(
  geography = "zcta",
  variables = c(labor_force ="B23025_003", unemployed = "B23025_005"),
  year = 2020,
  output = "wide" ) %>% 
  filter(str_detect(GEOID,"^191")) %>% 
  mutate(unemployment_rate_perc =  (unemployedE / labor_forceE) *100)


#----------- Racial Minorities 

#B01003_0011- total population
#B03002_003- Estimate!!Total:!!Not Hispanic or Latino:!!White alone

minority_rate_zip_philly <- get_acs(
  geography = "zcta",
  variables = c(white_non_hispanic ="B03002_003", total_population = "B01003_001"),
  year = 2020,
  output = "wide" ) %>% 
  filter(str_detect(GEOID,"^191")) %>% 
  mutate(minority_perc = (1 - (white_non_hispanicE / total_populationE)) * 100)
  
#---------------- Merge all the ACS data frames into one variable 

acs_zip_dfs <- list(med_hh_income_zip_philly, med_home_value_zip_philly, poverty_status_zip_philly,
                    unemployment_rate_zip_philly, minority_rate_zip_philly)

acs_zip_dfs_merge_raw <- reduce(acs_zip_dfs, full_join, by = "GEOID")

acs_zip_dfs_merge <- acs_zip_dfs_merge_raw %>% 
  select(GEOID,NAME,median_incomeE,median_home_valueE,poverty_rate_perc,unemployment_rate_perc,
         minority_perc,total_populationE) %>% 
  filter(!(GEOID == 19113 | GEOID == 19109)) #19113 ZCTA for only the PHL Airport, 19109 Zip code for single building


#saveRDS(acs_zip_dfs_merge, "acs_zip_dfs_merge_appfile.RDS")

#--------------------------------------------------
# Mapping Variables of Interest-- Unemployment rate 
#-------------------------------------------------

# Create quantiles for each variable 

qt <- quantile(acs_zip_dfs_merge$unemployment_rate_perc, probs=c(0,.20,.40,.60,.80,1),na.rm = TRUE)

acs_zip_dfs_merge$unemployment_rateqt <- cut(acs_zip_dfs_merge$unemployment_rate_perc, breaks = qt, include.lowest = TRUE)

table(acs_zip_dfs_merge$unemployment_rateqt)

acs_zip_dfs_merge$unemployment_level <- NA 
acs_zip_dfs_merge$unemployment_level[acs_zip_dfs_merge$unemployment_rateqt == "[2.21,4.5]" ] <- "Lowest"
acs_zip_dfs_merge$unemployment_level[acs_zip_dfs_merge$unemployment_rateqt == "(4.5,6.48]" ] <- "Low"
acs_zip_dfs_merge$unemployment_level[acs_zip_dfs_merge$unemployment_rateqt == "(6.48,10.1]" ] <- "Moderate"
acs_zip_dfs_merge$unemployment_level[acs_zip_dfs_merge$unemployment_rateqt == "(10.1,12.7]" ] <- "High"
acs_zip_dfs_merge$unemployment_level[acs_zip_dfs_merge$unemployment_rateqt == "(12.7,17.6]" ] <- "Highest"


# Convert shapefile to spatial polygon dataframe 
philly_zipcode_poly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")

#you need to interact with the shapefile which creates a special object 
#called  a spatial polygon dataframe 

# converting to dataframe (method without using merge functions)
philly_zipcode_poly@data <- data.frame(philly_zipcode_poly@data, acs_zip_dfs_merge[match(philly_zipcode_poly@data$CODE, acs_zip_dfs_merge$GEOID),])


#------ Unemployment choropleth

# Color factor light to dark (1 lightest, 5 darkest)

col_fact_1 <- "#cce8ff"
col_fact_2 <- "#89c4ff"
col_fact_3 <- "#187cc9"
col_fact_4 <- "#003670"
col_fact_5 <- "#001429"
#c("#FFCCE9","#E29BF4", "#A13CC2", "#8C0AA9","#4D0180")

factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$unemployment_level)

zip_popup <- paste("<strong>ZIP: </strong>", 
                   philly_zipcode_poly@data$CODE, 
                   "<br>Unemployment:", 
                   philly_zipcode_poly@data$unemployment_level)

map_unemployment_permits <- leaflet(philly_zipcode_poly) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2, 
              fillOpacity = .88,
              color = ~factpal(unemployment_level),
              weight = 1,
              popup = zip_popup) %>%
  addCircleMarkers(data = permits_assessment_merge,
                   lng = ~lng.x,
                   lat = ~lat.x,
                   weight = .5,
                   radius = ~  sqrt(estimated_value_pmax)/650,
                   color = "#e35c0e",
                   fillOpacity = .4,
                   popup = paste("Address:", permits_assessment_merge$address,
                                 "Value:",permits_assessment_merge$estimated_value_pmax, 
                                 "Scope:",permits_assessment_merge$approvedscopeofwork)) %>% 
  addLegend("bottomright", 
            colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
            labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
            title = "Unemployment level",
            opacity = 1) 
map_unemployment_permits
table(acs_zip_dfs_merge$unemployment_level)

#--------------------------------------------------
# Mapping Variables of Interest-- Poverty rate 
#-------------------------------------------------

# Create quantiles for each variable 

qt <- quantile(acs_zip_dfs_merge$poverty_rate_perc, probs=c(0,.20,.40,.60,.80,1),na.rm = TRUE)

acs_zip_dfs_merge$poverty_rateqt <- cut(acs_zip_dfs_merge$poverty_rate_perc, breaks = qt, include.lowest = TRUE)

table(acs_zip_dfs_merge$poverty_rateqt)

acs_zip_dfs_merge$poverty_level <- NA 
acs_zip_dfs_merge$poverty_level[acs_zip_dfs_merge$poverty_rateqt == "[5.98,10]" ] <- "Lowest"
acs_zip_dfs_merge$poverty_level[acs_zip_dfs_merge$poverty_rateqt == "(10,16.3]" ] <- "Low"
acs_zip_dfs_merge$poverty_level[acs_zip_dfs_merge$poverty_rateqt == "(16.3,20.9]" ] <- "Moderate"
acs_zip_dfs_merge$poverty_level[acs_zip_dfs_merge$poverty_rateqt == "(20.9,31.1]" ] <- "High"
acs_zip_dfs_merge$poverty_level[acs_zip_dfs_merge$poverty_rateqt == "(31.1,44.5]" ] <- "Highest"

# # Convert shapefile to spatial polygon dataframe 
 philly_zipcode_poly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
# 
# #you need to interact with the shapefile which creates a special object 
# #called  a spatial polygon dataframe 
# 
# # converting to dataframe (method without using merge functions)
 philly_zipcode_poly@data <- data.frame(philly_zipcode_poly@data, acs_zip_dfs_merge[match(philly_zipcode_poly@data$CODE, acs_zip_dfs_merge$GEOID),])


#------ Poverty choropleth

# Color factor light to dark (1 lightest, 5 darkest)

col_fact_1 <- "#cce8ff"
col_fact_2 <- "#89c4ff"
col_fact_3 <- "#187cc9"
col_fact_4 <- "#003670"
col_fact_5 <- "#001429"
#c("#FFCCE9","#E29BF4", "#A13CC2", "#8C0AA9","#4D0180")

factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$unemployment_level)

zip_popup <- paste("<strong>ZIP: </strong>", 
                   philly_zipcode_poly@data$CODE, 
                   "<br>Poverty:", 
                   philly_zipcode_poly@data$poverty_level)

map_poverty_permits <- leaflet(philly_zipcode_poly) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2, 
              fillOpacity = .88,
              color = ~factpal(poverty_level),
              weight = 1,
              popup = zip_popup) %>%
  addCircleMarkers(data = permits_assessment_merge,
                   lng = ~lng.x,
                   lat = ~lat.x,
                   weight = .5,
                   radius = ~  sqrt(estimated_value_pmax)/650,
                   color = "#e35c0e",
                   fillOpacity = .4,
                   popup = paste("Address:", permits_assessment_merge$address,
                                 "Value:",permits_assessment_merge$estimated_value_pmax, 
                                 "Scope:",permits_assessment_merge$approvedscopeofwork)) %>% 
  addLegend("bottomright", 
            colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
            labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
            title = "Poverty level",
            opacity = 1) 
map_poverty_permits

#--------------------------------------------------
# Mapping Variables of Interest-- Minority rate 
#-------------------------------------------------

# Create quantiles for each variable 

qt <- quantile(acs_zip_dfs_merge$minority_perc, probs=c(0,.20,.40,.60,.80,1),na.rm = TRUE)

acs_zip_dfs_merge$minorityqt <- cut(acs_zip_dfs_merge$minority_perc, breaks = qt, include.lowest = TRUE)

table(acs_zip_dfs_merge$minorityqt)

acs_zip_dfs_merge$minority_level <- NA 
acs_zip_dfs_merge$minority_level[acs_zip_dfs_merge$minorityqt == "[15.3,31.8]" ] <- "Lowest"
acs_zip_dfs_merge$minority_level[acs_zip_dfs_merge$minorityqt == "(31.8,48.9]" ] <- "Low"
acs_zip_dfs_merge$minority_level[acs_zip_dfs_merge$minorityqt == "(48.9,69.3]" ] <- "Moderate"
acs_zip_dfs_merge$minority_level[acs_zip_dfs_merge$minorityqt == "(69.3,92.9]" ] <- "High"
acs_zip_dfs_merge$minority_level[acs_zip_dfs_merge$minorityqt == "(92.9,97.5]" ] <- "Highest"

# # Convert shapefile to spatial polygon dataframe 
philly_zipcode_poly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
# 
# #you need to interact with the shapefile which creates a special object 
# #called  a spatial polygon dataframe 
# 
# # converting to dataframe (method without using merge functions)
philly_zipcode_poly@data <- data.frame(philly_zipcode_poly@data, acs_zip_dfs_merge[match(philly_zipcode_poly@data$CODE, acs_zip_dfs_merge$GEOID),])


#------ Minority choropleth

# Color factor light to dark (1 lightest, 5 darkest)

col_fact_1 <- "#cce8ff"
col_fact_2 <- "#89c4ff"
col_fact_3 <- "#187cc9"
col_fact_4 <- "#003670"
col_fact_5 <- "#001429"
#c("#FFCCE9","#E29BF4", "#A13CC2", "#8C0AA9","#4D0180")

factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$minority_level)

zip_popup <- paste("<strong>ZIP: </strong>", 
                   philly_zipcode_poly@data$CODE, 
                   "<br>Minority:", 
                   philly_zipcode_poly@data$minority_level)

map_minority_permits <- leaflet(philly_zipcode_poly) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2, 
              fillOpacity = .88,
              color = ~factpal(minority_level),
              weight = 1,
              popup = zip_popup) %>%
  addCircleMarkers(data = permits_assessment_merge,
                   lng = ~lng.x,
                   lat = ~lat.x,
                   weight = .5,
                   radius = ~  sqrt(estimated_value_pmax)/650,
                   color = "#e35c0e",
                   fillOpacity = .4,
                   popup = paste("Address:", permits_assessment_merge$address,
                                 "Value:",permits_assessment_merge$estimated_value_pmax, 
                                 "Scope:",permits_assessment_merge$approvedscopeofwork)) %>% 
  addLegend("bottomright", 
            colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
            labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
            title = "Minority level",
            opacity = 1) 
map_minority_permits





