#-----------------------------
# Freud Thomas
# GAFL 531
# A Changing Philly App script 
#-----------------------------

#----------------
# Final App Link
#----------------

# Final app can be found here : https://freudthomas.shinyapps.io/a_changing_city/

#------------------
# Load libraries 
#------------------
library(tidyverse)
library(lubridate)
library(leaflet)
library(rgdal)
library(shiny)
library(shinythemes)
library(plotly)
library(data.table)
library(DT)
library(rsconnect)

#-------------------------
# Data load and processing 
#-------------------------
permits_data <- readRDS("permits_data_appfile.rds")

# create month and year column
permits_data$permitissue_month_year <- format(as.Date(permits_data$permitissuedate), "%m-%Y")

#load permits data merged with assessment data 
permits_assessment_merge <- readRDS("permits_assessment_merge_appfile.RDS")

#Convert number to human friendly format 
permits_assessment_merge$estimated_value_pmax_new <- prettyNum(permits_assessment_merge$estimated_value_pmax, big.mark = ",", scientific = FALSE)

# load 2016-2020 5 year ACS 
acs_zip_dfs_merge <- readRDS("acs_zip_dfs_merge_appfile.RDS")


server <- shinyServer(function(input,output) {
  
  
  #--------------------------------
  # Plot 1 - Permits issued by year 
  #--------------------------------
  
  permits_data_counts <- permits_data %>% 
    group_by(permitissue_month_year) %>% 
    summarise(count = n()) 
  
  # convert column to date for plotting 
  #The date automatically applies to the first month of the year
  permits_data_counts$`Permit Issue Date` <- parse_date_time(permits_data_counts$permitissue_month_year, "my")
  
  permit_data_plot_permits_over_time <-  permits_data_counts %>% 
    ggplot(mapping = aes(x = `Permit Issue Date`, y = count, group = 1)) +
    geom_line(color = "#003e82") +
    geom_area(fill = "#0b6cd4", alpha=.09) +
    # geom_point(size=1, color="#69b3a2")+
    theme_classic() + 
    labs(x = '',
         y = "Total Permits Issued") + 
    theme(panel.grid.major.y = element_line( size=.1, color="grey")) 
  
   ggplotly(permit_data_plot_permits_over_time) 
  
  
  output$plot_permits_peryear <- renderPlotly({permit_data_plot_permits_over_time}) 
  
  
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
  
  
   output$plot_permits_top5_description <- renderPlot({top5_permits_plot})
   
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
   
   output$top5_permits_zip_code <- renderPlot({top5_permits_zip_code})
   
   
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
   
  output$permits_data_dotmap <- renderLeaflet({permits_data_dotmap})
  
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
                      popup = paste("<strong>Address:</strong>", permits_assessment_merge$address,
                                    "<strong><br>Value:</strong>",permits_assessment_merge$estimated_value_pmax_new, 
                                    "<strong><br>Scope:</strong>",permits_assessment_merge$approvedscopeofwork)) %>% 
     setView(lng = -75.128 , lat = 39.981, zoom = 11.6)
  permits_data_dotmap_value
  
  output$permits_data_dotmap_value <- renderLeaflet({permits_data_dotmap_value})
  
  
  #-------------------
  # Data Table
  #-------------------
  
  # Creating a  data table
  output$permits_data_merge_datatable <- DT::renderDataTable(DT::datatable({
     # Choosing which column to show
     data <- permits_assessment_merge[, c("opa_account_num","address","opa_owner","permitissue_month_year","zip_code.x","estimated_value_pmax","approvedscopeofwork")]
     
     # Filter data based on zip code 
     if (input$permit_value_merge_DT != "All") {
        data <- data[data$zip_code.x == input$permit_value_merge_DT, ]
     }
     data
  },
  # renaming columns in table
  colnames = c("OPA Account Number","Address","OPA Owner","Permit Issue Date","Zip Code","Estimated Value","Scope of Work")))
  
  
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
  
  #interact with the shapefile which creates a special object 
  #called a spatial polygon dataframe 
  
  # converting to dataframe (method without using merge functions)
  philly_zipcode_poly@data <- data.frame(philly_zipcode_poly@data, acs_zip_dfs_merge[match(philly_zipcode_poly@data$CODE, acs_zip_dfs_merge$GEOID),])
  
  
  #------ Unemployment choropleth
  
  # Color factor light to dark (1 lightest, 5 darkest)
  
  col_fact_1 <- "#cce8ff"
  col_fact_2 <- "#89c4ff"
  col_fact_3 <- "#187cc9"
  col_fact_4 <- "#003670"
  col_fact_5 <- "#001429"
  
  factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$unemployment_level)
  
  zip_popup <- paste("<strong>ZIP: </strong>", 
                     philly_zipcode_poly@data$CODE, 
                     "<strong><br>Unemployment:</strong>", 
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
                     popup = paste("<strong>Address</strong>:", permits_assessment_merge$address,
                                   "<strong><br>Value:</strong>",permits_assessment_merge$estimated_value_pmax_new, 
                                   "<strong><br>Scope:</strong>",permits_assessment_merge$approvedscopeofwork)) %>% 
    addLegend("bottomright", 
              colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
              labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
              title = "Unemployment level",
              opacity = 1)  %>% 
    setView(lng = -75.128 , lat = 39.993, zoom = 10.6)
    
  
  output$map_unemployment_permits <- renderLeaflet({map_unemployment_permits})
  
  
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
  
  factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$poverty_level)
  
  zip_popup <- paste("<strong>ZIP: </strong>", 
                     philly_zipcode_poly@data$CODE, 
                     "<strong><br>Poverty:</strong>", 
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
                     popup = paste("<strong>Address:</strong>", permits_assessment_merge$address,
                                   "<strong><br>Value:</strong>",permits_assessment_merge$estimated_value_pmax_new, 
                                   "<strong><br>Scope:</strong>",permits_assessment_merge$approvedscopeofwork)) %>% 
    addLegend("bottomright", 
              colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
              labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
              title = "Poverty level",
              opacity = 1)  %>% 
    setView(lng = -75.128 , lat = 39.993, zoom = 10.6)
  
  
  output$map_poverty_permits <- renderLeaflet({map_poverty_permits})
  
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
  
  factpal <- colorFactor(c(col_fact_4,col_fact_5, col_fact_2, col_fact_1, col_fact_3),acs_zip_dfs_merge$minority_level)
  
  zip_popup <- paste("<strong>ZIP: </strong>", 
                     philly_zipcode_poly@data$CODE, 
                     "<strong><br>Minority:</strong>", 
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
                     popup = paste("<strong>Address:</strong>", permits_assessment_merge$address,
                                   "<strong><br>Value:</strong>",permits_assessment_merge$estimated_value_pmax_new, 
                                   "<strong><br>Scope:</strong>",permits_assessment_merge$approvedscopeofwork)) %>% 
    addLegend("bottomright", 
              colors = c(col_fact_1,col_fact_2,col_fact_3,col_fact_4,col_fact_5),
              labels = c("Lowest", "Low", "Moderate", "High", "Highest"),  
              title = "Minority level",
              opacity = 1) %>% 
    setView(lng = -75.128 , lat = 39.993, zoom = 10.6)
  output$map_minority_permits <- renderLeaflet({map_minority_permits}) 
  
})


ui <- shinyUI(fluidPage(theme = shinytheme("flatly"), 
                        tags$head(tags$style('h4 {font-family: Arial;}')),
                        tags$head(tags$style('h3 {font-family: Arial;}')),
                        navbarPage("A Changing City",
                                   tabPanel("Overview",
                                            headerPanel("A Changing City: Philadelphia"), 
                                            h3("Overview"),
                                            h4("A city’s landscape and built environment is constantly changing. Within the same block, there could be a construction of a new sidewalk, demolishing an old building, or building of a new skyscraper. 
                                               Getting a clear idea of construction activity in a city is often a challenge. This web app allows users to see which areas in Philadelphia are receiving the most construction activity. The app also allows analysis of construction activity by demographic factors such as unemployment, poverty, and minority rates."),
                                            h3("Data"),
                                            h4("The analysis utilized administrative data from the City of Philadelphia hosted on OpenDataPhilly. Data for permit activity is from the", a("Department of Licenses and Inspection", href = "https://www.opendataphilly.org/dataset/licenses-and-inspections-building-permits"), ". Data for property value originates from the" , 
                                               a("Office of Property Assessment", href = "https://www.opendataphilly.org/dataset/opa-property-assessments"),
                                               ". Demographic data for Philadelphia zip codes is from the", a("American Community Survey", href = "https://www.census.gov/newsroom/press-kits/2021/acs-5-year.html"),"(ACS) 5-year estimates from 2016-2020."),
                                            h3("Use Case and Limitations"),
                                            h4("The web app provides tools to explore and analyze areas receiving construction activity in the City. It provides a macro-level look at geographic construction patterns and site-specific information on what is being built. Demographic data can be helpful for equity analysis. 
                                               For limitations, efforts have been made to provide entirely accurate information, but the use of administrative data may pose challenges due to data quality issues.")),
                                     tabPanel("Permits",
                                              headerPanel("Analyzing Permit Activity"), 
                                              h4("The City of Philadelphia releases information about each building permit that is issued. Permits are required in the city for various activities such as construction, demolition, zoning, and many other activities that affect structures in the city.
                                                 Permit activity provides a preliminary look at construction and real estate activity."),
                                              plotlyOutput("plot_permits_peryear", width = "50%", height = "400px"),
                                              h4("Permit activity since 2019 shows an estimated 5000 permits issued per month. There is a steep drop at the onset of the Coronavirus pandemic, with the lowest number issued in April 2020. 
                                                 There is a rebound in permits issued in 2021, but the value remains below 2019 levels."),
                                              br(),
                                              plotOutput("plot_permits_top5_description", width = "50%", height = "400px" ),
                                              h4("The number of permits issued differs by the permit description. Plumbing and electrical permits lead the way as top permits issued by the city. This makes sense, given that many structures require installation, repair, and replacement of plumbing and electrical materials. 
                                                 Residential building permits are also common, with similar numbers as zoning and mechanical/ fuel gas permits.")),
                                      tabPanel("Residential Activity", 
                                               headerPanel("Residential Construction Activity"),
                                               h4("Residential construction activity is one of the primary ways the city landscape changes. From the construction of single-family homes to large multi-unit apartment buildings, residential building activity is shaping the future landscape of the city. One key question is where exactly is this construction activity occurring. 
                                                  Data from permits for new construction can provide a way of analyzing residential construction activity."),
                                               leafletOutput("permits_data_dotmap", width = "60%", height = "415px"),
                                               h4("A geographic analysis of the residential new construction permits shows some concentration of where new residential properties are being built. Areas in Kensington, lower North Philadelphia, and South Philadelphia receive many permits for new residential construction. Areas such as Northeast Philadelphia and Germantown have scant new residential construction.
                                                  These areas may have an adequate supply of homes already built, little vacant land, or there may be other factors limiting residential construction."),
                                               br(),
                                               h3("Zip Codes With Highest Permits Issued"),
                                               plotOutput("top5_permits_zip_code", width = "50%", height = "400px" ),
                                               h4("Looking at the number of new permits by zip code shows that some zip codes in the city are receiving more construction activity than others. Zip codes 19125 and 19146 are ahead with the most number of permits. 19122 and 19121 follow closely; 19145 is the lowest of the top 5 with a little over 500 permits for new construction.
                                                  This amount is less than half of the top two zip codes, which shows how much of a large share of new construction the two highest zip codes represent."),
                                               br(),
                                               h3("Examine Permit Activity with the Properties' Estimated Value"),
                                               h4("A simple analysis that maps the location of the permits can be inadequate. A single location, or a dot on the map, can represent a permit for the construction of a new house worth $200,000, or it can represent a permit for the construction of a multi-family unit worth multi-millions of dollars. The following map provides a map of the residential permits but scales the dot with the properties’ estimated value.
                                                  The map also provides a description of the permit."),
                                               leafletOutput("permits_data_dotmap_value", width = "80%", height = "615px"),
                                               br(),
                                               br(),
                                               h3("Examine a Zip Code of Interest"),
                                               h4("Search for new residential permits by zip code. The OPA account number is a 9-digit number from the City of Philadelphia Office of Property Assessment that identifies a property."),
                                               selectInput(
                                                  "permit_value_merge_DT",
                                                  "Zip Code:",
                                                  c(
                                                     "All", unique(permits_assessment_merge$zip_code.x)
                                                     
                                                  )),
                                               DT::dataTableOutput("permits_data_merge_datatable")
                                               
                                               ),
                                   tabPanel("Demographic Analysis", 
                                            headerPanel("Residential Construction Activity Demographic Analysis"),
                                            h4("A key motivation of this analysis was to examine if there are patterns in the places receiving or not receiving new construction activity. Factors such as economic strength/weakness and racial composition of an area likely affect new construction activity. These factors are featured in debates about economic development and gentrification. Some government", a("policies", href = "https://iiusa.org/resources-data/targeted-employment-areas"), 
                                               "aim to spur economic development in",a("economically depressed", href = "https://www.irs.gov/newsroom/opportunity-zones"), "areas. 
                                               Individuals and organizations may also want to see if there are any",a("racial", href = "https://www.arcgis.com/home/item.html?id=56de4edea8264fe5a344da9811ef5d6e"), "components to new housing developments. The following maps show new residential construction activity overlaid with information about the Philadelphia zip code’s unemployment, poverty, and racial minority rates."),
                                            h3("New Construction and Unemployment Rate"),
                                            leafletOutput("map_unemployment_permits", width = "60%", height = "515px"),
                                            br(),
                                            h3("New Construction and Poverty Rate"),
                                            leafletOutput("map_poverty_permits", width = "60%", height = "515px"),
                                            br(),
                                            h3("New Construction and Minority Rate"), 
                                            leafletOutput("map_minority_permits",width = "60%", height = "515px"),
                                            br(),
                                            h4("The maps above show economic and racial patterns in the geographic distribution of new residential construction activity. Construction activity is primarily concentrated in areas with lower unemployment rates, but there are examples of new construction in some of the  zip codes with the highest unemployment rates. Similarly, activity is focused in areas of low poverty. Thus, it seems that much of the new construction is in economically well-off areas. 
                                            This may be due to economic motives since the real estate market may be better in these areas.
                                            However, it may indicate the fact that economically distressed areas are not receiving much investment to build new homes. Nonetheless, there are some examples of high-value properties being built in economically distressed areas. Policymakers can thus dive deeper into whether these properties are affordable for the residents of these zip codes or examine if these new constructions are part of patterns of gentrification."), 
                                           h4(" The map for new construction and racial minority rates is more striking. Nearly all of the new construction is in zip codes with low and moderate percentages of racial minorities. This may be because economic and racial demographics mirror each other in the City, but it can also indicate racialized patterns in new housing construction. 
                                            More work would need to be done to uncover the drivers of these patterns and the roles these data points can have in debates about economic development and gentrification in the City of Philadelphia.")),
                                    tabPanel("Implications",
                                             headerPanel("Policy Implications"),
                                             h4("Through the analysis, I found uneven residential construction activity in Philadelphia. New residential construction is located primarily in economically well-off areas with relatively few minorities. The analysis provided a macro-level look at these patterns, but researchers and policymakers may want to conduct further analysis to see the factors driving these unequal developments.
                                             Efforts may need to be undertaken to increase development and construction activity in economically deprived areas. In these areas, new construction would include an influx of newer homes that may change the landscape of these blighted areas and increase construction jobs and other positive economic and social benefits. However, the City would need to ensure that new housing construction is affordable for the residents and are not part of patterns of gentrification."))
                                            
                                              
                                   
                                   )))


shinyApp(ui = ui, server = server)
