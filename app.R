
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(lubridate)
library(shinylive)
library(httpuv)

sdf <- st_read("~/Documents/Application_form/NGO_projects/En10ergy_haringey/data/cleaned/epc_postcode.gpkg")
lsoa <- st_read("~/Documents/Application_form/NGO_projects/En10ergy_haringey/data/cleaned/lsoa.shp")
# Define UI for application that draws a histogram

total <- sdf %>% 
  st_drop_geometry() %>% 
  count(lsoa11cd, name = "total_counts")

# Calculate counts for each selected EPC rating
each_rate <- sdf %>% 
  st_drop_geometry() %>% 
  #filter(CURRENT_ENERGY_RATING %in% input$epc) %>%
  count(lsoa11cd, CURRENT_ENERGY_RATING, name = "counts")

ui <- fluidPage(
  
  titlePanel("Energy Efficiency Interactive Map"), 
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      
      checkboxGroupInput("epc",  "Energy Efficiency Rating: ", 
                         choices = c("A", "B", "C", "D", "E", "F", "G"), 
                         selected = NULL, inline = TRUE),
      
      sliderInput("imd", "Index of Multiple Deprivation Decile: ", min = 1, max = 9, value = c(1, 2), width = "100%"),  # 2
      
      sliderInput("years", "EPC inspected in year: ", 
                  min = 2006,  max = 2024, value = c(2015, 2024), width = "100%"), 
      
      hr(),
      HTML("<b>Notes:</b><br>
            Energy Efficient Rate A is the most efficient.<br>
            IMD Decile 1 is the most deprived.")
      
      
    ), 
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map')
    )
  )
)

server <- function(input, output){ 
  df = reactive({ 
    req(input$epc)
    
    
    
    sdf %>% 
      st_drop_geometry() %>% 
      filter(
        (INSPECTION_DATE >= as.Date(paste0(input$years[1], "-01-01")) & #  lubridate::ymd(input$years[1] + 1, truncated = 2L) 
           INSPECTION_DATE <= as.Date(paste0(input$years[2], "-12-31")) ) &  
          
          (SOA_decile >= input$imd[1] & SOA_decile <= input$imd[2]) & 
          
          (CURRENT_ENERGY_RATING %in% input$epc)
      ) %>% 
      
      count(lsoa11cd, name = "rating_counts") %>% 
      left_join(total, by = "lsoa11cd") %>% 
      mutate(ratio = round(rating_counts/total_counts*100, 1) )
  })
  
  output$map = renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -.11, lat = 51.59, zoom = 12.5) %>% # 1136 
      addPolygons(data = lsoa, color = "black", stroke = 0, opacity = 0, group = "Haringey Border") %>% 
      
      addLayersControl(
        overlayGroups = c("Haringey Border", "LSOA areas with selected EPC & IMD"), 
        options = layersControlOptions(collapsed = TRUE)) 
    
  })
  
  observe({
    df_data <- df()
    
    epc_pal <- colorNumeric("YlOrRd", domain = c(0, 100)) 
    
    # Create custom popup content
    df_data <- df_data %>%
      group_by(lsoa11cd) %>%
      summarize(
        total_counts = first(total_counts),
        ratio = first(ratio),
        popup_content = paste0("<strong>LSOA: ", lsoa11cd, "</strong><br>",
                               "Total EPCs: ", total_counts, 
                               "<br>Selected HH: ", rating_counts, ", ", ratio, "%"
        )
      ) %>%
      left_join(lsoa, by = "lsoa11cd") %>%
      st_as_sf(crs = st_crs(lsoa))
    
    
    leafletProxy("map", data = df_data) %>%
      clearGroup("LSOA areas with selected EPC & IMD") %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", domain = df_data$ratio)(ratio), 
        #fillColor = ~epc_pal(ratio), # for fixed scale
        fillOpacity = 1,
        weight = 0.5,
        smoothFactor = 0.2, 
        group = "LSOA areas with selected EPC & IMD", 
        popup = ~popup_content
      ) %>% 
      clearControls() %>%  # Clear existing legends and other controls 
      addLegend(
        "bottomleft", 
        # pal = epc_pal, values = c(0, 100), # for fixed scale
        pal = colorNumeric("YlOrRd", domain = df_data$ratio), values = df_data$ratio, 
        title = "Selected</br>HH %", 
        opacity = .8
      )
  })
  
}

shinyApp(ui = ui, server = server)

