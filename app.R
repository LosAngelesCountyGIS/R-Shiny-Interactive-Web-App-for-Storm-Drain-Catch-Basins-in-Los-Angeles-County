# Load required libraries
library(shiny)
library(dplyr)
library(leaflet)
library(DT)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Los Angeles County Catch Basin WebGIS "),
  # Create a split layout for draggable resizing
  tags$head(
    tags$style(HTML("
      #map {
        height: 600px;
      }
      #table {
        height: 600px;
        overflow-y: auto;
      }
      .highlight {
        background-color: yellow !important;
      }
    "))
  ),
  fluidRow(
    column(12, 
           div(id = "map", leafletOutput("bbmap", height = "100%")),
           div(style = "cursor: ew-resize; width: 5px; background-color: gray; display: inline-block;", 
               id = "drag", 
               title = "Drag to resize")
    ),
    column(12, 
           div(id = "table", DTOutput("data"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Import Data and clean it
  catch_basin_data <- read.csv("D:/PublicWorks_presentation/LosAngeles_County__StormDrain_CatchBasin.csv", stringsAsFactors = FALSE)
  
  # Convert Latitude and Longitude to numeric
  catch_basin_data$Latitude <- as.numeric(catch_basin_data$Latitude)
  catch_basin_data$Longitude <- as.numeric(catch_basin_data$Longitude)
  
  # Remove rows with NA values in Latitude or Longitude
  catch_basin_data <- filter(catch_basin_data, !is.na(Latitude) & !is.na(Longitude))
  
  # Create a new column for the popup label
  catch_basin_data <- mutate(catch_basin_data, cntnt = paste0(
    '<strong>OBJECTID:</strong> ', OBJECTID,
    '<br><strong>Name:</strong> ', NAME,
    '<br><strong>Permit No:</strong> ', PERMIT_NO,
    '<br><strong>DWG No:</strong> ', DWGNO,
    '<br><strong>Plan No:</strong> ', PLAN_NO,
    '<br><strong>Located In:</strong> ', LOCATED_IN,
    '<br><strong>Disclaimer:</strong> ', Disclaimer,
    '<br><strong>Longitude:</strong> ', Longitude,
    '<br><strong>Latitude:</strong> ', Latitude
  ))
  
  # Create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(catch_basin_data) %>% 
      addTiles() %>%
      addCircleMarkers(data = catch_basin_data, lat = ~Latitude, lng = ~Longitude, 
                       radius = 5, popup = ~as.character(cntnt), 
                       stroke = FALSE, fillOpacity = 0.8, layerId = ~OBJECTID) %>%
      setView(lng = -118.25, lat = 34.15, zoom = 12) %>%  # Adjusted center point and zoom level
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  # Create a data object to display data
  output$data <- DT::renderDataTable(datatable(
    catch_basin_data[, c("PERMIT_NO", "DWGNO", "MAINTAINED", "LOCATED_IN")],
    filter = 'top',
    colnames = c("Permit No", "DWG No", "Maintained", "Located In"),
    selection = 'single'
  ))
  
  # Observe double-click event on the DataTable
  observeEvent(input$data_rows_selected, {
    req(input$data_rows_selected)  # Ensure a row is selected
    
    # Get the index of the selected row
    selected_row <- input$data_rows_selected
    
    # Retrieve the corresponding Latitude and Longitude
    lat <- catch_basin_data$Latitude[selected_row]
    lng <- catch_basin_data$Longitude[selected_row]
    object_id <- catch_basin_data$OBJECTID[selected_row]
    
    # Zoom to the selected point
    leafletProxy("bbmap") %>% setView(lng = lng, lat = lat, zoom = 15)
    
    # Highlight the point on the map
    leafletProxy("bbmap") %>% 
      clearGroup("selected") %>% 
      addCircleMarkers(data = catch_basin_data[catch_basin_data$OBJECTID == object_id, ],
                       lat = ~Latitude, lng = ~Longitude,
                       radius = 7, color = "yellow", 
                       stroke = FALSE, fillOpacity = 0.8, 
                       group = "selected")
    
    # Highlight the selected row in the DataTable
    output$data <- DT::renderDataTable(datatable(
      catch_basin_data[, c("PERMIT_NO", "DWGNO", "MAINTAINED", "LOCATED_IN")],
      filter = 'top',
      colnames = c("Permit No", "DWG No", "Maintained", "Located In"),
      selection = 'single',
      options = list(
        rowCallback = JS('function(row, data, index) {',
                         '  if (index == ', selected_row - 1, ') {',
                         '    $(row).addClass("highlight");',
                         '  }',
                         '}')
      )
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
