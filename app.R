library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(stringr)
library(plotly)
library(DT)

# Load cleaned USGS site metadata
sites <- readRDS("data/cleaned/guam_sites.rds") %>%
  rename(Lat = latitude, Lon = longitude, Name = station_name)

# Load merged shapefile + census data
guam_merged <- readRDS("data/combined/guam_places_with_all_data.rds") %>%
  mutate(
    People_per_Unit = ifelse(Housing_Units > 0, Population / Housing_Units, NA)
  ) %>%
  st_transform(crs = 4326)

# Site type labels
site_type_labels <- c(
  "AT" = "Atmosphere", "ES" = "Estuary", "GW" = "Groundwater", "GW-TH" = "Test Hole",
  "LA" = "Land Application", "LK" = "Lake/Reservoir", "SB-CV" = "Cave",
  "SP" = "Spring", "ST" = "Stream"
)
site_type_choices <- setNames(names(site_type_labels), site_type_labels)

# Choropleth variable options
choropleth_vars <- c(
  "Population" = "Population",
  "Housing Units" = "Housing_Units",
  "People per Housing Unit" = "People_per_Unit"
)

# UI
ui <- fluidPage(
  titlePanel("USGS Sites and Housing/Demographics in Guam"),
  p("This dashboard explores publicly available data from the USGS and 2020 Census for Guam. 
   Use the sidebar to filter water monitoring sites by type and choose which demographic or housing variable to map."),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Select USGS site type(s)",
                  choices = site_type_choices,
                  selected = "ST",
                  multiple = TRUE),
      selectInput("choropleth", "Select choropleth variable", choices = choropleth_vars, selected = "Population")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 600)),
        tabPanel("Charts",
                 tags$p("Use the charts below to explore relationships between population, housing, density, and USGS water site coverage across villages in Guam."),
                 br(),
                 tags$h4("Top Villages by Population and Housing"),
                 tags$p("Compares the top 10 most populated villages with their total housing unit counts."),
                 plotlyOutput("bar_chart"),
                 br(),
                 tags$h4("People per Housing Unit"),
                 tags$p("Shows how densely occupied housing is in each village. Higher values may indicate overcrowding."),
                 plotlyOutput("density_chart"),
                 br(),
                 tags$h4("USGS Monitoring Site Types"),
                 tags$p("Breakdown of all USGS water monitoring stations in Guam by type. 'Groundwater' and 'Stream' are the most common."),
                 plotlyOutput("usgs_type_chart"),
                 br(),
                 tags$h4("Population vs Housing (with Regression)"),
                 tags$p("Each point represents a village. The red line shows the linear relationship between population and number of housing units."),
                 plotlyOutput("scatter_plot")
        ),
        tabPanel("Data Explorer",
                 tags$p("Use the table below to browse and sort population, housing, and density metrics for all villages in Guam. Click on column headers to sort."),
                 br(),
                 DT::dataTableOutput("data_table")),
        tabPanel("About",
                 h3("Data Sources"),
                 p("USGS National Water Information System site data from ", 
                   a("https://waterdata.usgs.gov", href = "https://waterdata.usgs.gov", target = "_blank")),
                 p("2020 Guam Census population and housing data from the U.S. Census Bureau."),
                 h3("Author"),
                 p("Created by Kristen Bestavros, MA415 Final Project at Boston University.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive color palette
  paletteInput <- reactive({
    selected_col <- input$choropleth
    values <- guam_merged[[selected_col]]
    
    if (is.numeric(values) && any(is.finite(values), na.rm = TRUE)) {
      colorNumeric("Reds", domain = values, na.color = "transparent")
    } else {
      # fallback: blank palette so leaflet won't break
      colorNumeric("Greys", domain = c(0, 1), na.color = "transparent")
    }
  })
  
  # Filtered site markers
  filtered_sites <- reactive({
    req(input$type)  # ensures it's not NULL
    sites %>%
      filter(site_type %in% input$type)
  })
  
  output$map <- renderLeaflet({
    pal <- paletteInput()
    selected_col <- input$choropleth
    label_field <- names(choropleth_vars)[choropleth_vars == selected_col]
    
    leaflet() %>%
      addTiles() %>%
      
      addPolygons(
        data = guam_merged,
        layerId = ~NAME,  # optional: allows future interactivity
        fillColor = ~pal(get(selected_col)),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(
          "<strong>", NAME, "</strong><br>",
          "Population: ", formatC(Population, big.mark = ","), "<br>",
          "Housing Units: ", formatC(Housing_Units, big.mark = ","), "<br>",
          "People/Unit: ", round(People_per_Unit, 2)
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      
      addLegend(
        "bottomright",
        pal = pal,
        values = guam_merged[[selected_col]],
        title = label_field,
        opacity = 0.7
      )
  })
  
  observe({
    leafletProxy("map", data = filtered_sites()) %>%
      clearGroup("usgs") %>%
      {
        if (nrow(filtered_sites()) > 0) {
          addCircleMarkers(
            .,
            lng = ~Lon, lat = ~Lat,
            popup = ~paste0(
              "<strong>", Name, "</strong><br>",
              "Type: ", site_type_labels[site_type], "<br>",
              "Datum: ", datum, "<br>",
              "USGS Site ID: ", site_no, "<br>",
              "<a href='https://waterdata.usgs.gov/nwis/uv?site_no=", site_no, 
              "' target='_blank'>Real-Time Graph</a>"
            ),
            group = "usgs",
            radius = 5,
            fillOpacity = 0.7
          )
        } else {
          .
        }
      }
  })
  
  output$bar_chart <- renderPlotly({
    plot_data <- guam_merged %>%
      st_drop_geometry() %>%
      arrange(desc(Population)) %>%
      slice_head(n = 10)
    
    plot_ly(plot_data, x = ~reorder(NAME, Population), y = ~Population, type = 'bar', name = "Population",
            marker = list(color = 'rgba(55, 83, 109, 0.7)')) %>%
      add_trace(y = ~Housing_Units, name = "Housing Units") %>%
      layout(
        barmode = 'group',
        xaxis = list(title = "Village"),
        yaxis = list(title = "Count"),
        title = "Top 10 Villages by Population and Housing"
      )
  })
  
  output$density_chart <- renderPlotly({
    plot_data <- guam_merged %>% st_drop_geometry()
    
    plot_ly(plot_data, 
            x = ~reorder(NAME, -People_per_Unit), 
            y = ~People_per_Unit, 
            type = "bar", 
            name = "People per Housing Unit",
            marker = list(color = 'rgba(204, 102, 119, 0.7)')) %>%
      layout(title = "Density: People per Housing Unit by Village",
             xaxis = list(title = "Village"), 
             yaxis = list(title = "People per Unit"))
  })
  
  output$data_table <- DT::renderDataTable({
    guam_merged %>%
      st_drop_geometry() %>%
      select(NAME, Population, Housing_Units, People_per_Unit) %>%
      arrange(desc(Population))
  })
  
  output$usgs_type_chart <- renderPlotly({
    sites %>%
      count(site_type) %>%
      mutate(label = site_type_labels[site_type]) %>%
      plot_ly(x = ~reorder(label, -n), y = ~n, type = 'bar',
              marker = list(color = 'rgba(0,123,255,0.7)')) %>%
      layout(title = "USGS Site Type Distribution in Guam",
             xaxis = list(title = "Site Type"), 
             yaxis = list(title = "Number of Sites"))
  })
  
  output$scatter_plot <- renderPlotly({
    plot_data <- guam_merged %>%
      st_drop_geometry() %>%
      select(Village = NAME, Population, Housing_Units, People_per_Unit) %>%
      filter(!is.na(Population), !is.na(Housing_Units))
    
    # Fit linear model
    lm_model <- lm(Population ~ Housing_Units, data = plot_data)
    
    # Create fitted line data
    fitted_line <- data.frame(
      Housing_Units = sort(plot_data$Housing_Units),
      Fitted_Pop = predict(lm_model, newdata = data.frame(Housing_Units = sort(plot_data$Housing_Units)))
    )
    
    # Scatter plot with regression overlay
    plot_ly(
      x = plot_data$Housing_Units,
      y = plot_data$Population,
      type = 'scatter',
      mode = 'markers',
      name = 'Villages',
      text = paste0(plot_data$Village, "<br>", "People/unit: ", round(plot_data$People_per_Unit, 2)),
      marker = list(size = 10, color = 'rgba(0,100,200,0.6)', line = list(width = 1, color = 'white'))
    ) %>%
      add_trace(
        data = fitted_line,
        x = ~Housing_Units,
        y = ~Fitted_Pop,
        mode = "lines",
        name = "Linear Fit",
        line = list(color = "red", width = 2)
      ) %>%
      layout(
        title = "Population vs Housing Units (with Regression)",
        xaxis = list(title = "Housing Units"),
        yaxis = list(title = "Population"),
        hovermode = "closest"
      )
  })
  
}

shinyApp(ui, server)
