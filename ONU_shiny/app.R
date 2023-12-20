#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pacman)
p_load(stringr, dplyr, sf, leaflet)

onu_totalen <- read_sf("C:/projekt/birds/data/gis/onu_totalen.shp")
onu_totalen$link <- paste0("https://artportalen.se/Sighting/",onu_totalen$Id,"#ChildSightings")

shiny_df <- onu_totalen
shiny_df$year <- as.numeric(str_sub(shiny_df$Startdatum,1,4))

onu_lines <- read_sf("C:/projekt/birds/data/gis/onu_totalen_migrating_wgs84.shp")
onu_lines <- onu_lines %>% left_join(as.data.frame(onu_totalen), by = "Id")

shiny_lines <- onu_lines

onu_upptackar <- read_sf("C:/projekt/birds/data/gis/onu_totalen_upptackarpunkt2.shp")
onu_upptackar <- onu_upptackar %>% left_join(as.data.frame(onu_totalen), by = "Id")

shiny_upptackar <- onu_upptackar
shiny_upptackar$year <- as.numeric(str_sub(shiny_upptackar$Startdatum,1,4))

temp <- st_as_sf(st_connect(shiny_upptackar[1,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[1]),], progress = F))
temp$year <- as.numeric(shiny_upptackar[1,"year"])[1]
for(i in seq_along(shiny_upptackar$Id)){
  temp$x[i] <- st_as_sf(st_connect(shiny_upptackar[i,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[i]),], progress = F))
  temp$year[i] <- as.numeric(shiny_upptackar[i,"year"])[1]
}
#onu_totalen %>% filter(as.numeric(str_sub(Startdatum,1,4)) == 2015)

  
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    leafletOutput("mymap"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Choose year(s):",
                        min = 2015,
                        max = 2022,
                        value = c(2020,2022))
        ),

        # Show a plot of the generated distribution
        mainPanel = mainPanel(leafletOutput(outputId="map", height="80vh")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  map_df = reactive({
    shiny_df %>% dplyr::filter(year >= input$years[1] & year <= input$years[2])
    
  })
  
  upptackar_df = reactive({
    shiny_upptackar %>% dplyr::filter(year >= input$years[1] & year <= input$years[2])
    
  })
  
  temp_df = reactive({
    temp <- st_connect(upptackar_df()[1,], map_df()[which(map_df()$Id == upptackar_df()$Id[1]),], progress = F)
    for(i in seq_along(upptackar_df()$Id)){
      temp[i] <- st_connect(upptackar_df()[i,], map_df()[which(map_df()$Id == upptackar_df()$Id[i]),], progress = F)
    }
    
  })

    output$map <- renderLeaflet({
      
      leaflet() %>%
        addTiles() %>%
        setView(lng=17.1, lat = 57.35, zoom = 13) %>%
        addCircleMarkers(data=map_df()) %>% 
        addCircleMarkers(data=upptackar_df(), color="red") #%>% 
        addPolylines(data=temp_df(), color="white", dashArray = "10 10", weight=3)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
