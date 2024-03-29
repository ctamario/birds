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
<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R
p_load(stringr, dplyr, sf, leaflet, nngeo, leaflet.extras2, ggplot2, scales, flextable)

prefix_df_unique <- read.table(paste0(getwd(), "/data/prefix_df_out.txt"))

onu_totalen <- read_sf(paste0(getwd(), "/data/onu_totalen.shp"))
=======
p_load(stringr, dplyr, sf, leaflet, nngeo, leaflet.extras2, ggplot2)

prefix_df_unique <- read.table(paste0(getwd(), "/data/prefix_df_out.txt"))

onu_totalen <- read_sf("/data/temp/onu_totalen.shp")
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R
onu_totalen$link <- paste0("https://artportalen.se/Sighting/",onu_totalen$Id,"#ChildSightings")

shiny_df <- onu_totalen
shiny_df$year <- as.numeric(str_sub(shiny_df$Startdatum,1,4))
shiny_df <- shiny_df %>% left_join(prefix_df_unique, by = "Artnamn")

<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R
shiny_df <- shiny_df %>% filter(year < 2023)

onu_lines <- read_sf(paste0(getwd(), "/data/onu_totalen_migrating_wgs84.shp"))
=======
onu_lines <- read_sf("/data/temp/onu_totalen_migrating_wgs84.shp")
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R
onu_lines <- onu_lines %>% left_join(as.data.frame(shiny_df), by = "Id")
#onu_lines <- onu_lines %>% left_join(prefix_df_unique, by = "Artnamn")

shiny_lines <- onu_lines

<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R
onu_upptackar <- read_sf(paste0(getwd(), "/data/onu_totalen_upptackarpunkt2.shp"))
=======
onu_upptackar <- read_sf("/data/temp/onu_totalen_upptackarpunkt2.shp")
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R
onu_upptackar <- onu_upptackar %>% left_join(as.data.frame(shiny_df), by = "Id")
#onu_upptackar <- onu_upptackar %>% left_join(prefix_df_unique, by = "Artnamn")

shiny_upptackar <- onu_upptackar
#shiny_upptackar$year <- as.numeric(str_sub(shiny_upptackar$Startdatum,1,4))

dim(shiny_lines %>% filter(year == 2015))

<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R

#hm <- head(data.frame(shiny_df %>% select(Artnamn, year) %>% filter(Artnamn == "Citronärla")))
#hm

temp <- st_as_sf(st_connect(shiny_upptackar[1,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[1]),], progress = F))
temp$year <- as.numeric(shiny_upptackar[1,"year"])[1]
for(i in seq_along(shiny_upptackar$Id)){
  temp$x[i] <- st_as_sf(st_connect(shiny_upptackar[i,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[i]),], progress = F))
  temp$year[i] <- as.numeric(shiny_upptackar[i,"year"])[1]
}

as.data.frame(temp)
=======
# temp <- st_as_sf(st_connect(shiny_upptackar[1,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[1]),], progress = F))
# temp$year <- as.numeric(shiny_upptackar[1,"year"])[1]
# for(i in seq_along(shiny_upptackar$Id)){
#   temp$x[i] <- st_as_sf(st_connect(shiny_upptackar[i,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[i]),], progress = F))
#   temp$year[i] <- as.numeric(shiny_upptackar[i,"year"])[1]
# }
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R

#onu_totalen %>% filter(as.numeric(str_sub(Startdatum,1,4)) == 2015)

  
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fynd på ÖNU"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Choose year(s):",
                        min = 2015,
                        max = 2022,
                        value = c(2015,2022)),
            selectInput("fromPrefix", "Lägsta prefix:", 0:4, selected=0),
            selectInput("toPrefix", "Högsta prefix", 0:4, selected=4),
            selectInput("SPEC", "Artnamn", "Alla")
         #   plotOutput(outputId = "fyndPlot")#, width=3
        ),
        # Show a plot of the generated distribution
        mainPanel = mainPanel(leafletOutput(outputId="map", height="80vh"))
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  map_df = reactive({
    shiny_df %>% dplyr::filter(year >= input$years[1] & year <= input$years[2]) %>%
      dplyr::filter(prefix >= input$fromPrefix & prefix <= input$toPrefix)
    
  })
  
  upptackar_df = reactive({
    shiny_upptackar %>% dplyr::filter(year >= input$years[1] & year <= input$years[2]) %>%
      dplyr::filter(prefix >= input$fromPrefix & prefix <= input$toPrefix)
    
  })
  
  lines_df = reactive({
    shiny_lines %>% dplyr::filter(year >= input$years[1] & year <= input$years[2]) %>%
      dplyr::filter(prefix >= input$fromPrefix & prefix <= input$toPrefix)
    
  })
  
  observe({
    updateSelectInput(session, "SPEC",
                      choices = c("Alla", unique(map_df()$Artnamn))
    )})
  
  # if(input$SPEC != "Alla"){
  #   map_df = reactive({
  #     shiny_df %>% dplyr::filter(year >= input$years[1] & year <= input$years[2]) %>%
  #       dplyr::filter(prefix >= input$fromPrefix & prefix <= input$toPrefix) %>%
  #       dplyr::filter(Artnamn == input$SPEC)
  #   })
  #   
  # } https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices

  #temp_df = reactive({
  #  temp <- st_connect(upptackar_df()[1,], map_df()[which(map_df()$Id == upptackar_df()$Id[1]),], progress = F)
  #  for(i in seq_along(upptackar_df()$Id)){
  #    temp[i] <- st_connect(upptackar_df()[i,], map_df()[which(map_df()$Id == upptackar_df()$Id[i]),], progress = F)
  #  }
  #  
  #})

    output$map <- renderLeaflet({

      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng=17.1, lat = 57.35, zoom = 13)
      

    })
      
      
      observe({
        
        validate(
          need(input$fromPrefix <= input$toPrefix, 'Se till att ha ett lika som eller lägre "Lägsta prefix" än "Högsta prefix"'),
          need(dim(map_df())[1] > 0, "No cases to plot")
        )
        
        if(input$SPEC == "Alla"){
          leafletProxy("map") %>%
            
            clearMarkers()  %>%
            clearShapes() %>%
            
            addCircleMarkers(data=upptackar_df(), radius=3, color="green",
                             label=~paste(as.character(Artnamn),"upptäcktes härifrån."),
                             group="Upptäckarplats",
                             labelOptions = labelOptions(textsize = "12px")) %>%
            
            addCircleMarkers(data = map_df(), fill = T, fillColor="white", color="black", label = ~paste(Artnamn), group="Fynd", fillOpacity=1, weight=2,
                             popup = ~paste(sep="<br/>",as.character(Artnamn), 
                                            as.character(Startdatum), 
                                            paste("Aktivitet:",as.character(Aktivitet)),
                                            paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), radius=5,
                             labelOptions = labelOptions(textsize = "12px"))
        } else if (input$SPEC != "Alla"){
          leafletProxy("map") %>%
            
            clearMarkers()  %>%
            clearShapes() %>%
            
            addCircleMarkers(data=upptackar_df(), radius=3, color="green",
                             label=~paste(as.character(Artnamn),"upptäcktes härifrån."),
                             group="Upptäckarplats",
                             labelOptions = labelOptions(textsize = "12px")) %>%
            
            addCircleMarkers(data = map_df() %>% dplyr::filter(Artnamn == input$SPEC), fill = T, fillColor="white", color="black", label = ~paste(Artnamn), group="Fynd", fillOpacity=1, weight=2,
                             popup = ~paste(sep="<br/>",as.character(Artnamn), 
                                            as.character(Startdatum), 
                                            paste("Aktivitet:",as.character(Aktivitet)),
                                            paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), radius=5,
                             labelOptions = labelOptions(textsize = "12px"))
        }




      })
      
<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R
output$fyndPlot <- renderPlot({
  if(input$SPEC == "Alla"){
    ggplot(data=map_df(), aes(x=year)) + geom_histogram(binwidth=1) + xlim(c(2014,2024))# + scale_y_continuous(breaks = pretty_breaks()) + theme_classic()
  } else if(input$SPEC != "Alla"){
    ggplot(data=map_df() %>% dplyr::filter(Artnamn == input$SPEC), aes(x=year)) +
      geom_histogram(binwidth=1) +
      xlim(c(2014,2024))# + 
      #scale_y_continuous(breaks = pretty_breaks()) + theme_classic()
  }
=======
      observe({
        
        validate(
          need(input$fromPrefix <= input$toPrefix, 'Se till att ha ett lika som eller lägre "Lägsta prefix" än "Högsta prefix"'),
          need(dim(lines_df())[1] > 0, "No cases to plot")
        )
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R

        leafletProxy("map", data = map_df()) %>%

<<<<<<< HEAD:ONU_shiny/app_troubleshooting.R
 output$fyndTable <- renderTable({
   if(input$SPEC == "Alla"){
     data.frame(map_df()) %>% select(Artnamn, Startdatum, link) %>% filter(Artnamn == input$SPEC) %>% dplyr::arrange(desc(Startdatum))
   } else if(input$SPEC != "Alla"){
     data.frame(map_df()) %>% dplyr::filter(Artnamn == input$SPEC) %>% select(Artnamn, Startdatum, link) %>% dplyr::arrange(desc(Startdatum))
   }
 })
=======
        addArrowhead(data = lines_df(), color="white", label=~paste(Artnamn), group="Fynd",
                     popup=~paste(sep="<br/>",as.character(Artnamn),
                                  as.character(Startdatum), 
                                  as.character(Aktivitet),
                                  paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), 
                     options=arrowheadOptions(frequency="200m", size="10px"),
                     weight=3,
                     labelOptions = labelOptions(textsize = "12px"))
      })
      
      #output$fyndPlot <- renderPlot({map_df()
      #  map_df() %>% ggplot(aes(x=year)) + geom_histogram(breaks=2015:2022, center = 1, color = "#000000", fill = "#0099F8") + theme_classic()
      #  })
      
 #     observe({
#        leafletProxy("map", data = upptackar_df()) %>%
 #         clearMarkers() %>%

   #   })
>>>>>>> f247624ef48127d29c7038f98ba0e1989b95df2d:ONU_shiny/old/app.R

}

# https://stackoverflow.com/questions/48781380/shiny-how-to-highlight-an-object-on-a-leaflet-map-when-selecting-a-record-in-a

# Run the application 
shinyApp(ui = ui, server = server)
