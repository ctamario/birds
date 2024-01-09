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
p_load(stringr, dplyr, sf, leaflet, nngeo, leaflet.extras2, ggplot2, scales, flextable, DT)

prefix_df_unique <- read.table(paste0(getwd(), "/data/prefix_df_out.txt"))

onu_totalen <- read_sf("/data/temp/onu_totalen.shp")
onu_totalen$link <- paste0("https://artportalen.se/Sighting/",onu_totalen$Id,"#ChildSightings")

shiny_df <- onu_totalen
shiny_df$year <- as.numeric(str_sub(shiny_df$Startdatum,1,4))
shiny_df <- shiny_df %>% left_join(prefix_df_unique, by = "Artnamn")


onu_lines <- read_sf("/data/temp/onu_totalen_migrating_wgs84.shp")
onu_lines <- onu_lines %>% left_join(as.data.frame(shiny_df), by = "Id")

shiny_lines <- onu_lines

onu_upptackar <- read_sf("/data/temp/onu_totalen_upptackarpunkt2.shp")
onu_upptackar <- onu_upptackar %>% left_join(as.data.frame(shiny_df), by = "Id")

shiny_upptackar <- onu_upptackar
#shiny_upptackar$year <- as.numeric(str_sub(shiny_upptackar$Startdatum,1,4))

dim(shiny_lines %>% filter(year == 2011))


hm <- head(data.frame(shiny_df %>% select(Artnamn, year) %>% filter(Artnamn == "Citronärla")))
hm

founder <- st_as_sf(st_connect(shiny_upptackar[1,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[1]),], progress = F))
founder$Id <- as.numeric(shiny_upptackar[1,"Id"])[1]

#temp2 <- st_as_sf(st_connect(shiny_upptackar[3,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[3]),], progress = F))
#temp2$year <- as.numeric(shiny_upptackar[3,"year"])[1]

#rbind(founder, temp2)

for(i in 2:length(shiny_upptackar$Id)){
  temp <- st_as_sf(st_connect(shiny_upptackar[i,], shiny_df[which(shiny_df$Id == shiny_upptackar$Id[i]),], progress = F))
  temp$Id <- as.numeric(shiny_upptackar[i,"Id"])[1]
  founder <- rbind(founder, temp)
}

founder2 <- founder %>% left_join(as.data.frame(shiny_df), by = "Id")

#str(temp)

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
                        max = 2024,
                        value = c(2015,2024)),
            selectInput("fromPrefix", "Lägsta prefix:", 0:4, selected=0),
            selectInput("toPrefix", "Högsta prefix", 0:4, selected=4),
            selectInput("SPEC", "Artnamn", "Alla"),
            plotOutput(outputId = "fyndPlot", height = 200),
            tableOutput(outputId = "fyndTable")
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
  
  founder_df = reactive({
    founder2 %>% dplyr::filter(year >= input$years[1] & year <= input$years[2]) %>%
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
          need(input$fromPrefix <= input$toPrefix, 'Se till att ha ett lika som eller lägre "Lägsta prefix" än "Högsta prefix"')
          #need(dim(map_df())[1] > 0, "No cases to plot")
        )
        
        if(dim(map_df())[1] > 0){

          if(input$SPEC == "Alla"){
            leafletProxy("map") %>%
              
              clearMarkers()  %>%
              clearShapes() %>%
              addPolylines(data=founder_df(), color="white", dashArray = "10 10", weight=3, group="Upptäckarplats") %>%
              addCircleMarkers(data = map_df(), fill = T, fillColor="white", color="black", label = ~paste(Artnamn), group="Fynd", fillOpacity=1, weight=2,
                               popup = ~paste(sep="<br/>",as.character(Artnamn), 
                                              as.character(Startdatum), 
                                              paste("Id:", as.character(Id)),
                                              paste("Aktivitet:",as.character(Aktivitet)),
                                              paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), radius=5,
                               labelOptions = labelOptions(textsize = "12px")) %>%
              addCircleMarkers(data=upptackar_df(), radius=3, color="green", 
                               label=~paste(as.character(Artnamn),"upptäcktes härifrån."), 
                               group="Upptäckarplats",
                               labelOptions = labelOptions(textsize = "12px")) %>%
              addLayersControl(overlayGroups = c("Fynd", "Upptäckarplats"), 
                               options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
          } else if (input$SPEC != "Alla"){
            if(nrow(lines_df() %>% dplyr::filter(Artnamn == input$SPEC)) > 0){
              leafletProxy("map") %>%
                
                clearMarkers()  %>%
                clearShapes() %>%
                addPolylines(data=founder_df() %>% dplyr::filter(Artnamn == input$SPEC), color="white", dashArray = "10 10", weight=3, group="Upptäckarplats") %>%
                addArrowhead(data = lines_df() %>% dplyr::filter(Artnamn == input$SPEC), color="white", label=~paste(Artnamn), group="Sträckriktning",
                             popup=~paste(sep="<br/>",as.character(Artnamn),
                                          as.character(Startdatum), 
                                          as.character(Aktivitet),
                                          paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), 
                             options=arrowheadOptions(frequency="200m", size="10px"),
                             weight=3,
                             labelOptions = labelOptions(textsize = "12px")) %>%
                addCircleMarkers(data = map_df() %>% dplyr::filter(Artnamn == input$SPEC), fill = T, fillColor="white", color="black", label = ~paste(Artnamn), group="Fynd", fillOpacity=1, weight=2,
                                 popup = ~paste(sep="<br/>",as.character(Artnamn), 
                                                as.character(Startdatum), 
                                                paste("Id:", as.character(Id)),
                                                paste("Aktivitet:",as.character(Aktivitet)),
                                                paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), radius=5,
                                 labelOptions = labelOptions(textsize = "12px")) %>%
                addCircleMarkers(data=upptackar_df() %>% dplyr::filter(Artnamn == input$SPEC), radius=3, color="green", 
                                 label=~paste(as.character(Artnamn),"upptäcktes härifrån."), 
                                 group="Upptäckarplats",
                                 labelOptions = labelOptions(textsize = "12px")) %>%
                addLayersControl(overlayGroups = c("Fynd", "Upptäckarplats", "Sträckriktning"), 
                                 options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
            } else if (nrow(lines_df() %>% dplyr::filter(Artnamn == input$SPEC)) == 0){
              leafletProxy("map") %>%
                
                clearMarkers()  %>%
                clearShapes() %>%
                addPolylines(data=founder_df() %>% dplyr::filter(Artnamn == input$SPEC), color="white", dashArray = "10 10", weight=3, group="Upptäckarplats") %>%
                #addArrowhead(data = lines_df() %>% dplyr::filter(Artnamn == input$SPEC), color="white", label=~paste(Artnamn), group="Sträckriktning",
                #             popup=~paste(sep="<br/>",as.character(Artnamn),
                #                          as.character(Startdatum), 
                #                          as.character(Aktivitet),
                #                          paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), 
                #             options=arrowheadOptions(frequency="200m", size="10px"),
                #             weight=3,
                #             labelOptions = labelOptions(textsize = "12px")) %>%
                addCircleMarkers(data = map_df() %>% dplyr::filter(Artnamn == input$SPEC), fill = T, fillColor="white", color="black", label = ~paste(Artnamn), group="Fynd", fillOpacity=1, weight=2,
                                 popup = ~paste(sep="<br/>",as.character(Artnamn), 
                                                as.character(Startdatum), 
                                                paste("Id:", as.character(Id)),
                                                paste("Aktivitet:",as.character(Aktivitet)),
                                                paste0("<a href='",link, "'target='_blank'>", "Artportalen</a>")), radius=5,
                                 labelOptions = labelOptions(textsize = "12px")) %>%
                addCircleMarkers(data=upptackar_df() %>% dplyr::filter(Artnamn == input$SPEC), radius=3, color="green", 
                                 label=~paste(as.character(Artnamn),"upptäcktes härifrån."), 
                                 group="Upptäckarplats",
                                 labelOptions = labelOptions(textsize = "12px")) %>%
                addLayersControl(overlayGroups = c("Fynd", "Upptäckarplats", "Sträckriktning"), 
                                 options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
            }
            
            
          }
          
        } else if (dim(map_df())[1] == 0){
          leafletProxy("map") %>%
            clearMarkers()  %>%
            clearShapes()
        

        }
        
        })

      
    output$fyndPlot <- renderPlot({
      if(input$SPEC == "Alla"){
        ggplot(data=map_df(), aes(x=year)) +
          geom_histogram(binwidth=1) +
          scale_x_continuous(limits=c(2015,2024), breaks=seq(2015,2024,by=2), oob = scales::oob_keep)+
          theme_classic() # + scale_y_continuous(breaks = pretty_breaks()) + theme_classic()
      } else if(input$SPEC != "Alla"){
        ggplot(data=map_df() %>% dplyr::filter(Artnamn == input$SPEC), aes(x=year)) +
          geom_histogram(binwidth=1) +
          scale_x_continuous(limits=c(2015,2024), breaks=seq(2015,2024,by=2), oob = scales::oob_keep)+
          theme_classic() #+
          #scale_y_continuous(breaks = pretty_breaks()) + theme_classic()
      }
    
      
    })
  

 # output$fyndTable <- renderTable({
 #   if(input$SPEC == "Alla"){
 #     data.frame(map_df()) %>% select(Artnamn, year, link) %>% filter(Artnamn == input$SPEC)
 #   } else if(input$SPEC != "Alla"){
 #     data.frame(map_df()) %>% dplyr::filter(Artnamn == input$SPEC) %>% select(Artnamn, year, link)
 #   }
 # })

}

# https://stackoverflow.com/questions/48781380/shiny-how-to-highlight-an-object-on-a-leaflet-map-when-selecting-a-record-in-a

# Run the application 
shinyApp(ui = ui, server = server)
