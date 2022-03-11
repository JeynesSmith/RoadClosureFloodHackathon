#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(leaflet.extras)


# pre loading
roads <- st_read("data/BrisbaneRoads.shp")
suburbs <- read_csv("data/20220120000odstreet.csv")

# huge dataset so reducing to just Streets and Roads of South Brisbane
# and side effect also any other streets and roads in wider Brisbane with the same name
suburbSubset <- suburbs %>%
    filter(SUBURB == "SOUTH BRISBANE") %>%
    mutate(full = paste(`STREET NAME`,`STREET TYPE`))

subsetRoads <- roads %>%
    filter(tolower(name) %in% tolower(suburbSubset$full)) %>%
    mutate(statues = "open") %>%
    mutate(reports = 0) %>%
    mutate(statues = case_when(name == "Fish Lane" ~ "closed",
                            name != "Fish Lane" ~ statues))

#TODO: add custom colour
# colours, change number based on number of categories
factpal <- colorFactor(topo.colors(2), subsetRoads$statues)


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Road Closures"),


    
    
    # display map
    leafletOutput("roadMap"),
    
    
    # Entry for road name
    selectizeInput("road",
                   label = "Select Road",
                   choices = unique(subsetRoads$osm_id),
                   options = list(create = TRUE, maxItems = 1, create = FALSE,
                                  placeholder = 'Search Roads')),
    
    # Submit as closed button
    actionButton("closed", "Flag as Closed")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## reactive database ##
    DF1 <- reactiveValues(data=subsetRoads)
    
    
    ## render map ##
    output$roadMap <- renderLeaflet({
        leaflet(DF1$data) %>%
            addTiles() %>%
            addPolylines(label = ~reports, layerId = ~osm_id,
                         color = ~factpal(statues)) %>%
            leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))
        })

    
    ## click on markers ##
    observeEvent(input$roadMap_shape_click, {

        # update select box
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             inputId = "road",
                             selected = input$roadMap_shape_click$id)
                             # selected = filter(subsetRoads, osm_id == input$roadMap_shape_click$id)$name)
    })

    
    ## highlight selected road segment ##
    proxy <- leafletProxy("roadMap")

    observe({
        if(input$road %in% subsetRoads$osm_id){
            selectedPolygon <- subset(subsetRoads,subsetRoads$osm_id==input$road) #filter(subsetRoads, osm_id == input$road)
            
            # remove any previously highlighted polygon
            proxy %>% clearGroup(group = "highlightedPolygon")

            # add a slightly thicker red polygon on top of the selected one
            proxy %>% addPolylines(stroke=TRUE, weight = 12,color="orange", opacity=0.9,
                                   data=selectedPolygon,group="highlightedPolygon")
        }
    })
    
    
    ## flag as closed ##
    observeEvent(input$closed, {
        # proxy %>% addPolylines(stroke=TRUE, weight = 5,color="red", opacity=0.9,
        #                        data=selectedPolygon,group="floodedPolygon")
        
        df<-reactive({
            df <- DF1$data %>%
                mutate(statues = case_when(osm_id == input$road ~ "closed",
                                           osm_id != input$road ~ statues)) %>%
                mutate(reports = case_when(osm_id == input$road ~ reports + 1,
                                           osm_id != input$road ~ reports))
            return(df)
        })
        DF1$data <- df()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


## ideas for future implimentation ##
# different levels of reporting, 
# eg we trust police or other emergency service workers reports of road colsure more

