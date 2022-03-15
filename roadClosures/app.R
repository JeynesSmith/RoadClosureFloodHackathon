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
library(osmextract)
library(sf)
library(leaflet.extras)
source("homemade_leaflet.R")


# pre loading

# load all osm data for brisbane
file <- "data/Brisbane.osm.pbf"

# extract polygons
feature_mpolygons <- oe_read(file, "multipolygons")

# get boundary for desired suburb
suburb <- "South Brisbane"

suburbBoundary <- filter(feature_mpolygons, name == suburb)

# Extract named roads and paths within suburb, add columns for statues and # of reports
subsetRoads <- oe_read(file, boundary = suburbBoundary) %>%
    drop_na(name, highway) %>%
    mutate(statues = "open") %>%
    mutate(reports = 0) %>%
    mutate(statues = case_when(name == "Fish Lane" ~ "closed",
                                name != "Fish Lane" ~ statues))



#TODO: add custom colour
# colours, change number based on number of categories
factpal <- colorFactor(c("#ff0000", "#00E5FF"), subsetRoads$statues)


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Road Closures"),

    # display map
    leafletOutput("roadMap", height = "75vh"),

    # Entry for road name
    selectizeInput("road",
                   label = "Select Road",
                   choices = unique(subsetRoads$osm_id),
                   options = list(create = TRUE, maxItems = 1, create = FALSE,
                                  placeholder = 'Search Roads')),

    # Submit as closed button
    actionButton("closed", "Flag as Closed"),
    loadLeafletChannel()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ## reactive database ##
    DF1 <- reactiveValues(data=subsetRoads)

    ## render map ##
    output$roadMap <- renderLeaflet({
        leaflet(DF1$data) %>%
            addTiles() %>%
            addPolylines(label = ~reports, layerId = ~osm_id,
                         color = ~factpal(statues), weight = 20) %>%
            leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
            addReceiver()
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

        send_to(list(type = "function_call", message = "saveView"), session)
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
