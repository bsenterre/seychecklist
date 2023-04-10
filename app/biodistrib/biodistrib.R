# Load data and libraries
library(shiny)#
library(leaflet)#
library(dplyr)#
library(RColorBrewer)
library(ggplot2)
library(data.table)#
library(stringr)
library(mapview)
library(sf)
library(tidyverse)
library(DT)
library(redlistr)
#library(rsconnect)

#To use mapview with shiny see: https://stackoverflow.com/questions/36679944/mapview-for-shiny
#To add user geolocation see: https://github.com/AugustT/shiny_geolocation
#OR THE IMPROVED VERSION HERE: https://github.com/pdelboca/shiny-golf/blob/master/watchpos.R
#OR se https://bowkerlab.shinyapps.io/geotest/

#To develop a 'download' function: https://shiny.rstudio.com/articles/download.html

# R code that runs only once goes here e.g. import data, run calculations
#Outdated: SDimport = data.table::fread(unzip("dwca-seychecklist-v1.2.zip", "occurrence.txt"), header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
if (file.exists("D:/ZAnalysis/ZDraft/datainput/SDint_sensitive.txt")) {
  SDint = data.table::fread("D:/ZAnalysis/ZDraft/datainput/SDint_sensitive.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
} else {
  SDint = data.table::fread("SDint_obscured.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
}
#SDint = data.table::fread("SDint_obscured.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
#OR IF ACCESS TO THE SENSITIVE DATA
#SDimport = data.table::fread("SDint_sensitive.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")


#Filter data that can be mapped
SD <-  SDint[decimalLatitude < 0 & !is.na(decimalLatitude) & decimalLongitude > 0 & !is.na(decimalLongitude),]
#SD <- SD[1:1000,]

#Rename the lat long field for use with leaflet
SD <- dplyr::rename(SD, latitude = decimalLatitude, longitude = decimalLongitude)

#Filter the first 5000 records to make it easier for testing this, and pick needed columns
SD <- dplyr::select(SD, kingdom, scientificName, latitude, longitude, locality,
             informationWithheld, locationRemarks, recordedBy, eventDate)

#Get a list of sspecies names as given in the input file
SDList <- sort(unique(SD$scientificName))
SD2 <- sf::st_as_sf(SD, coords = c("longitude", "latitude"), crs = 4326 )

# Create the App's User Interface and server (using a dropdown list of species names)
ui <- bootstrapPage(
  titlePanel("Exploring species distribution data from the BIO database"),

  tags$script('
              $(document).ready(function () {

                        function getLocation(callback){
                        var options = {
                        enableHighAccuracy: true,
                        timeout: 5000,
                        maximumAge: 0
                        };

                        navigator.geolocation.getCurrentPosition(onSuccess, onError);

                        function onError (err) {
                        Shiny.onInputChange("geolocation", false);
                        }

                        function onSuccess (position) {
                        setTimeout(function () {
                        var coords = position.coords;
                        var timestamp = new Date();

                        console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
                        Shiny.onInputChange("geolocation", true);
                        Shiny.onInputChange("lat", coords.latitude);
                        Shiny.onInputChange("long", coords.longitude);
                        Shiny.onInputChange("accuracy", coords.accuracy);
                        Shiny.onInputChange("time", timestamp)

                        console.log(timestamp);

                        if (callback) {
                        callback();
                        }
                        }, 1100)
                        }
                        }

                        var TIMEOUT = 5000; //SPECIFY
                        var started = false;
                        function getLocationRepeat(){
                        //first time only - no delay needed
                        if (!started) {
                        started = true;
                        getLocation(getLocationRepeat);
                        return;
                        }

                        setTimeout(function () {
                        getLocation(getLocationRepeat);
                        }, TIMEOUT);

                        };

                        getLocationRepeat();

                        });
                        '),
  #The chunk of code above communicates with Java script and creates 3 shiny inputs: geolocation, lat, long

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Species distribution map",
                      fluidRow(
                        column(10,selectInput(inputId="spname", label="Pick a species (n.b. sensitive data are rounded to 1km accuracy)", choices=SDList, selected="Cinnamomum verum J.Presl")),
                        column(2, downloadButton("downloadData", "Download"))
                      ),
                      fluidRow(leafletOutput("mymap")),
                      #verbatimTextOutput("counter"), #The counter does not work
                      p("App version biodistrib 1.1")
                         ),
                tabPanel("Nearby records",
                         verbatimTextOutput("lat"),
                         verbatimTextOutput("long"),
                         verbatimTextOutput("accuracy"),
                         verbatimTextOutput("time"),
                         verbatimTextOutput("geolocation"),

                         numericInput(inputId = "dist", "Enter the distance to your position in meters for visualization of the table with observations existing in the BIO database near your current position (default is about 100m)", value = 100),
                         numericInput(inputId = "nx", "Enter manually Longitude of your current location in decimal degrees (in case of bad gps reception)", value = 55.6515),
                         numericInput(inputId = "ny", "Idem with latitude", value = -4.4797),
                         dataTableOutput("mytable_nearby")
                         ),
                tabPanel("View the list of records",
                         DTOutput("mytable_occurrences")
                         )
    )
  )
)

server <- function(session, input, output) {

  #Create a hit counter (number of visits to the page)
  output$counter <- renderText({
    if(!file.exists("appusage.RData"))
      hits <- 0
    else
      load(file="appusage.RData")
    hits <- hits + 1
    save(hits, file="appusage.RData")
    paste0("Number of visits (hits) since 1st September 2022: ", hits)
  })

  output$mymap <- renderLeaflet({
    data <- SD2[SD2$scientificName==input$spname,]

    dataToCoordinate <- data[data$locationRemarks == "toCoordinate",]
    dataToCoordinateutm <- sf::st_transform(dataToCoordinate, crs=32740)
    dataToCoordinateutm <- dataToCoordinateutm %>% as('Spatial')
    if(nrow(dataToCoordinateutm)>1) {
      AOO <- getAOO(dataToCoordinateutm, grid.size = 2000, min.percent.rule = FALSE, percent = 10)
      EOOpolygon <- makeEOO(dataToCoordinateutm)
      EOOarea <- round(getAreaEOO(EOOpolygon), digits=0)
      EOOpolygon$EOOkm2 <- EOOarea
      AOOpolygon <- makeAOOGrid(dataToCoordinateutm, 2000, min.percent.rule = FALSE,percent = 10)
      AOOpolygon$AOONumberOf2000GridCells <- AOO
    }

    mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery"))

    if(nrow(dataToCoordinateutm)>1) {
      mapAOOEOO <- mapview(AOOpolygon, color="blue", alpha.regions = 0, lwd=0.5, legend=FALSE) + mapview(EOOpolygon, color="#50b397", alpha.regions=0, lwd=2, legend=FALSE)
      map_mapview <- mapview(data, zcol='locationRemarks') + mapAOOEOO
    } else {
      map_mapview <- mapview(data, zcol='locationRemarks')
    }

    map_mapview@map
  })

  observe({
    if(!is.null(input$lat) && input$accuracy < 100){

      lat <- input$lat
      lng <- input$long
      acc <- input$accuracy
      time <- input$time

      proxy <- leafletProxy("mymap")

      proxy  %>%
        clearGroup(group="pos") %>%
        addMarkers(lng=lng, lat=lat, popup=paste("My location is:","<br>",
                                                 lng,"Longitude","<br>",
                                                 lat,"Latitude", "<br>",
                                                 "My accuracy is:",  "<br>",
                                                 acc, "meters"),
                   group="pos") %>%
        addCircles(lng=lng, lat=lat, radius=acc, group="pos")
    }
    else  {

      lat <- input$ny
      lng <- input$nx
      acc <- 10
      time <- "manual"

      proxy <- leafletProxy("mymap")

      proxy  %>%
        clearGroup(group="pos") %>%
        addMarkers(lng=lng, lat=lat, popup=paste("My location is:","<br>",
                                                 lng,"Longitude","<br>",
                                                 lat,"Latitude", "<br>",
                                                 "My accuracy is:",  "<br>",
                                                 acc, "meters",
                                                 "My positioning is:",  "<br>",
                                                 time),
                   group="pos") %>%
        addCircles(lng=lng, lat=lat, radius=acc, group="pos")


    }
  })

  output$mytable_occurrences <- renderDT({
    data <- SDint[SDint$scientificName==input$spname,]
    DT::datatable(data[, c("islandGroup", "island", "locality", "recordedBy", "eventDate", "basisOfRecord",  "occurrenceStatus", "establishmentMeans", "locationRemarks", "occurrenceID")],
                  #class = "compact",
                  rownames = FALSE, #To remove the column '0' with numbering
                  #colnames=c("Species name", "Local name"),
                  #or , colnames=c("Species name" = "species_short")
                  options=list(pageLength=50, #searching=FALSE,
                            order = list(list(1,'asc'),list(2,'asc'),list(5,'asc'),list(4,'asc')) #To order columns
                  ))
  })

  output$mytable_nearby <- renderDataTable({
    latdd <- if (!is.null(input$lat) && input$accuracy<30) {input$lat} else {input$ny}
    lngdd <- if (!is.null(input$long) && input$accuracy<30) {input$long} else {input$nx} #{55.6515}
    distdd <- input$dist / 111139
    SDcrop <- st_crop(SD2,xmin=lngdd-distdd, ymin=latdd-distdd, xmax=lngdd+distdd, ymax=latdd+distdd) %>% as.data.table()
    SDcrop[,c('scientificName', 'informationWithheld', 'locationRemarks', 'recordedBy', 'eventDate')]
    #sort(SDcropii$scientificName)
    })

  output$lat <- renderPrint({
    input$lat})

  output$long <- renderPrint({
    input$long})

  output$accuracy <- renderPrint({
    input$accuracy})

  output$time <- renderPrint({
    input$time})

  output$geolocation <- renderPrint({
    input$geolocation})

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$spname, ".geojson", sep = "")
    },
    content = function(file) {
      st_write(SD2[SD2$scientificName==input$spname,], file)#"MSNP.geojson", driver = "GeoJSON")
    }
  )

  }

shinyApp(ui = ui, server = server)
