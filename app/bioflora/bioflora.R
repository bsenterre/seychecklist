#bioflora version 1.2
#Shiny App giving an overview of the main data contained within the BIO database
#This version corrects a few issues related to misuse of data.table, and sums lacking the na.rm=TRUE
#To simplify, SP2acc has columns for isNative, isEndemic, isThreatenedNat, etc.
#SDint has also been properly cleaned for bad latitude longitude and inconsistent geographies

#Load libraries
library(data.table)
library(dplyr)
library(DT)
library(crosstable)
library(sf)
library(geojsonio) #To load geojson input data
library(mapview)
library(shiny)
library(leaflet)
library(janitor)
library(shinydashboard)
#library(imager)

##########1. Intro code (runs 1 time)
#Get input data
SP2acc = data.table::fread("SP2acc.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")

if (file.exists("SDint_sensitive.txt")) {
  SDint = data.table::fread("SDint_sensitive.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
} else {
  SDint = data.table::fread("SDint_obscured.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
}
#SDint = data.table::fread("SDint_obscured.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
#OR IF ACCESS TO THE SENSITIVE DATA
#SDint = data.table::fread("SDint_sensitive.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8")
#Take out some fields that will trouble when linking to SP2acc
SDint <- SDint[,!c("scientificName", "kingdom", "phylum", "class", "order", "family", "informationWithheld")]

KBA = sf::st_read("Seychelles_KBA.shp", crs = 4326, quiet = TRUE)
#KBA <- subset(KBA, STATUSID != 8)
#to remove from the database the duplicated geometries corresponding to earlier/outdated outlines
#sf::sf_use_s2(FALSE) # To avoid an error message coming from some bug when group-summarizing or something
#KBA <- dplyr::summarise(dplyr::group_by(KBA, STATUSID, STATUS, ISLAND_GRO, PA_NAME))


#Get choice lists for Shiny
#scaleList <- c("National", "Inner/outer islands", "Island group", "Island", "Key Biodiversity Area", "Custom Area Of Interest")
scaleList <- c("national", "inner/outer islands"="tdwg4", "islandGroup", "island", "kba", "custom")
tdwg4List <- sort(unique(SDint[nchar(SDint$tdwg4)>2,]$tdwg4)) %>% as.data.frame() %>% rename(nametxt = 1)
tdwg4List$scale <- "tdwg4"
islandGroupList <- sort(unique(SDint[nchar(SDint$islandGroup)>2,]$islandGroup)) %>% as.data.frame() %>% rename(nametxt = 1)
islandGroupList$scale <- "islandGroup"
islandList <- sort(unique(SDint[nchar(SDint$island)>2,]$island)) %>% as.data.frame() %>% rename(nametxt = 1)
islandList$scale <- "island"
kbaList <- sort(unique(SDint[nchar(SDint$PA_NAME)>2,]$PA_NAME)) %>% as.data.frame() %>% rename(nametxt = 1)
kbaList$scale <- "kba"
elementOfFilterListAll <- rbind(tdwg4List, islandGroupList, islandList, kbaList)

#Calculate the SDint geoaccuracy stats for the whole dataset for each taxonID
SDgeoaccuracyNat <- SDint
SDgeoaccuracyNat$isToCoordinate <- ifelse(SDgeoaccuracyNat$locationRemarks == "toCoordinate",1,0)
SDgeoaccuracyNat$isToLocality <- ifelse(SDgeoaccuracyNat$locationRemarks == "toLocality",1,0)
SDgeoaccuracyNat$isVague <- ifelse(SDgeoaccuracyNat$locationRemarks == "vague",1,0)
SDgeoaccuracyNat$isNA <- ifelse(nchar(SDgeoaccuracyNat$locationRemarks) > 1,0,1)
SDgeoaccuracyNat <- SDgeoaccuracyNat %>% dplyr::group_by(taxonID) %>% dplyr::summarize(toCoordNat = sum(isToCoordinate, na.rm=TRUE), toLocNat = sum(isToLocality, na.rm=TRUE), vagueNat = sum(isVague, na.rm=TRUE), isNANat = sum(isNA, na.rm=TRUE))
#To be replace by the next line to make sure no issue: SDgeoaccuracyNat$nObsNat <- SDgeoaccuracyNat$toCoordNat + SDgeoaccuracyNat$toLocNat + SDgeoaccuracyNat$vagueNat + SDgeoaccuracyNat$isNANat
SDgeoaccuracyNat <- SDgeoaccuracyNat %>% rowwise() %>% mutate(nObsNat = sum(toCoordNat, toLocNat, vagueNat, isNANat, na.rm = TRUE))

detailList <- c("General stats", "Species list", "Occurrence data", "Authors contributions", "Decadal stats",
                "Red List of species", "Red List of Ecosystems", "List of KBA triggers")

##########2. Shiny UI
# Then star with the 7.4 section: to be in the Shiny UI parameters
ui <- dashboardPage(
  dashboardHeader(title= "Flora of Seychelles"),
  dashboardSidebar(
    sidebarMenu(id="sidebar",
                selectInput(inputId="scaleSelect", label="Select the scale for your Area Of Interest", choices=scaleList, selected="national"),
                selectInput("elementOfFilter", "Pick one element of the list", choices=NULL),
                conditionalPanel(condition = "input.scaleSelect == 'custom'",
                  fileInput(inputId = "userDefinedAOI",
                            label = "Upload your own AOI (GEOJson file)",
                            multiple = FALSE,
                            accept = '.geojson')),
                menuItem("Map", tabName = "map"),
                menuItem("Species stats", tabName = "speciesStat"),
                menuItem("Species list", tabName = "speciesList"),
                menuItem("National Red list", tabName = "nationalRL"),
                menuItem("More species stats", tabName = "geoaccuracyStat"),
                menuItem("Contributors' stats", tabName = "authorStat"),
                menuItem("Other National stats", tabName = "otherStat")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "map",
              fluidRow(
                p("Map of the existing species distribution data for all species within the selected Area Of Interest (AOI). The legend indicates the geographic accuracy of the records."),
                p("To see the map for a given species:", tags$a(href="https://bsenterre.shinyapps.io/biodistrib/", "Click here!")),
                column(11.7,
                       leaflet::leafletOutput("mymap")#, width = "100%", height = "100%")#,
                       #imageOutput("myLogo")#img(src='Bio_logo.png', align = "left")
                ),
                #verbatimTextOutput("counter"),
                p("App version bioflora 1.2, based on ", tags$a(href="https://www.gbif.org/dataset/d0b7cc29-8ce3-49ad-b1b6-675fd39412ee", "seychecklist v1.6"))
              )
              #fillPage(leaflet::leafletOutput("mymap"), height = "100%")
      ),
      #Next tab item
      tabItem(tabName = "speciesStat",
              fluidRow(
                column(11.7,
                       DTOutput("mytable_speciesStat")
                )
              )
      ),
      #Next tab item
      tabItem(tabName = "speciesList",
              fluidRow(
                column(11.7,
                       DTOutput("mytable_speciesList")
                )
              )
      ),
      #Next tab item
      tabItem(tabName = "nationalRL",
              fluidRow(
                column(11.7,
                       DTOutput("mytable_nationalRL")
                )
              )
      ),
      tabItem(tabName = "geoaccuracyStat",
              fluidRow(
                column(11.7,
                       DTOutput("mytable_geoaccuracyStat")
                )
              )
      ),
      tabItem(tabName = "authorStat",
              fluidRow(
                column(11.7,
                       DTOutput("mytable_authorStat")
                )
              )
      ),
      tabItem(tabName = "otherStat",
              fluidRow(
                column(11.7,DTOutput("mytable_sensitiveSpecies")),
                br(),br(),
                column(11.7,DTOutput("mytable_syngbif")),
                br(),br(),
                column(11.7,DTOutput("mytable_rl2review"))
              )
      )
    ) #Closing the tabItems
  ) #Closing the body
) #Closing the UI dashboard page


##########3. Shiny Server
server <- function(session, input, output) {
  #Create a hit counter (number of visits to the page)
  output$counter <- renderText({
    if(!file.exists("appusage.Rdata"))
      hits <- 0
    else
      load(file="appusage.Rdata")
    hits <- hits + 1
    save(hits, file="appusage.Rdata")
    paste0("Number of visits (hits) since 1st September 2022: ", hits)
  })

  #Get the Map tabItem to load by default
  updateTabItems(session, inputId = "sidebar", selected = "map")

  #Load my Logo for BIO
  #output$myLogo <- renderImage({
  #  im <- load.image('Bio_logo.png', deleteFile=TRUE)
  #  plot(im)
  #})

#First I have to observe the scaleSelect user input and update the user dashboard with the appropriate list
  #of elementOfFilter to pick from
  observe({
    elementOfFilterList <- elementOfFilterListAll %>% filter(scale == input$scaleSelect) %>% dplyr::select(nametxt)
    updateSelectInput(session, "elementOfFilter", "Pick one element of the list", choices=unique(elementOfFilterList))
  })

#Then I need to get the SDintAOI as a reactive function's return, to be called like an input
  #, in another series of reactive functions producing the various tables
  SDintAOI_r <- reactive({
    if(input$scaleSelect == "national") {SDintAOI <- SDint}
    if(input$scaleSelect == "tdwg4") {SDintAOI <- SDint[SDint$tdwg4 == input$elementOfFilter,]}
    if(input$scaleSelect == "islandGroup") {SDintAOI <- SDint[SDint$islandGroup == input$elementOfFilter,]}
    if(input$scaleSelect == "island") {SDintAOI <- SDint[SDint$island == input$elementOfFilter,]}
    if(input$scaleSelect == "kba") {SDintAOI <- SDint[SDint$PA_NAME == input$elementOfFilter,]}
    if(input$scaleSelect == "custom" && !is.null(input$userDefinedAOI)) {
      #userAOIshape <- sf::st_as_sf(geojsonio::geojson_read("sey_cadastral_selection2.geojson",  what = "sp"), crs = 4326)
      userFiledf <- input$userDefinedAOI #dataframe with 1 row + 4 columns (name, datapath)
      userFiledf$directory <- dirname(paste(userFiledf[1,"datapath"]))
      #userFiledir <- paste(userFiledf[1,"datapath"])
      #directory <- dirname(userFiledir)
      userFile <- paste0(dirname(paste(userFiledf[1,"datapath"])), "/", paste("0.geojson"))
      #userFile <- paste0(userFile[1,"datapath"],"/",userFile[1,"name"])
      userAOIshape <- sf::st_as_sf(geojsonio::geojson_read(userFile,  what = "sp"), crs = 4326)
      userAOIshape$userAOI <- "userDefined"
      #sf::sf_use_s2(FALSE)
      userAOIshape <- dplyr::summarise(dplyr::group_by(userAOIshape, userAOI))
      SDintuserAOI <- SDint[SDint$locationRemarks=="toCoordinate" | SDint$locationRemarks=="toLocality",]
      SDintuserAOI <- SDintuserAOI[SDintuserAOI$decimalLatitude > 0 | SDintuserAOI$decimalLatitude < 0,]
      #when converting to sf the lat long will be lost, so I need these 2 lines
      SDintuserAOI$latitude <- SDintuserAOI$decimalLatitude
      SDintuserAOI$longitude <- SDintuserAOI$decimalLongitude
      SDintuserAOIshp <- sf::st_as_sf(x = SDintuserAOI, coords = c("longitude", "latitude"), crs=4326 )
      SDintAOI <- st_intersection(SDintuserAOIshp, userAOIshape) %>% as.data.table() %>%  dplyr::select(-geometry)
    }
    return(SDintAOI[SDintAOI$occurrenceStatus != "absent",])
  })

#Render the map (= just same as reactive function)
  output$mymap <- renderLeaflet({
    req(SDintAOI_r())#input$myCadList_rows_selected)
    SDintAOI <- SDintAOI_r()

    mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery"))

    #SDintAOI <- with(SDintAOI, SDintAOI[order(decimalLatitude) , ])
    #SDintAOI <- SDintAOI[1:8000,]#ifelse(nrow(SDintAOI)>3000,SDintAOI[1:3000,],SDintAOI)
    SDintAOIview <- SDintAOI[SDintAOI$decimalLatitude > 0 | SDintAOI$decimalLatitude < 0,]
    #SDintAOIview <- SDintAOIview[!is.na(decimalLatitude),]
    if(nrow(SDintAOIview) > 20000) {SDintAOIview <- SDintAOIview[1:20000,]}
    #SDintAOIview <- SDintAOIview[1:3000,]
    SDintAOIviewshp <- st_as_sf(x = SDintAOIview, coords = c("decimalLongitude", "decimalLatitude"), crs=4326 )
    SDintAOIviewshp <- SDintAOIviewshp[,c("occurrenceID","recordedBy","eventDate","basisOfRecord",
                                          "establishmentMeans","degreeOfEstablishment","locationRemarks","locality")]
    if(nrow(SDintAOIviewshp)>0) {
    map_mapview <- mapview(SDintAOIviewshp, zcol = "locationRemarks", cex = 4,layer.name="Species data geoaccuracy") #default cex is 8
    } else {map_mapview <- mapview()}

    if(input$scaleSelect == "custom" && !is.null(input$userDefinedAOI)) {
      userFiledf <- input$userDefinedAOI
      userFiledf$directory <- dirname(paste(userFiledf[1,"datapath"]))
      userFile <- paste0(dirname(paste(userFiledf[1,"datapath"])), "/", paste("0.geojson"))
      userAOIshape <- sf::st_as_sf(geojsonio::geojson_read(userFile,  what = "sp"), crs = 4326)
      map_mapview <- mapview(userAOIshape) + map_mapview}

    if(input$scaleSelect == "kba" && !is.null(input$elementOfFilter)) {
      KBAselected <- KBA[KBA$PA_NAME == input$elementOfFilter,]
      map_mapview <- mapview(KBAselected) + map_mapview}
    return(map_mapview@map)
  })

#Get Species list table as a reactive function
  speciesList_r <- reactive({
    SDintAOI <- SDintAOI_r()

    #Get local (AOI) establishmentMeans as a numeric value
    SDintx <- SDintAOI[SDintAOI$occurrenceStatus != "absent",]
    SDintx$establishmentMeansNumeric <- SDintx$establishmentMeans
    SDintx$establishmentMeansNumeric[SDintx$establishmentMeansNumeric == "introducedAssistedColonisation"] <- 1
    SDintx$establishmentMeansNumeric[SDintx$establishmentMeansNumeric == "introduced"] <- 1
    SDintx$establishmentMeansNumeric[SDintx$establishmentMeansNumeric == "native"] <- 2
    SDintx$establishmentMeansNumeric[SDintx$establishmentMeansNumeric == "nativeEndemic"] <- 3
    SDintx$establishmentMeansNumeric[SDintx$establishmentMeansNumeric == "nativeEndemicPaleo"] <- 4
    #Add the interpreted establishmentMeans within the selected AOI
    SDintxAOI <-SDintx %>% dplyr::group_by(taxonID) %>% dplyr::summarize(Orig=max(establishmentMeansNumeric), n = n())

    #Put back the taxonomic data from SP2acc (lost during group_by) (remove Rsp etc that were for national status)
    #SDintxAOI <- dplyr::left_join(SDintxAOI, SP2acc[,!c("RSp", "Rnative", "Rend", "RPend", "Rexo")], by = "taxonID")
    SDintxAOI <- dplyr::left_join(SDintxAOI, SP2acc, by = "taxonID")

    #Take the default local orig status according to national
    SDintxAOI$establishmentMeansAOI <- SDintxAOI$establishmentMeansNat
    #Replace orig status in case it is locally known only as introduced (max of Orig=1)
    SDintxAOI$establishmentMeansAOI[SDintxAOI$Orig == 1] <- "introduced"
    SDintxAOI$establishmentMeansBoth <- paste(SDintxAOI$establishmentMeansAOI, SDintxAOI$establishmentMeansNat, sep="|")

    #Do the same filtering of attributes and DT as for the Flora of Sey tab
    #SDintxAOITab <- SDintxAOI[,c("species_short", "vernacularName", "taxon_group_s", "synonyms", "establishmentMeansBoth", "degreeOfEstablishmentNat", "isInvasive", "threatStatusNat", "n")]
    #SDintxAOITab[order(SDintxAOITab$species_short),]
    #DT::datatable(SDintxAOITab[, 1:9])
    return(SDintxAOI)
  })

#Render the Species List
  output$mytable_speciesList <- renderDT({
    SDintxAOI <- speciesList_r()
    SDintxAOITab <- SDintxAOI[,c("species_short", "vernacularName", "taxon_group_s", "synonyms", "establishmentMeansBoth", "degreeOfEstablishmentNat", "isInvasive", "threatStatusNat", "n")]
    SDintxAOITab <- SDintxAOITab[order(SDintxAOITab$species_short),]
    DT::datatable(SDintxAOITab[, 1:9],rownames = FALSE, #options=list(pageLength=100),
                  extensions='Buttons',
                  options = list(pageLength=100, scrollX = TRUE,
                                 dom='Blfrtip', #Bfrtip is for buttons
                                 buttons =
                                   list(
                                     list(
                                       extend = 'excel', text = "Download to Excel",
                                       buttons = c('excel'),
                                       exportOptions = list(
                                         modifiers = list(page = "all")
                                       )
                                     )),
                                 #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                                 lengthMenu = list(c(10, 50, 100, -1),
                                                   c('10', '50', '100', 'all')),
                                 columnDefs = list(
                                   list(
                                     targets = 3,
                                     render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 10 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                                       "}")))),
                  escape = FALSE,
                  caption="List of species recorded from the selected Area Of Interest (AOI), showing the origin status (establishmentMeans) at both the AOI (left) and National (right) scales, as well as the degree of establishment (spread or invasion status).",
                  colnames=c("Taxonomic group"="taxon_group_s", "Origin (AOI|Nat)"="establishmentMeansBoth", "Spread"="degreeOfEstablishmentNat")
                  ) #class = "compact")
  })

  #Get main stats on species origins per taxonomic group as a reactive function
  speciesStat_r <- reactive({
    SDintxAOIStat <- speciesList_r()
    SDintxAOIStat$RSp <- 1
    #Outdate:SDintxAOIStat$Rnative <- ifelse(stringr::str_detect(SDintxAOIStat$establishmentMeansAOI, "native"), 1, 0)
    #Outdate:SDintxAOIStat$Rend <- ifelse(stringr::str_detect(SDintxAOIStat$establishmentMeansAOI, "Endemic"), 1, 0)
    #Outdate:SDintxAOIStat$RPend <- ifelse(stringr::str_detect(SDintxAOIStat$establishmentMeansAOI, "EndemicPaleo"), 1, 0)
    #Outdate:SDintxAOIStat$Rexo <- ifelse(stringr::str_detect(SDintxAOIStat$establishmentMeansAOI, "introduced"), 1, 0)

    #Outdate:StatAOI <- SDintxAOIStat %>% dplyr::group_by(taxon_group_concat) %>% dplyr::summarize(Total=sum(RSp), introduced=sum(Rexo), nativeAll = sum(Rnative), nativeEndemic=sum(Rend), nativePaleoEndemic=sum(RPend))
    StatAOI <- SDintxAOIStat %>% dplyr::group_by(taxon_group_concat) %>% dplyr::summarize(Total=sum(RSp, na.rm=TRUE), introduced=sum(isIntroduced, na.rm=TRUE), nativeAll = sum(isNative, na.rm=TRUE), nativeEndemic=sum(isEndemic, na.rm=TRUE), nativePaleoEndemic=sum(isEndemicPaleo, na.rm=TRUE))
    #StatAOI$EndRate <- round(StatAOI$nativeEndemic / StatAOI$nativeAll * 100, digits = 1)
    StatAOI <- data.table::setDT(StatAOI)
    StatAOI <- transform(janitor::adorn_totals(StatAOI), EndRate = round(nativeEndemic/nativeAll * 100, digits = 1))
    #DT::datatable(StatAOI[, 1:7])
    return(StatAOI)
  })

#Render the Species Stat
  output$mytable_speciesStat <- renderDT({
    StatAOI <- speciesStat_r()
    DT::datatable(StatAOI[, 1:7], colnames=c("Taxonomic group" = "taxon_group_concat"),
                  extensions='Buttons',
                  options = list(dom='Blfrtip', #Bfrtip is for buttons
                                 buttons =
                                   list(
                                     list(
                                       extend = 'excel', text = "Download to Excel",
                                       buttons = c('excel'),
                                       exportOptions = list(
                                         modifiers = list(page = "all")
                                       )
                                     )),
                                 lengthMenu = list(c(10, 50, 100, -1),
                                                   c('10', '50', '100', 'all'))
                                 ),
                                 #buttons=c('copy', 'csv' ,'excel')), #last bit for buttons
                  caption="Number of species in the flora of Seychelles, per taxonomic group and per types of origin (or establishmentMeans). The endemism rate (EndRate) is also indicated for each group.")
  })

#Now I want to built another speciesList table but less taxonomic and more conservation-oriented
  #I want just the full scientificName, vernacName, origNat+AOI, isInv, iucnNat+Glo, sensitive, KBA trigger,
  #firstYear, lastYear, geoaccuracy stats for each species
  output$mytable_nationalRL <- renderDT({
    spListAOI <- speciesList_r()
    SP2acc_rl <- spListAOI[spListAOI$threatStatusNat == "EX" | spListAOI$threatStatusNat == "CR" | spListAOI$threatStatusNat == "EN" | spListAOI$threatStatusNat == "VU",] # & spListAOI$establismentMeansNat != "introduced",]
    SP2acc_rl <- SP2acc_rl[,c("species_short", "vernacularName", "taxon_group_s", "synonyms", "establishmentMeansNat", "threatStatusNat", "threatStatusGlobal", "informationWithheld")]
    SP2acc_rl <- SP2acc_rl[order(SP2acc_rl$threatStatusNat, SP2acc_rl$species_short),]
    return(DT::datatable(SP2acc_rl[, 1:8], #class = "compact",
                         rownames = FALSE, #options=list(pageLength=100),
                         extensions='Buttons',
                         options = list(pageLength=100, scrollX = TRUE,
                                        dom='Blfrtip', #Bfrtip is for buttons
                                        buttons =
                                          list(
                                            list(
                                              extend = 'excel', text = "Download to Excel",
                                              buttons = c('excel'),
                                              exportOptions = list(
                                                modifiers = list(page = "all")
                                              )
                                            )),
                                        #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                                        lengthMenu = list(c(10, 50, 100, -1),
                                                          c('10', '50', '100', 'all')),
                                        columnDefs = list(
                                          list(
                                            targets = 3,
                                            render = JS(
                                              "function(data, type, row, meta) {",
                                              "return type === 'display' && data.length > 10 ?",
                                              "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                                              "}")))),
                         escape = FALSE,
                         caption="List of species from the Seychelles National Red List that have been recorded in the selected Area Of Interest (AOI), showing the National and Global Red List statuses.",
                         colnames=c("Taxonomic group" = "taxon_group_s")
                         )
           )
    })

#Get another tab with more species stats, especially relevant for KBAs
  output$mytable_geoaccuracyStat <- renderDT({
    #Gettin basic distribution data based on occurrence excluding 'absence' records and SPdistrib records
    SDintxtmp <- SDintAOI_r()
    SDintxtmp$isToCoordinate <- ifelse(SDintxtmp$locationRemarks == "toCoordinate",1,0)
    SDintxtmp$isToLocality <- ifelse(SDintxtmp$locationRemarks == "toLocality",1,0)
    SDintxtmp$isVague <- ifelse(SDintxtmp$locationRemarks == "vague",1,0)
    SDintxtmp$isNA <- ifelse(nchar(SDintxtmp$locationRemarks) > 1,0,1)
    SDgeoaccuracyAOI <- SDintxtmp %>% dplyr::group_by(taxonID) %>% dplyr::summarize(toCoord = sum(isToCoordinate, na.rm=TRUE), toLoc = sum(isToLocality, na.rm=TRUE), vague = sum(isVague, na.rm=TRUE), isNA = sum(isNA, na.rm=TRUE))
    #replaced by below:SDgeoaccuracyAOI$nObs <- SDgeoaccuracyAOI$toCoord + SDgeoaccuracyAOI$toLoc + SDgeoaccuracyAOI$vague + SDgeoaccuracyAOI$isNA
    SDgeoaccuracyAOI <- SDgeoaccuracyAOI %>% rowwise() %>% mutate(nObs = sum(toCoord, toLoc, vague, isNA, na.rm = TRUE))
    #tdwg4KBA2022Stats <- tdwg4KBA2022Stats %>% rowwise() %>% mutate(PAPlus2022 = sum(PA, Proposed2022, na.rm = TRUE))

    #Add the first and last Year of occurrence record in AOI (#xxOR use dplyr first last??)
    SDintxtmp$year <- as.numeric(substr(SDintxtmp$eventDate, 1, 4))
    SDintfirstYear <- aggregate(x = year ~ taxonID, data = SDintxtmp, FUN = min) %>% rename(firstYear=year)
    SDintlastYear <- aggregate(x = year ~ taxonID, data = SDintxtmp, FUN = max) %>% rename(lastYear=year)

    #Now I can put geoaccuracy and years data back with the speciesList of the AOI
    NatTab2 <- dplyr::left_join(speciesList_r(), SDgeoaccuracyAOI, by = "taxonID")
    NatTab2 <- dplyr::left_join(NatTab2, SDintfirstYear, by = "taxonID")
    NatTab2 <- dplyr::left_join(NatTab2, SDintlastYear, by = "taxonID")

    #Add the national count toCoord and Total
    NatTab2 <- dplyr::left_join(NatTab2,  SDgeoaccuracyNat, by = "taxonID")
    NatTab2$ratio <- round((as.numeric(NatTab2$nObs)/as.numeric(NatTab2$nObsNat))*100, digits = 1)

    #Get the table to renderDT
    NatTab2 <- NatTab2[,c("species_short", "vernacularName", "occurrenceStatusNat", "toCoord", "toLoc", "vague", "isNA", "nObs", "nObsNat", "ratio", "firstYear", "lastYear")] %>% rename(occStatus=occurrenceStatusNat)
    NatTab2 <- NatTab2[order(NatTab2$species_short),]
    DT::datatable(NatTab2[, 1:12],
                  extensions='Buttons',
                  options = list(dom='Blfrtip', #Bfrtip is for buttons
                                 buttons =
                                   list(
                                     list(
                                       extend = 'excel', text = "Download to Excel",
                                       buttons = c('excel'),
                                       exportOptions = list(
                                         modifiers = list(page = "all")
                                       )
                                     )),
                                 #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                                 lengthMenu = list(c(10, 50, 100, -1),
                                                   c('10', '50', '100', 'all'))

                                ),
                  caption="More detailed statistics on the existing data for each species depending on the geographic accuracy of occurrences. The ratio of the number of observations for the Area Of Interest (nObs) over the total number of observations in Seychelles (nObsNat) is provided, as well as the first and last year of observation (according to the available data)."
                  )
  })

#Then I want less critical stats, on the authors contribution and on the decade contributions
  output$mytable_authorStat <- renderDT({
    #Extract main author from recordedBy
    x <- SDintAOI_r()
    SDintAuthorsData <-x[nchar(x$recordedBy) > 2 & !is.na(x$recordedBy) & nchar(x$occurrenceID) >1, c("occurrenceID", "recordedBy")]
    if(nrow(SDintAuthorsData) > 0) {
      SDintAuthors <- splitstackshape::cSplit(SDintAuthorsData, "recordedBy", "|", type.convert = FALSE)
      SDintAuthors <- SDintAuthors[,1:2]
      SDintAuthors <- left_join(x, SDintAuthors, by = "occurrenceID")
    } else {
      SDintAuthors <- x
      SDintAuthors$recordedBy_1 <- "noData"
    }

    #I tried making it work with crosstable::crosstable (nice) but it does not work, so I do just like for geoaccuracy
    SDintAuthors$isToCoordinate <- ifelse(SDintAuthors$locationRemarks == "toCoordinate",1,0)
    SDintAuthors$isToLocality <- ifelse(SDintAuthors$locationRemarks == "toLocality",1,0)
    SDintAuthors$isVague <- ifelse(SDintAuthors$locationRemarks == "vague",1,0)
    SDintAuthors$isNA <- ifelse(nchar(SDintAuthors$locationRemarks) > 1,0,1)
    SDintAuthorsGeoaccuracy <- SDintAuthors %>% dplyr::group_by(recordedBy_1) %>% dplyr::summarize(toCoordinate = sum(isToCoordinate, na.rm=TRUE), toLocality = sum(isToLocality, na.rm=TRUE), vague = sum(isVague, na.rm=TRUE), isNA = sum(isNA, na.rm=TRUE))
    SDintAuthorsGeoaccuracy <- SDintAuthorsGeoaccuracy %>% rename(Contributor = recordedBy_1)
    #Replaced by line below:SDintAuthorsGeoaccuracy$Total <- SDintAuthorsGeoaccuracy$toCoordinate + SDintAuthorsGeoaccuracy$toLocality + SDintAuthorsGeoaccuracy$vague + SDintAuthorsGeoaccuracy$isNA
    SDintAuthorsGeoaccuracy <- SDintAuthorsGeoaccuracy %>% rowwise() %>% mutate(Total = sum(toCoordinate, toLocality, vague, isNA, na.rm = TRUE))
    SDintAuthorsGeoaccuracy <- SDintAuthorsGeoaccuracy %>% dplyr::arrange(desc(as.numeric(Total)))
    totalToCoord <- SDintAuthorsGeoaccuracy %>% dplyr::group_by() %>% dplyr::summarize(Total = sum(toCoordinate, na.rm=TRUE))
    totalToCoord <- totalToCoord$Total
    SDintAuthorsGeoaccuracy$toCoordP <- round(SDintAuthorsGeoaccuracy$toCoordinate * 100 / totalToCoord, digits=1)
    SDintAuthorsGeoaccuracy$toCoordinate <- paste0(SDintAuthorsGeoaccuracy$toCoordinate, " (", SDintAuthorsGeoaccuracy$toCoordP, " %)")
    SDintAuthorsGeoaccuracyTab <- SDintAuthorsGeoaccuracy[,c("Contributor", "toCoordinate", "toLocality", "vague", "isNA", "Total")]

    DT::datatable(SDintAuthorsGeoaccuracyTab[, 1:6], rownames = FALSE, options=list(pageLength=50),
                  caption="Number of species occurrence data recorded by the different contributors at the different levels of geographic accuracy (or unknown accuracy: ‘NA’)"
                  ) #class = "compact",
  })


#Then, I want a tabItem showing me the data that require attention, such as doubtfull taxa, new ones, GBIF-BIO disagreements,
  #iucnGlo-Nat disagreements: To improve performence, we use the National data rather that the SDintAOI

  #xxx get the renderDT here for each and add 1 tabItem with the 3 tables vertically
  output$mytable_sensitiveSpecies <- renderDT({
    #List of sensitive taxa
    SP2acc_sensitive <- SP2acc %>% dplyr::filter(grepl('sensitiv', informationWithheld))
    DT::datatable(SP2acc_sensitive[, c("species_short", "vernacularName")],
                  class = "compact", rownames = FALSE,
                  #colnames=c("Species name", "Local name"),
                  extensions='Buttons',
                  options=list(pageLength=10,
                               dom='Blfrtip', #Bfrtip is for buttons
                               buttons =
                                 list(
                                   list(
                                     extend = 'excel', text = "Download to Excel",
                                     buttons = c('excel'),
                                     exportOptions = list(
                                       modifiers = list(page = "all")
                                     )
                                   )),
                               #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                               lengthMenu = list(c(10, 50, 100, -1),
                                                 c('10', '50', '100', 'all')),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                  caption="List of plant species considered 'sensitive' (obscured coordinates) for the Flora of Seychelles")
    })

  output$mytable_syngbif <- renderDT({
    #List of Accepted names in BIO that are synonyms in GBIF
    SP2acc_syngbif <- SP2acc[SP2acc$gbifStatus != "ACCEPTED"]
    SP2acc_syngbif <- SP2acc_syngbif[order(SP2acc_syngbif$species_short),]
    SP2acc_syngbif$gbifLink <-ifelse(nchar(SP2acc_syngbif$gbifID)>1,  paste0("<a href='","https://www.gbif.org/species/", SP2acc_syngbif$gbifID,"'>","https://www.gbif.org/species/", SP2acc_syngbif$gbifID,"</a>"), "")
    DT::datatable(SP2acc_syngbif[, c("scientificName", "gbifLink")],
                  escape = FALSE, #For him to understand the html bit as DT does not normally (as opposed to shiny::datatable)
                  class = "compact", rownames = FALSE,
                  extensions='Buttons',
                  options=list(pageLength=10,
                               dom='Blfrtip', #Bfrtip is for buttons
                               buttons =
                                 list(
                                   list(
                                     extend = 'excel', text = "Download to Excel",
                                     buttons = c('excel'),
                                     exportOptions = list(
                                       modifiers = list(page = "all")
                                     )
                                   )),
                               #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                               lengthMenu = list(c(10, 50, 100, -1),
                                                 c('10', '50', '100', 'all')),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                  caption="List of plant species names considered accepted in BIO but synonyms or absent in GBIF")
  })

  output$mytable_rl2review <- renderDT({
    #List of threatened species that have different threat status Nationally and Globally
    SP2acc_rl2review <- SP2acc[SP2acc$threatStatusNat != SP2acc$threatStatusGlobal | is.na(SP2acc$threatStatusGlobal),]
    SP2acc_rl2review <- SP2acc_rl2review[order(SP2acc_rl2review$species_short),]
    DT::datatable(SP2acc_rl2review[, c("species_short", "threatStatusNat", "threatStatusGlobal")],
                  class = "compact", rownames = FALSE,
                  extensions='Buttons',
                  options=list(pageLength=10,
                               dom='Blfrtip', #Bfrtip is for buttons
                               buttons =
                                 list(
                                   list(
                                     extend = 'excel', text = "Download to Excel",
                                     buttons = c('excel'),
                                     exportOptions = list(
                                       modifiers = list(page = "all")
                                     )
                                   )),
                               #buttons=c('copy', 'csv' ,'excel'), #last bit for buttons
                               lengthMenu = list(c(10, 50, 100, -1),
                                                 c('10', '50', '100', 'all')),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                  caption="List of plant species having a National Red List status different from the Global status")
  })

#Finally, I want an ignorance map ## biodiversity map ## Ecosystem maps and red lists xxxetc ...

}
shinyApp(ui = ui, server = server)


#Solve establishmentMeansBoth
#Find a way to abbreviate (hide with '...' the long 'synonyms', so I can still search them)

#2 stuff to solve in the script producing SDint
#Solve island names issues (Aride, etc.)
#Get an option to make it run by uploading the sensitive txt file (so that never stored on server)

#Get the species ignorance map

#Then develop another shiny for ecosystems AND one for KBAs

