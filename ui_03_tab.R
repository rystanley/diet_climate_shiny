### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for tab 3
### ----------------------------------------------
### ----------------------------------------------



Tab3 <- function(dat3){
  
  fluidPage(
    
    fluidRow( 
      status= "primary",
      
      box(width = 12,
          #background = "green",
          column(width=3,
                 h4("Compare Years"),
                 "Decapod abundances have varied over both time and space on the Canadian Atlantic Coast. Use this tab to compare temporal changes between groups of years using our three model outputs."
          ),
          
          column(width=9,
                 
                 
                 
                 fluidRow(style="display: inline-block;vertical-align:middle;",
                          
                          column(width=4,
                                 radioGroupButtons(
                                   inputId = "species3",
                                   label = "Target Species",
                                   choiceNames  = list("Northern Shrimp", 
                                                       "Snow Crab"),
                                   choiceValues = list("shrimp","crab"),
                                   selected = "crab",
                                   justified = TRUE,
                                   status="primary"
                                 )
                          ),
                          
                          column(width=4,
                                 sliderInput("year3_1.2", 
                                             label = "Year Group 1", 
                                             min = 1995, 
                                             max = 2018, 
                                             value = c(1996,2000),
                                             sep="",
                                 )
                          ),
                          column(width=4,
                                 sliderInput("year3_2.2", 
                                             label = "Year Group 2", 
                                             min = 1995, 
                                             max = 2018, 
                                             value = c(2010,2014),
                                             sep="")
                          ),
                          
                          
                 ),
                 
                 fluidRow(
                   column(width=4, selectInput("model3", "Model Scenario",  choices  = list("Climate only" = "1", "Climate + Predator Density" = "2", "Climate + Predator Index" = "3"),  selected = "1",)),
                   column(width=4,radioGroupButtons(inputId = "map3_showme", label = "Show Me", choiceNames= c("Total Change",  "Percent Change"),choiceValues = list("total","percent"), direction = "horizontal",status="info", selected = "total")),
                   column(width=4,div("Project change between the mean of years. Show", HTML("<b>total change</b>"), "in kg/km2, or as", HTML("<b>percent change</b>"), "capped at 200% increase.", ))
                 ),
                 
          )
      )
    ),
    
    # row 2 - maps
    fluidRow(
      
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map3_1',
                           height = "55vh"),
             
      ),
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map3_2',
                           height = "55vh"),
             
      ),
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map3_3',
                           height = "55vh"),
             
      ),
      
    ), # end map fluid row
    
    br(),
    
    # begin polygon check box fluidrow
    fluidRow(
      box(      status= "primary",
                width = 12,
                
                checkboxGroupButtons(
                  inputId = "shapefiles3",
                  individual = TRUE,
                  label = "Show Polygons",
                  choiceNames = list("Shrimp Fishing Areas", "DFO Regions", "Marine Protected Areas",
                                     "NAFO Zones", "OECM Areas", "Bioregions"),
                  choiceValues = list("sfa","dfo","mpa",'nafo',"oecm","bioregions"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                )
      )
    ) # end polygons fluid row
    
  )
}




