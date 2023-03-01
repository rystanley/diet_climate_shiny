### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for tab 2
### ----------------------------------------------
### ----------------------------------------------



Tab2 <- function(dat2){
  
  fluidPage(
    
    fluidRow( 
      
      box(width = 12,
          status="info",
          #background = "green",
          column(width=4,
                 h4("Compare Models"),
                 "In this study, we tested whether including predator density or estimated predation rates as covariates in species distribution models were able to substantially improve the accuracy of range predictions. Use this tab to explore differences between our three model scenarios. "
          ),
          
          # box 1 - species and year
          column(width=4,
                 # Widget specifying the species to be included on the plot
                 
                 radioGroupButtons(
                   inputId = "species2",
                   label = "Target Species",
                   choiceNames  = list("Northern Shrimp", 
                                       "Snow Crab"),
                   choiceValues = list("shrimp","crab"),
                   selected = "crab",
                   justified = TRUE,
                   status="primary"
                 ),
                 
                 
                 # Slider widget for year
                 sliderInput("year2", 
                             label = "Projection Year", 
                             min = 1995, 
                             max = 2018, 
                             value = 2000,
                             sep="")
          ), # end box 1
          
          
          
          # box 2 - custom threshold
          column(width=4,
                 style = "padding-top:23px;",
                 # Check to specify threshold
                 switchInput(
                   inputId = "custom2",
                   label = "Custom Threshold", 
                   labelWidth = "100%",
                   
                 ),
                 
                 # make a threshold slider
                 sliderInput("dynamic_slider2", label = "Custom Range", min = 0, 
                             max = 250, 
                             value = c(20, 250))
          ), # end box 2
          
          
          
      )
    ),
    
    # row 2 - maps
    fluidRow(
      
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map2_1',
                           height = "55vh"),
             
      ),
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map2_2',
                           height = "55vh"),
             
      ),
      # make wider column for map
      column(width = 4,
             # Leaflet map
             leafletOutput('map2_3',
                           height = "55vh"),
             
      ),
      
      
      
    ), # end row 2
    
    br(),
    
    fluidRow(
      box(status= "primary",
          width = 12,
          
          checkboxGroupButtons(
            inputId = "shapefiles2",
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
    )
    
    
  )
}




