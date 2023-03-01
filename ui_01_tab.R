### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for tab 1
### ----------------------------------------------
### ----------------------------------------------



Tab1 <- function(dat){
  fluidRow(
    
    # make inputs box
    column(width = 3,
           box(
             #background = "green",
             title = "Improving Projections", width = NULL, status = "primary",
             "Traditional species distribution models project species ranges under future environmental conditions,
               but often exclude biotic interactions that may be important in setting range boundaries.
               Here, we show how biotic and abiotic factors influence range projections of two decapods 
               in the Canadian Atlantic.",
             
             # insert blank box to skip a line
             #div(style="height: 35px;"),
             br(),br(),
             
             # Widget specifying the species to be included on the plot
             radioGroupButtons(
               # style="padding: 50px",
               inputId = "species",
               label = "Target Species",
               choiceNames  = list("Northern Shrimp", 
                                   "Snow Crab"),
               choiceValues = list("shrimp","crab"),
               selected = "crab",
               justified = TRUE,
               status= "primary"
             ),
             
             # Widget specifying the variable to be plotted
             selectInput("model",
                         "Model Scenario",
                         choices  = list("Climate only" = "1",
                                         "Climate + Predator Density" = "2",
                                         "Climate + Predator Index" = "3"),
                         selected = "1",
             ),
             
             # Slider widget for year
             sliderInput("year", 
                         label = "Projection Year", min = 1995, 
                         max = 2018, value = 2000,
                         sep=""),
             
             
             # insert blank box to skip a line
             #div(style="height: 35px;"),
             br(),br(),
             
             # Check to specify threshold
             switchInput(
               inputId = "custom",
               label = "Custom Threshold", 
               labelWidth = "100%"
             ),
             
             
             # try with an updateinput slider instead - Mar 14 solution
             # try updateinput slider
             sliderInput("dynamic_slider",
                         "Threshold Slider",
                         min=0,
                         max=250,
                         value= c(20, 160)),
             
           ), # end box
           
           box(width=NULL, status="primary", style="padding-top:0px; padding-bottom:0px",
               
               h6("Zabihi-Seissan, S., Baker, K., Stanley, R.R.E, Tunny, T., et. al. (2021), Interactive effects of  predation  and climate on the distributions of marine shellfish in the Northwest Atlantic. In Press"))
    ),
    
    # make wider column for map
    column(width = 9,
           style = "margin: 0px;",
           
           
           fluidRow(
             # Leaflet map
             leafletOutput('map', height = "75vh"),
           ),
           
           br(),
           
           fluidRow(
             
             box(width=12,
                 status="primary",
                 style = "margin: 0px;",
                 
                 
                 
                 checkboxGroupButtons(
                   inputId = "shapefiles",
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
           
    ),
    
  )
  
  
}




