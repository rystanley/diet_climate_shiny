#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(raster)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(shinyWidgets)
library(leafsync)
library(leaflet.minicharts)



# Source tabs -------------------------------------------------------------
# source data upload tab
source("00_initialize_app.R")

# The content for each tab is stored in a separate file. Source all .R files in the current directory that start with "ui_":
sapply(list.files(
    pattern = "^ui_.*\\.R$",
    path = ".",
    full.names = TRUE
),
source)


# Make Sidebar ------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("View Model", tabName = "tab1", icon = icon("dashboard")),
        menuItem("Compare Models", tabName = "tab2", icon = icon("clone")),
        menuItem("Compare Years", tabName = "tab3", icon = icon("calendar"))
        
    ),
    collapsed = TRUE
)



# Make Body ---------------------------------------------------------------


body <- dashboardBody(
    tags$head(tags$style(".leaflet-top {z-index:999!important;}")),
    
    tabItems(
        
        ### Tab 1 content ---
        tabItem(tabName = "tab1",
                Tab1(dat)
        ),
        
        # Second tab content
        tabItem(tabName = "tab2",
                Tab2(dat2)
        ),
        
        # third tab content
        tabItem(tabName = "tab3",
                Tab3(dat3)
        )
    )
)







# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(
        title = "Projecting Distributions of Canadian Fisheries",
        titleWidth = 450,
        
        
        tags$li(
            class = "dropdown",
            # change link in logo here
            a(href = 'https://www.dfo-mpo.gc.ca/contact/regions/maritimes-eng.html', # add link for DFO
              img(src = 'DFO_logo_no_back.png', title = "dfologo", height = "50px"), 
              style = "padding-top:0px; padding-bottom:0px;"
            ) # /a
        )
    ),
    
    
    sidebar,
    body
    
)

