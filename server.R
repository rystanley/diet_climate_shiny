# This is a Shiny App for DFO Canada Project ___________
# Developed by Jake Lawlor - jake.lawlor@mail.mcgill.ca

# Porjecting Distributions of Canadian Fisheries - 
# Hindcasted Species Distribution Model Outputs
# using 3 model scenarios for two species of Canadian arthropods



# Libraries ---------------------------------------------------------------
options(warn=-1) 
# this shiny throws some palette warnings about numbers outside palette thresholds 
# due to the palette trying to change before the species reacts, so 
# I'm just going to suppress warnings. They don't affect the map
# use options(warn=0) to turn back on
options("rgdal_show_exportToProj4_warnings"="none") # mute warnings from rgdal because I'm using proj strings
library(shiny)
library(dplyr)
library(raster)

# Start Server ------------------------------------------------------------
shinyServer(function(input, output, session) {
    
    
    
    
    ###  --- --- --- --- --- --- --- --- --- --- --- -
    ### Tab 1 ----------------------------
    ### --- --- --- --- --- --- --- --- --- --- --- -
    
    # set species density limits reactive to species selection
    density_lim <- reactive({
        switch(input$species,
               "crab" = c(0:250),
               "shrimp" = c(0:25000)
        )
    })
    # change slider limits to reflect species density limits when species is changed
    observeEvent(input$species, {
        updateSliderInput(session,'dynamic_slider', 
                          max=max(density_lim()),
                          # value = c(max(density_lim())*.25, max(density_lim())*.75)
        )
    })
    
    
    # create a color palette for the map
    map_pal <- reactiveValues() 
    observe({
        
        # if custom threshold IS NOT selected, use a viridis palette
        if (input$custom == "FALSE"){
            map_pal$pal <- colorNumeric(palette = "plasma", 
                                        density_lim(),
                                        na.color = "transparent",
                                        reverse=F)
            
        } else{
            # if custom threshold IS selected, build a palette of grey / purple / grey
            # using threshold limits set by threshold slider inputs
            req(input$dynamic_slider[1],input$dynamic_slider[2],density_lim())
            
            # create a dummy variable to prevent the case when the "times" argument is more than the species allows
            # (this would happen when you switch from high-limit species to low-limit species)
            mytimes <- ifelse((max(density_lim())+1)>input$dynamic_slider[2] , max(density_lim())-input$dynamic_slider[2], input$dynamic_slider[2])
            map_pal$pal <-  colorNumeric(palette = c(rep(c("grey70"), times = input$dynamic_slider[1]),
                                                     rep(c("purple"), times = (input$dynamic_slider[2]-input$dynamic_slider[1])),
                                                     rep(c("grey70"), times = mytimes) ), 
                                         domain = density_lim(),
                                         na.color="transparent",
                                         reverse=F)
            
        }
    })
    
    
    # make a reverse palette to use for leaflet legend
    # (necessary because leaflet default legend is upside down and we don't want that)
    map_pal_rev <- reactiveValues()
    observe({
        
        if (input$custom == "FALSE"){
            map_pal_rev$pal <- colorNumeric(palette = "plasma", 
                                            density_lim(),
                                            na.color = "transparent",
                                            reverse=T)
            
        } else{ # then with set thresholds
            req(input$dynamic_slider[1],input$dynamic_slider[2],density_lim())
            mytimes2 <- ifelse((max(density_lim())+1)>input$dynamic_slider[2] , max(density_lim())-input$dynamic_slider[2], input$dynamic_slider[2])
            map_pal_rev$pal <-  colorNumeric(palette = c(rep(c("grey70"), times = input$dynamic_slider[1]),
                                                         rep(c("purple"), times = (input$dynamic_slider[2]-input$dynamic_slider[1])),
                                                         rep(c("grey70"), times = mytimes2 )  ), 
                                             domain = density_lim(),
                                             na.color="transparent",
                                             reverse=T)
            
            
        }
    })
    
    
    # make reactive data object to constantly pull columns with the correct
    # species, year, and model number from the main dataset
    map_dat <- reactive({
        get(input$species) %>%
            dplyr::select(x,y,paste0("X",input$year,".model",input$model)) %>%
            rename(output = paste0("X",input$year,".model",input$model)) 
    })
    
    # rasterize the map
    map_dat_raster <- reactiveValues()
    observe({
        map_dat_raster$raster <-  rasterFromXYZ(map_dat(),
                                                crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
    })
    

    
    #### start map ---------------------------------------------------------------
    output$map <- renderLeaflet({
        
        # add blank leaflet map 
        leaflet('map', options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE)) %>%
            addProviderTiles("CartoDB.VoyagerNoLabels") %>%
            setView(lng = -60, lat = 51, zoom = 4) 
        
    }) # end render map
    
    
    # add proxy for showing raster object
    observe({
        # this require statement prevents a glitch when switching species.
        req(input$dynamic_slider[2]<(max(density_lim()))+1) 
        
        # set palette and data for raster object
        pal <-   map_pal$pal
        dfr <- map_dat_raster$raster
        
        leafletProxy("map") %>%
            clearImages() %>% 
            addRasterImage(dfr, colors = pal, opacity = 0.7,
                           project=TRUE, group = "output",
                           layerId = "output") 
    })
    
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        # again, this require statement prevents a glitch when switching species
        req(input$dynamic_slider[2]<(max(density_lim()))+1) 
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        pal <- map_pal_rev$pal
        leafletProxy("map") %>%
            clearControls() %>%
            addLegend(position = "bottomright",
                      pal = pal, 
                      values = density_lim(),
                      title = paste0(stringr::str_to_title(input$species)," density<br>(kg/km2)"),
                      opacity = 1,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)
                      )
                      
            )
    })
    
    
    # add observe to add polygons by proxy
    
    # add polygons for SFAs
    observe({
        req("sfa" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            # fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE), label = ~NOM_ZONE,
                        group = "SFAs" ) %>%
            showGroup("SFAs")
    })
    # remove SFA polygons
    observe({
        req(!("sfa" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("SFAs")
    })
    
    
    # add polygons for OECMs
    observe({
        req("oecm" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),
                        label = ~NAME_E, group = "OECMs") %>%
            showGroup("OECMs")
    })
    # remove OECM polygons
    observe({
        req(!("oecm" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("OECMs")
    })
    
    
    # add polygons for MPAs
    observe({
        req("mpa" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~NAME_E,
                        group = "MPAs") %>%
            showGroup("MPAs") 
    })
    # remove MPA polygons
    observe({
        req(!("mpa" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("MPAs")
    })
    
    # add polygons for DFO regions
    observe({
        req("dfo" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~region,
                        group = "DFOs") %>%
            showGroup("DFOs") 
    })
    # remove DFO regions
    observe({
        req(!("dfo" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("DFOs")
    })
    
    
    # add polygons for NAFO regions
    observe({
        req("nafo" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~ZONE,
                        group = "NAFOs") %>%
            showGroup("NAFOs") 
    })
    # remove NAFO regions
    observe({
        req(!("nafo" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("NAFOs")
    })
    
    # add polygons for bioregions
    observe({
        req("bioregions" %in% input$shapefiles)
        leafletProxy("map") %>%
            addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~Legend,
                        group = "BIOREGIONS") %>%
            showGroup("BIOREGIONS") 
    })
    # remove bioregions
    observe({
        req(!("bioregions" %in% input$shapefiles))
        leafletProxy("map") %>%
            hideGroup("BIOREGIONS")
    })
    
    
    
    
    
    
    
   ### --- --- --- --- --- --- --- --- --- --- --- -
   ### Tab 2 ----------------------------
   ### --- --- --- --- --- --- --- --- --- --- --- -
   
   
   # set species density limits reactive to species selection
   density_lim2 <- reactive({
       switch(input$species2,
              "crab" = c(0:250),
              "shrimp" = c(0:25000)
       )
   })
   
   
   # change slider limits to reflect species density limits when species is changed
   observeEvent(input$species2, {
       updateSliderInput(session,'dynamic_slider2', 
                         max=max(density_lim2()),
                         # value = c(max(density_lim())*.25, max(density_lim())*.75)
       )
   })
   
   # make reactive data object to constantly pull columns with the correct
   # species, year, and model number from the main dataset
   map2_dat <- reactiveValues()
   observe({
       
       map2_dat$dat1 <- get(input$species2) %>%
           dplyr::select(x,y, paste0("X",input$year2,".model1")) 
       map2_dat$dat2 <- get(input$species2) %>%
           dplyr::select(x,y, paste0("X",input$year2,".model2")) 
       map2_dat$dat3 <- get(input$species2) %>%
           dplyr::select(x,y, paste0("X",input$year2,".model3")) 
       
       
   })
   
   # rasterize the map
   map2_dat_raster <- reactiveValues()
   observe({
       map2_dat_raster$raster1 <-  rasterFromXYZ(map2_dat$dat1,
                                                 crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
       
       map2_dat_raster$raster2 <-  rasterFromXYZ(map2_dat$dat2,
                                                 crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
       
       map2_dat_raster$raster3 <-  rasterFromXYZ(map2_dat$dat3,
                                                 crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
       
   })
   
   
   
   # create a color palette for the map
   # create a color palette for the map
   map2_pal <- reactiveValues() 
   observe({
       
       # if custom threshold IS NOT selected, use a viridis palette
       if (input$custom2 == "FALSE"){
           map2_pal$pal <- colorNumeric(palette = "plasma", 
                                        density_lim2(),
                                        na.color = "transparent",
                                        reverse=F)
           
       } else{
           # if custom threshold IS selected, build a palette of grey / purple / grey
           # using threshold limits set by threshold slider inputs
           req(input$dynamic_slider2[1],input$dynamic_slider2[2],density_lim2())
           
           # create a dummy variable to prevent the case when the "times" argument is more than the species allows
           # (this would happen when you switch from high-limit species to low-limit species)
           mytimes <- ifelse((max(density_lim2())+1)>input$dynamic_slider2[2] , max(density_lim2())-input$dynamic_slider2[2], input$dynamic_slider2[2])
           map2_pal$pal <-  colorNumeric(palette = c(rep(c("grey70"), times = input$dynamic_slider2[1]),
                                                     rep(c("purple"), times = (input$dynamic_slider2[2]-input$dynamic_slider2[1])),
                                                     rep(c("grey70"), times = mytimes) ), 
                                         domain = density_lim2(),
                                         na.color="transparent",
                                         reverse=F)
           
       }
   })
   
   
   
   #### Map 1 ------
   output$map2_1 <- renderLeaflet(
       leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE,doubleClickZoom= FALSE)) %>%
           addProviderTiles("CartoDB.VoyagerNoLabels") %>%
           setView(lng = -60, lat = 51, zoom = 4) %>%
           leaflet.minicharts::syncWith("tab2maps") %>%
           addControl("<b>Climate Only</b>", position = "topright"),
       
       
   )
   
   # add proxy for showing raster object
   observe({
       # set palette and data for raster object
       pal <-   map2_pal$pal
       dfr <- map2_dat_raster$raster1
       
       leafletProxy("map2_1") %>%
           clearImages() %>% 
           addRasterImage(dfr, colors = pal, opacity = 0.7,
                          project=TRUE, group = "output",
                          layerId = "output") 
   })
   
   #### Map 2 ----
   output$map2_2 <- renderLeaflet(
       leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE,doubleClickZoom= FALSE)) %>%
           addProviderTiles("CartoDB.VoyagerNoLabels") %>%
           setView(lng = -60, lat = 51, zoom = 4) %>%
           leaflet.minicharts::syncWith("tab2maps") %>%
           addControl("<b>Climate + Predator Density</b>", position = "topright"),
       
       
   )
   
   # add proxy for showing raster object
   observe({
       # set palette and data for raster object
       pal <-   map2_pal$pal
       dfr <- map2_dat_raster$raster2
       
       leafletProxy("map2_2") %>%
           clearImages() %>% 
           addRasterImage(dfr, colors = pal, opacity = 0.7,
                          project=TRUE, group = "output",
                          layerId = "output") 
   })
   
   #### Map 3 ----
   output$map2_3 <- renderLeaflet(
       leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE, doubleClickZoom= FALSE)) %>%
           addProviderTiles("CartoDB.VoyagerNoLabels") %>%
           setView(lng = -60, lat = 51, zoom = 4) %>%
           leaflet.minicharts::syncWith("tab2maps") %>%
           addControl("<b>Climate + Predator Index</b>", position = "topright"),
       
       
   )
   
   # add proxy for showing raster object
   observe({
       # set palette and data for raster object
       pal <-   map2_pal$pal
       dfr <- map2_dat_raster$raster3
       
       leafletProxy("map2_3") %>%
           clearImages() %>% 
           addRasterImage(dfr, colors = pal, opacity = 0.7,
                          project=TRUE, group = "output",
                          layerId = "output") 
   })
   
   #### add polygons --------------------
   
   # add polygons for SFAs
   observe({
       req("sfa" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           # fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE), label = ~NOM_ZONE,
                       group = "SFAs" ) %>%
           showGroup("SFAs")
       leafletProxy("map2_2") %>%
           addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           # fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE), label = ~NOM_ZONE,
                       group = "SFAs" ) %>%
           showGroup("SFAs")
       leafletProxy("map2_3") %>%
           addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           # fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE), label = ~NOM_ZONE,
                       group = "SFAs" ) %>%
           showGroup("SFAs")
   })
   # remove SFA polygons
   observe({
       req(!("sfa" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("SFAs")
       leafletProxy("map2_2") %>%
           hideGroup("SFAs")
       leafletProxy("map2_3") %>%
           hideGroup("SFAs")
   })
   
   
   # add polygons for OECMs
   observe({
       req("oecm" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),
                       label = ~NAME_E, group = "OECMs") %>%
           showGroup("OECMs")
       leafletProxy("map2_2") %>%
           addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),
                       label = ~NAME_E, group = "OECMs") %>%
           showGroup("OECMs")
       leafletProxy("map2_3") %>%
           addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),
                       label = ~NAME_E, group = "OECMs") %>%
           showGroup("OECMs")
   })
   # remove OECM polygons
   observe({
       req(!("oecm" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("OECMs")
       leafletProxy("map2_2") %>%
           hideGroup("OECMs")
       leafletProxy("map2_3") %>%
           hideGroup("OECMs")
   })
   
   
   # add polygons for MPAs
   observe({
       req("mpa" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~NAME_E,
                       group = "MPAs") %>%
           showGroup("MPAs") 
       leafletProxy("map2_2") %>%
           addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~NAME_E,
                       group = "MPAs") %>%
           showGroup("MPAs") 
       leafletProxy("map2_3") %>%
           addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~NAME_E,
                       group = "MPAs") %>%
           showGroup("MPAs") 
   })
   # remove MPA polygons
   observe({
       req(!("mpa" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("MPAs")
       leafletProxy("map2_2") %>%
           hideGroup("MPAs")
       leafletProxy("map2_3") %>%
           hideGroup("MPAs")
   })
   
   # add polygons for DFO regions
   observe({
       req("dfo" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~region,
                       group = "DFOs") %>%
           showGroup("DFOs") 
       leafletProxy("map2_2") %>%
           addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~region,
                       group = "DFOs") %>%
           showGroup("DFOs") 
       leafletProxy("map2_3") %>%
           addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~region,
                       group = "DFOs") %>%
           showGroup("DFOs") 
   })
   # remove DFO regions
   observe({
       req(!("dfo" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("DFOs")
       leafletProxy("map2_2") %>%
           hideGroup("DFOs")
       leafletProxy("map2_3") %>%
           hideGroup("DFOs")
   })
   
   
   # add polygons for NAFO regions
   observe({
       req("nafo" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~ZONE,
                       group = "NAFOs") %>%
           showGroup("NAFOs") 
       leafletProxy("map2_2") %>%
           addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~ZONE,
                       group = "NAFOs") %>%
           showGroup("NAFOs") 
       leafletProxy("map2_3") %>%
           addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~ZONE,
                       group = "NAFOs") %>%
           showGroup("NAFOs") 
   })
   # remove NAFO regions
   observe({
       req(!("nafo" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("NAFOs")
       leafletProxy("map2_2") %>%
           hideGroup("NAFOs")
       leafletProxy("map2_3") %>%
           hideGroup("NAFOs")
   })
   
   # add polygons for bioregions
   observe({
       req("bioregions" %in% input$shapefiles2)
       leafletProxy("map2_1") %>%
           addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~Legend,
                       group = "BIOREGIONS") %>%
           showGroup("BIOREGIONS") 
       leafletProxy("map2_2") %>%
           addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~Legend,
                       group = "BIOREGIONS") %>%
           showGroup("BIOREGIONS") 
       leafletProxy("map2_3") %>%
           addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                       highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                           #fillColor="white", fillOpacity = .2,
                                                           bringToFront = TRUE),label = ~Legend,
                       group = "BIOREGIONS") %>%
           showGroup("BIOREGIONS") 
   })
   # remove bioregions
   observe({
       req(!("bioregions" %in% input$shapefiles2))
       leafletProxy("map2_1") %>%
           hideGroup("BIOREGIONS")
       leafletProxy("map2_2") %>%
           hideGroup("BIOREGIONS")
       leafletProxy("map2_3") %>%
           hideGroup("BIOREGIONS")
   })
   
    
    ###  ---/---/---/---/---/---/---/---/---/---/---/-
    #     Tab 3 ----
    ### ---/---/---/---/---/---/---/---/---/---/---/-
    
    
    #### Tab3 density limits -----
    density_lim3 <- reactive({
        switch(input$species3,
               "crab" = c(0:250),
               "shrimp" = c(0:25000)
        )
    })
    
    
    
    #### Tab 3 dataframes -----
    map3_dat <- reactiveValues()
    observe({
        

        map3_dat$dat1 <- get(input$species3) %>%
            dplyr::transmute(x, y, mean_year_group1 = rowMeans(dplyr::select(.,  matches(stringr::str_c(paste0("X",input$year3_1.2,".model",input$model3), collapse="|"))), na.rm = TRUE))
        map3_dat$dat2 <- get(input$species3) %>%
            dplyr::transmute(x, y, mean_year_group2 = rowMeans(dplyr::select(.,  matches(stringr::str_c(paste0("X",input$year3_2.2,".model",input$model3), collapse="|"))), na.rm = TRUE))
        
        
        map3_dat$dat4 <-  get(input$species3) %>%
            dplyr::transmute(x, y,
                             mean_year_group1 = rowMeans(dplyr::select(.,  matches(stringr::str_c(paste0("X",input$year3_1.2,".model",input$model3), collapse="|"))), na.rm = TRUE),
                             mean_year_group2 = rowMeans(dplyr::select(., matches(stringr::str_c(paste0("X",input$year3_2.2,".model",input$model3),  collapse="|"))), na.rm = TRUE)) %>%
            dplyr::transmute(x,y,
                             total = mean_year_group2 - mean_year_group1,
                             percent = (mean_year_group2-mean_year_group1)/mean_year_group1*100) %>%
            mutate(percent = case_when(percent >= 200 ~ 200,
                                       percent <= -100 ~ -100,
                                       TRUE ~ percent)) 
    })
    
    # set species density limits reactive to species selection
    delta_lim3 <- reactive({
        if(input$map3_showme == "total"){
            c(min(map3_dat$dat4[,3],na.rm = T):max(map3_dat$dat4[,3],na.rm = T))
        } else {
            c(min(map3_dat$dat4[,4],na.rm = T):max(map3_dat$dat4[,4],na.rm = T))
        }
    })
    
    
    # rasterize the map
    map3_dat_raster <- reactiveValues()
    observe({
        map3_dat_raster$raster1 <-  rasterFromXYZ(map3_dat$dat1,
                                                  crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
        
        map3_dat_raster$raster2 <-  rasterFromXYZ(map3_dat$dat2,
                                                  crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

        map3_dat_raster$raster4 <-  rasterFromXYZ(map3_dat$dat4 %>% dplyr::select(x,y,input$map3_showme),
                                                  crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
        
        
    })
    
    
    
    # create a color palette for the map
    map3_pal <- reactiveValues() 
    observe({
        
        map3_pal$pal <- colorNumeric(palette = "plasma", 
                                     density_lim3(),
                                     na.color = "transparent",
                                     reverse=F)
        map3_pal$pal_rev <- colorNumeric(palette = "plasma", 
                                         density_lim3(),
                                         na.color = "transparent",
                                         reverse=T)

        # create a red/blue color pal to show increase or decrease
        rampcols <- c(  colorRampPalette(rev(RColorBrewer::brewer.pal(8,"Blues")))((0 - min(delta_lim3()))+1),
                        colorRampPalette(RColorBrewer::brewer.pal(8,"YlOrRd"))(max(delta_lim3())+1))
        map3_pal$pal3 <- colorNumeric(palette = rampcols, domain = delta_lim3(),
                                      na.color="transparent")
        map3_pal$pal3_rev <- colorNumeric(palette = rampcols, domain = delta_lim3(),
                                          na.color="transparent", reverse = T)
        
        
        
        
    })
    
    
    
    #### map 1 ---------------------------
    output$map3_1 <- renderLeaflet(
        leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE,doubleClickZoom= FALSE)) %>%
            addProviderTiles("CartoDB.VoyagerNoLabels") %>%
            setView(lng = -57, lat = 49, zoom = 4) %>%
            leaflet.minicharts::syncWith("tab3maps"), 
        
    )
    # add proxy for showing raster object
    observe({
        # set palette and data for raster object
        pal <-   map3_pal$pal
        pal_rev <- map3_pal$pal_rev
        dfr <- map3_dat_raster$raster1
        
        leafletProxy("map3_1") %>%
            clearImages() %>% 
            clearControls() %>%
            addRasterImage(dfr, colors = pal, opacity = 0.7,
                           project=TRUE, group = "output",
                           layerId = "output") %>%
            addControl("<b>Year Group 1</b>", position = "topright")%>%
            addLegend('bottomright',
                      pal = pal_rev, 
                      values = density_lim3(),
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                      title =  paste0(stringr::str_to_title(input$species3)," Density<br>kg/km2")) 
        
        
    })
    
    #### map 2 ----------------------------------
    output$map3_2 <- renderLeaflet(
        leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE,doubleClickZoom= FALSE)) %>%
            addProviderTiles("CartoDB.VoyagerNoLabels") %>%
            setView(lng = -57, lat = 49, zoom = 4) %>%
            leaflet.minicharts::syncWith("tab3maps"),
        
    )
    
    # # add proxy for showing raster object
    observe({
        # set palette and data for raster object
        pal <-   map3_pal$pal
        pal_rev <- map3_pal$pal_rev
        dfr <- map3_dat_raster$raster2
        
        leafletProxy("map3_2") %>%
            clearImages() %>% 
            clearControls() %>%
            addRasterImage(dfr, colors = pal, opacity = 0.7,
                           project=TRUE, group = "output",
                           layerId = "output") %>%
            addControl("<b>Year Group 2</b>", position = "topright")%>%
            addLegend('bottomright',
                      pal = pal_rev, 
                      values = density_lim3(),
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                      title =  paste0(stringr::str_to_title(input$species3)," Density<br>kg/km2")) 
        
        
    })
    
    #### map 3 ----------------------------
    output$map3_3 <- renderLeaflet(
        leaflet( options = leafletOptions(minZoom = 3, maxZoom = 7, zoomControl = TRUE,doubleClickZoom= FALSE)) %>%
            addProviderTiles("CartoDB.VoyagerNoLabels") %>%
            setView(lng = -57, lat = 49, zoom = 4) %>%
            leaflet.minicharts::syncWith("tab3maps") 
    )
    
    # # add proxy for showing raster object
    observe({
        # set palette and data for raster object
        pal <- map3_pal$pal3
        pal_rev <- map3_pal$pal3_rev
        dfr <- map3_dat_raster$raster4
        
        leafletProxy("map3_3") %>%
            clearImages() %>% 
            clearControls() %>%
            addControl("<b>Delta</b>", position = "topright") %>%
            addRasterImage(dfr, colors = pal, opacity = 0.7,
                           project=TRUE, group = "output",
                           layerId = "output") %>%
            addLegend('bottomright',
                      pal = pal_rev, 
                      values = delta_lim3(),
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                      title =  paste0(stringr::str_to_title(input$species3)," Density<br>Change")) 
        
    })
    
    #### add polygons --------------------
    
    # add polygons for SFAs
    observe({
        req("sfa" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            # fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE), label = ~NOM_ZONE,
                        group = "SFAs" ) %>%
            showGroup("SFAs")
        leafletProxy("map3_2") %>%
            addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            # fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE), label = ~NOM_ZONE,
                        group = "SFAs" ) %>%
            showGroup("SFAs")
        leafletProxy("map3_3") %>%
            addPolygons(data=sfa,weight=3,col = 'red', fillColor="transparent", fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            # fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE), label = ~NOM_ZONE,
                        group = "SFAs" ) %>%
            showGroup("SFAs")
    })
    # remove SFA polygons
    observe({
        req(!("sfa" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("SFAs")
        leafletProxy("map3_2") %>%
            hideGroup("SFAs")
        leafletProxy("map3_3") %>%
            hideGroup("SFAs")
    })
    
    
    # add polygons for OECMs
    observe({
        req("oecm" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),
                        label = ~NAME_E, group = "OECMs") %>%
            showGroup("OECMs")
        leafletProxy("map3_2") %>%
            addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),
                        label = ~NAME_E, group = "OECMs") %>%
            showGroup("OECMs")
        leafletProxy("map3_3") %>%
            addPolygons(data=oecm,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),
                        label = ~NAME_E, group = "OECMs") %>%
            showGroup("OECMs")
    })
    # remove OECM polygons
    observe({
        req(!("oecm" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("OECMs")
        leafletProxy("map3_2") %>%
            hideGroup("OECMs")
        leafletProxy("map3_3") %>%
            hideGroup("OECMs")
    })
    
    
    # add polygons for MPAs
    observe({
        req("mpa" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~NAME_E,
                        group = "MPAs") %>%
            showGroup("MPAs") 
        leafletProxy("map3_2") %>%
            addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~NAME_E,
                        group = "MPAs") %>%
            showGroup("MPAs") 
        leafletProxy("map3_3") %>%
            addPolygons(data=mpa,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~NAME_E,
                        group = "MPAs") %>%
            showGroup("MPAs") 
    })
    # remove MPA polygons
    observe({
        req(!("mpa" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("MPAs")
        leafletProxy("map3_2") %>%
            hideGroup("MPAs")
        leafletProxy("map3_3") %>%
            hideGroup("MPAs")
    })
    
    # add polygons for DFO regions
    observe({
        req("dfo" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~region,
                        group = "DFOs") %>%
            showGroup("DFOs") 
        leafletProxy("map3_2") %>%
            addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~region,
                        group = "DFOs") %>%
            showGroup("DFOs") 
        leafletProxy("map3_3") %>%
            addPolygons(data=dfo,weight=3,col = 'red', fill=F, 
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~region,
                        group = "DFOs") %>%
            showGroup("DFOs") 
    })
    # remove DFO regions
    observe({
        req(!("dfo" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("DFOs")
        leafletProxy("map3_2") %>%
            hideGroup("DFOs")
        leafletProxy("map3_3") %>%
            hideGroup("DFOs")
    })
    
    
    # add polygons for NAFO regions
    observe({
        req("nafo" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~ZONE,
                        group = "NAFOs") %>%
            showGroup("NAFOs") 
        leafletProxy("map3_2") %>%
            addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~ZONE,
                        group = "NAFOs") %>%
            showGroup("NAFOs") 
        leafletProxy("map3_3") %>%
            addPolygons(data=nafo,weight=3,col = 'red', fill=F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~ZONE,
                        group = "NAFOs") %>%
            showGroup("NAFOs") 
    })
    # remove NAFO regions
    observe({
        req(!("nafo" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("NAFOs")
        leafletProxy("map3_2") %>%
            hideGroup("NAFOs")
        leafletProxy("map3_3") %>%
            hideGroup("NAFOs")
    })
    
    # add polygons for bioregions
    observe({
        req("bioregions" %in% input$shapefiles3)
        leafletProxy("map3_1") %>%
            addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~Legend,
                        group = "BIOREGIONS") %>%
            showGroup("BIOREGIONS") 
        leafletProxy("map3_2") %>%
            addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~Legend,
                        group = "BIOREGIONS") %>%
            showGroup("BIOREGIONS") 
        leafletProxy("map3_3") %>%
            addPolygons(data=bioregions,weight=3,col = 'red', fill =F,
                        highlightOptions = highlightOptions(color = "white", weight = 4, #fill=T,
                                                            #fillColor="white", fillOpacity = .2,
                                                            bringToFront = TRUE),label = ~Legend,
                        group = "BIOREGIONS") %>%
            showGroup("BIOREGIONS") 
    })
    # remove bioregions
    observe({
        req(!("bioregions" %in% input$shapefiles3))
        leafletProxy("map3_1") %>%
            hideGroup("BIOREGIONS")
        leafletProxy("map3_2") %>%
            hideGroup("BIOREGIONS")
        leafletProxy("map3_3") %>%
            hideGroup("BIOREGIONS")
    })
    
    
    
})

#runApp("shiny", display.mode = "showcase")



