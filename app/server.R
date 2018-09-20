shinyServer(function(input, output, session){
  observe({
    print(paste0("User: ", session$user))
    print(paste0("Start time: ", Sys.time()))
  })
  session$onSessionEnded(function() {
    print(paste0("End time: ", Sys.time()))
  })
  
  rv <- reactiveValues(x = NULL, y = NULL)
  # RESET BUTTON ----
  observeEvent(input$resetInputs, {
    updateSelectizeInput(session, 'x_var', selected = 'ORIGIN_PLACE_TYPE')
    updateSelectizeInput(session, 'y_var', selected = 'None')
    updateSelectizeInput(session, 'facet_by', selected = 'None')
    updateCheckboxInput(session, 'freq', value = FALSE)
    updateCheckboxGroupInput(session, 'route_type', inline = TRUE, choices = init[, levels(RouteType)])
    updateCheckboxGroupInput(session, 'transfers_from', inline = TRUE, choices = init[, levels(TRANSFERS_FROM)])
    updateCheckboxGroupInput(session, 'orig_type', inline = TRUE, choices = init[, levels(ORIGIN_PLACE_TYPE)])
    updateCheckboxGroupInput(session, 'access_mode', inline = TRUE, choices = init[, levels(ACCESS_MODE)])
    updateCheckboxGroupInput(session, 'transfers_to', inline = TRUE, choices = init[, levels(TRANSFERS_TO)])
    updateCheckboxGroupInput(session, 'dest_type', inline = TRUE, choices = init[, levels(DESTIN_PLACE_TYPE)])
    updateCheckboxGroupInput(session, 'license', inline = TRUE, choices = init[, levels(HAS_DRIVE_LICENSE)])
    updateCheckboxGroupInput(session, 'age_group', inline = TRUE, choices = init[, levels(AGE)])
    updateCheckboxGroupInput(session, 'race', inline = TRUE, choices = init[, levels(race_ethnicity)])
    updateCheckboxGroupInput(session, 'disab', inline = TRUE, choices = init[, levels(DISABILITY)])
    updateCheckboxGroupInput(session, 'gender', inline = TRUE, choices = init[, levels(GENDER)])
    updateCheckboxGroupInput(session, 'egress_mode', inline = TRUE, choices = init[, levels(EGRESS_MODE)])
    updateCheckboxGroupInput(session, 'time_period', inline = TRUE, choices = init[, levels(TIME_PERIOD)])
    updateCheckboxGroupInput(session, 'trip_opp_dir', inline = TRUE, choices = init[, levels(TRIP_IN_OPPOSITE_DIR)])
    updateCheckboxGroupInput(session, 'payment', inline = TRUE, choices = init[, levels(PAYMENT_METHOD)])
    updateCheckboxGroupInput(session, 'fare_type', inline = TRUE, choices = init[, levels(FARE_TYPE)])
    updateCheckboxGroupInput(session, 'trip_purpose', inline = TRUE, choices = init[, levels(trip_purpose)])
    updateCheckboxGroupInput(session, 'is_visitor', inline = TRUE, choices = init[, levels(VISITOR)])
    updateCheckboxGroupInput(session, 'hh_veh', inline = TRUE, choices = init[, levels(COUNT_VH_HH)])
    updateCheckboxGroupInput(session, 'use_veh_trip', inline = TRUE, choices = init[, levels(CAN_USE_VEH_TRIP)])
    updateCheckboxGroupInput(session, 'hh_members', inline = TRUE, choices = init[, levels(COUNT_MEMBER_HH)])
    updateCheckboxGroupInput(session, 'hh_employed', inline = TRUE, choices = init[, levels(COUNT_EMPLOYED_HH)])
    updateCheckboxGroupInput(session, 'employ_status', inline = TRUE, choices = init[, levels(STATUS_EMPLOYMENT)])
    updateCheckboxGroupInput(session, 'stud_status', inline = TRUE, choices = init[, levels(STUDENT_STATUS)])
    updateCheckboxGroupInput(session, 'fare_subsidy', inline = TRUE, choices = init[, levels(FARE_SUBSIDY)])
    updateCheckboxGroupInput(session, 'income', inline = TRUE, choices = init[, levels(INCOME)])
    updateCheckboxGroupInput(session, 'home_lang', inline = TRUE, choices = init[, levels(HOME_LANG_OTHER)])
    updateCheckboxGroupInput(session, 'eng_ability', inline = TRUE, choices = init[, levels(ENGLISH_ABILITY)])
    updateSelectInput(session, 'route_p', choices = c("All", sort(as.numeric(unique(tbi$route)))))
  })
  
  observeEvent(input$clear_route, {
    updateSelectInput(session, 'route_p', choices = c("All", sort(as.numeric(unique(tbi$route)))))
  })
  
  observe({
    if (input$x_var == 'None' | input$y_var == 'None') {
      updateSelectizeInput(session, 'facet_by', selected = 'None')
      updateCheckboxInput(session, 'freq', value = FALSE)
    }
    rv$x <- input$x_var
    rv$y <- input$y_var
  })
  
  # INTERACTIVE PLOTS ---------------------
  observeEvent(input$swap, {
    updateSelectInput(session, 'x_var', selected = rv$y)
    updateSelectInput(session, 'y_var', selected = rv$x)
  })
  
  dataInput <- reactive({
    facets <- c(input$x_var, input$y_var, input$facet_by)
    facetVar <- facets[sapply(facets, function(x) x != 'None')]
    # cat('facetVar: ', facetVar, '\n')
    
    # all filtering variables
    filterVal <- list(RouteType = input$route_type, TRANSFERS_FROM = input$transfers_from, ORIGIN_PLACE_TYPE = input$orig_type,
                      ACCESS_MODE = input$access_mode, TRANSFERS_TO = input$transfers_to, DESTIN_PLACE_TYPE = input$dest_type,
                      HAS_DRIVE_LICENSE = input$license, AGE = input$age_group, race_ethnicity = input$race, DISABILITY = input$disab, 
                      GENDER = input$gender, EGRESS_MODE = input$egress_mode, TIME_PERIOD = input$time_period, TRIP_IN_OPPOSITE_DIR = input$trip_opp_dir,
                      PAYMENT_METHOD = input$payment, FARE_TYPE = input$fare_type, trip_purpose = input$trip_purpose, VISITOR = input$is_visitor, COUNT_VH_HH = input$hh_veh, 
                      CAN_USE_VEH_TRIP = input$use_veh_trip, COUNT_MEMBER_HH = input$hh_members, COUNT_EMPLOYED_HH = input$hh_employed,
                      STATUS_EMPLOYMENT = input$employ_status, STUDENT_STATUS = input$stud_status, FARE_SUBSIDY = input$fare_subsidy,
                      INCOME = input$income, HOME_LANG_OTHER = input$home_lang, ENGLISH_ABILITY = input$eng_ability, 
                      route = input$route_p)
    
    for (ix in seq_along(facetVar)) {
      varLevels <- if (is.factor(tbi[, eval(as.name(facetVar[ix]))])) {
        thisLevels <- tbi[, levels(eval(as.name(facetVar[ix])))]
        factor(thisLevels, levels = thisLevels, ordered = T)
      } else {
        tbi[, unique(eval(as.name(facetVar[ix])))]
      }
      filterVal[[facetVar[ix]]] <- varLevels
    }
    activeFilters <- names(filterVal)[!sapply(filterVal, is.null)]
    
    matched <- if (length(activeFilters) >= 1) {
      setkeyv(tbi, activeFilters)
      tbi[do.call(CJ, filterVal[!sapply(filterVal, is.null)]), nomatch = 0L, allow.cartesian = TRUE]
    } else {
      tbi
    }
    if (input$linked) {
      return(matched[, .(obs = .N, N = sum(unlinked_wgts * LINKED_MULTIPLIER)), by = eval(activeFilters[activeFilters %in% facets])])
    } else {
      return(matched[, .(obs = .N, N = sum(unlinked_wgts)), by = eval(activeFilters[activeFilters %in% facets])])
    }
  })
  
  # small sample warning
  output$warningSmallSample <- renderUI({
    # generate warning message when there are groups with less than 30 observations
    if (input$x_var == "None" | input$y_var == "None" | input$x_var == input$y_var) {
      small_sample <- dataInput()[obs < 30]
    } else if (input$facet_by == "None") {
      small_sample <- dataInput()[, .(obs = sum(obs)), by = eval(input$x_var)][obs < 30]
    } else {
      small_sample <- dataInput()[, .(obs = sum(obs)), by = c(eval(input$x_var), eval(input$facet_by))][obs < 30]
    }
    if (nrow(small_sample) > 0) {
      small_sample[, nextrow := "<br/>"]
      textt <- apply(small_sample, 1, paste, collapse=" ")
      outText <- HTML(paste0(c("CAUTION: Group(s) below have small sample size: <br/> ", textt)))
    } else {
      outText <- ""
    }
    outText
  })
  
  output$statPlot <- renderPlotly({
    p <- plot_stats(newdata = dataInput(), x = input$x_var, y = input$y_var, f_v = input$facet_by, freq = input$freq)
    # t <- paste0(ifelse(input$x_var != 'None', input$x_var, ""), ifelse(input$y_var != 'None', paste0(" by ", input$y_var), ""),
    #             ifelse(input$facet_by != 'None', paste0(" faceted by ", input$facet_by), ""),  
    #             c(" filtered by ", "")[all(sapply(list(net_promoters = input$netPromoter, mode = input$mode, race = input$race, 
    #                                                    age_group = input$age, tod = input$tod, hh_income = input$hhIncome, 
    #                                                    payment = input$payment, dow = input$dow, 
    #                                                    harassed = input$harassed, fear = input$fear, 
    #                                                    gender = input$gender), is.null)) + 1], 
    #             ifelse(is.null(input$netPromoter), "", "net promoters, "), 
    #             ifelse(is.null(input$mode), "", "mode, "), 
    #             ifelse(is.null(input$race), "", "race, "), 
    #             ifelse(is.null(input$age), "", "age group, "), 
    #             ifelse(is.null(input$tod), "", "time of day, "), 
    #             ifelse(is.null(input$hhIncome), "", "household income, "), 
    #             ifelse(is.null(input$payment), "", "payment type, "), 
    #             ifelse(is.null(input$dow), "", "day of week, "), 
    #             ifelse(is.null(input$harassed), "", "experienced harassment, "), 
    #             ifelse(is.null(input$fear), "", "fear for safety, "), 
    #             ifelse(is.null(input$gender), "", "gender"))
    t <- paste0(ifelse(input$x_var != 'None', input$x_var, ""), ifelse(input$y_var != 'None', paste0(" by ", input$y_var), ""))
    m <- list(l = 50, r = 50, b = 250, t = 50, pad = 4)
    if (input$x_var == 'None' | input$y_var == 'None' | input$freq == FALSE | input$x_var == input$y_var) {
      ggplotly(p, tooltip = 'text') %>% layout(height = 800, margin = m)
    } else {
      ggplotly(p, tooltip = 'text') %>%
        layout(yaxis = list(autorange = "reversed"), height = 800, margin = m)
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      facets <- c(input$y_var, input$x_var, input$facet_by)
      facetText <- paste0('by_', facets[facets != 'None'], collapse = "_")
      # filterText <- c("_Filtered", "")[all(sapply(list(net_promoters = input$netPromoter, mode = input$mode, race = input$race, 
      #                                                  age_group = input$age, tod = input$tod, hh_income = input$hhIncome, 
      #                                                  payment = input$payment, dow = input$dow, 
      #                                                  harassed = input$harassed, fear = input$fear, 
      #                                                  gender = input$gender), is.null)) + 1]
      filterText <- ""
      paste0('2016CustomerSurvey_', facetText, filterText, '.png')
    },
    content = function(file) {
      # t <- paste0(ifelse(input$x_var != 'None', input$x_var, ""), ifelse(input$y_var != 'None', paste0(" by ", input$y_var), ""),
      #             ifelse(input$facet_by != 'None', paste0(" faceted by ", input$facet_by), ""),  
      #             c(" filtered by ", "")[all(sapply(list(net_promoters = input$netPromoter, mode = input$mode, race = input$race, 
      #                                                    age_group = input$age, tod = input$tod, hh_income = input$hhIncome, 
      #                                                    payment = input$payment, dow = input$dow, 
      #                                                    harassed = input$harassed, fear = input$fear, 
      #                                                    gender = input$gender), is.null)) + 1], 
      #             ifelse(is.null(input$netPromoter), "", "net promoters, "), 
      #             ifelse(is.null(input$mode), "", "mode, "), 
      #             ifelse(is.null(input$race), "", "race, "), 
      #             ifelse(is.null(input$age), "", "age group, "), 
      #             ifelse(is.null(input$tod), "", "time of day, "), 
      #             ifelse(is.null(input$hhIncome), "", "household income, "), 
      #             ifelse(is.null(input$payment), "", "payment type, "), 
      #             ifelse(is.null(input$dow), "", "day of week, "), 
      #             ifelse(is.null(input$harassed), "", "experienced harassment, "), 
      #             ifelse(is.null(input$fear), "", "fear for safety, "), 
      #             ifelse(is.null(input$gender), "", "gender"))
      t <- paste0(ifelse(input$x_var != 'None', input$x_var, ""), ifelse(input$y_var != 'None', paste0(" by ", input$y_var), ""))
      ggsave(file, plot = plot_stats(dataInput(), input$x_var, input$y_var, input$facet_by,freq = input$freq) + 
               theme(axis.text.x = element_text(angle = 45, hjust = 1)), width = 20, height = 6)
    }
  )
  
  # MAP by Route -------------
  observe({
    updateSelectInput(session, 'od_time', choices = tbi[!is.na(TIME_PERIOD) & route %in% input$route, sort(unique(TIME_PERIOD))])
  })
  observe({
    updateSelectInput(session, 'od_age', choices = tbi[!is.na(AGE) & route %in% input$route & TIME_PERIOD %in% input$od_time, sort(unique(AGE))])
  })
  observe({
    updateSelectInput(session, 'od_race', choices = tbi[!is.na(race_ethnicity) & route %in% input$route & TIME_PERIOD %in% input$od_time & AGE %in% input$od_age, sort(unique(race_ethnicity))])
  })
  observe({
    updateSelectInput(session, 'od_income', choices = tbi[!is.na(INCOME) & route %in% input$route & TIME_PERIOD %in% input$od_time & AGE %in% input$od_age & race_ethnicity %in% input$od_race, sort(unique(INCOME))])
  })
  mapInput <- reactive({
    if (input$measureOD == "Origin - Destination") {
      lines <- copy(od_lines)
    } else {
      lines <- copy(ba_lines)
    }
    filterVal <- list(route = input$route, TIME_PERIOD = input$od_time, 
                      AGE = input$od_age, race_ethnicity = input$od_race, INCOME = input$od_income)
    
    activeFilters <- names(filterVal)[!sapply(filterVal, is.null)]
    
    if (length(activeFilters) >= 1) {
      setkeyv(tbi, activeFilters)
      tbi_temp <- tbi[do.call(CJ, filterVal[!sapply(filterVal, is.null)]), nomatch = 0L, allow.cartesian = TRUE]
      ids <- tbi_temp$ID
      tbi_temp[, `:=` (lat = DESTINATION_LAT, lon = DESTINATION_LON)]
      temp <- do.call('rbind', lines[ids])
      return(list(temp, tbi_temp))
    } else {
      lines
    }
  })
  for_map <- reactive({
    # if ("All" %in% input$route) {
    #   routes
    # } else {
    subset(routes, route %in% input$route)
    # }
  })
  output$routesSelected <- renderText({
    paste("Route(s) selected:", paste0(input$route, collapse = ", "))
  })
  output$myMap <- renderLeaflet({
    id <- tbi[route == '921', ID]
    temp <- do.call("rbind", od_lines[id])
    endPoints <- tbi[ID %in% id]
    leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      addPolylines(data = subset(routes, route %in% '921'), color = "#008144", weight = 4, opacity = 1) %>%
      addPolylines(data = temp, weight = 1, opacity = .3, color = "#0053A0") %>%
      addCircleMarkers(data = endPoints, lng = ~lon, lat = ~lat, radius = 2, opacity = .7, weight = 1, color = "#0053A0", 
                       popup = ~paste0("Route taken: ", route, " ", ifelse(is.na(TRANSFERS_FROM_FIRST_CODE), "", TRANSFERS_FROM_FIRST_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_SECOND_CODE), "", TRANSFERS_FROM_SECOND_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_THIRD_CODE), "", TRANSFERS_FROM_THIRD_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_FOURTH_CODE), "", TRANSFERS_FROM_FOURTH_CODE), " ",
                                       ifelse(is.na(TRANSFERS_TO_FIRST_CODE), "", TRANSFERS_TO_FIRST_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_SECOND_CODE), "", TRANSFERS_TO_SECOND_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_THIRD_CODE), "", TRANSFERS_TO_THIRD_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_FOURTH_CODE), "", TRANSFERS_TO_FOURTH_CODE),
                                       "<br>Time period: ", TIME_PERIOD), layerId = ~paste0("dest", ID)) %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11)
  })
  
  observeEvent(c(input$plotButton), {
    leafletProxy('myMap', session) %>% clearControls() %>% clearShapes() %>% clearMarkers() %>%
      addPolylines(data = for_map(), color = "#008144", weight = 4, opacity = 1) %>%
      addPolylines(data = mapInput()[[1]], weight = 1, opacity = .3, color = "#0053A0") %>%
      addCircleMarkers(data = mapInput()[[2]], lng = ~lon, lat = ~lat, radius = 2, opacity = .7, weight = 1, color = "#0053A0", 
                       popup = ~paste0("Route taken: ", route, " ", ifelse(is.na(TRANSFERS_FROM_FIRST_CODE), "", TRANSFERS_FROM_FIRST_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_SECOND_CODE), "", TRANSFERS_FROM_SECOND_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_THIRD_CODE), "", TRANSFERS_FROM_THIRD_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_FROM_FOURTH_CODE), "", TRANSFERS_FROM_FOURTH_CODE), " ",
                                       ifelse(is.na(TRANSFERS_TO_FIRST_CODE), "", TRANSFERS_TO_FIRST_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_SECOND_CODE), "", TRANSFERS_TO_SECOND_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_THIRD_CODE), "", TRANSFERS_TO_THIRD_CODE), " ", 
                                       ifelse(is.na(TRANSFERS_TO_FOURTH_CODE), "", TRANSFERS_TO_FOURTH_CODE),
                                       "<br>Time period: ", TIME_PERIOD), layerId = ~paste0("dest", ID))
  })
  
  output$dlodr <- downloadHandler(
    filename = "ODMapByRoute.html", 
    content = function(file) {
      src <- normalizePath('mymap_route.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_route.Rmd')
      
      out <- rmarkdown::render('mymap_route.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  
  output$pdf_odr <- downloadHandler(
    filename = "ODMapByRoute.pdf", 
    content = function(file) {
      src <- normalizePath('mymap_route.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_route.Rmd')
      
      out <- webshot(rmarkdown::render('mymap_route.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
  
  # HEAT MAP -------------------------------------------------------------------------------------------------------
  output$heatmap <- renderLeaflet({
    t <- tbi[route %in% '921',
             `:=` (lat = as.numeric(HOME_OR_HOTEL_ADDR_LAT), 
                   lon = as.numeric(HOME_OR_HOTEL_ADDR_LON))][!is.na(lat) & !is.na(lon)]
    leaflet() %>%
      addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
               under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
               Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
               under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      addPolylines(data = subset(routes, route %in% '921'), color = "#008144", weight = 4, opacity = 1) %>%
      addHeatmap(data = t, lng = ~lon, lat = ~lat, blur = 9, radius = 6) %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11)
  })
  
  tbi_hm <- reactive({
    if (any("lat" == names(tbi))) {
      tbi[, `:=` (lat = NULL, lon = NULL)]
    }
    if (is.null(input$route_hm) | input$route_hm == "All" | input$route_hm == '') {
      if (input$measure == "Home/Hotel") {
        tbi[, `:=` (lat = as.numeric(HOME_OR_HOTEL_ADDR_LAT), lon = as.numeric(HOME_OR_HOTEL_ADDR_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Destination") {
        tbi[, `:=` (lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Origin") {
        tbi[, `:=` (lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Boarding Location") {
        tbi[, `:=` (lat = as.numeric(BOARDING_LAT), lon = as.numeric(BOARDING_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Alighting Location") {
        tbi[, `:=` (lat = as.numeric(ALIGHTING_LAT), lon = as.numeric(ALIGHTING_LON))][!is.na(lat) & !is.na(lon)]
      }
    } else {
      if (input$measure == "Home/Hotel") {
        tbi[route %in% input$route_hm, `:=` (lat = as.numeric(HOME_OR_HOTEL_ADDR_LAT), 
                                             lon = as.numeric(HOME_OR_HOTEL_ADDR_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Destination") {
        tbi[route %in% input$route_hm, `:=` (lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Origin") {
        tbi[route %in% input$route_hm, `:=` (lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Boarding Location") {
        tbi[route %in% input$route_hm, `:=` (lat = as.numeric(BOARDING_LAT), lon = as.numeric(BOARDING_LON))][!is.na(lat) & !is.na(lon)]
      } else if (input$measure == "Alighting Location") {
        tbi[route %in% input$route_hm, `:=` (lat = as.numeric(ALIGHTING_LAT), lon = as.numeric(ALIGHTING_LON))][!is.na(lat) & !is.na(lon)]
      }
    }
  })
  
  for_heatmap <- reactive({
    if ("All" %in% input$route_hm) {
      routes
    } else {
      subset(routes, route %in% input$route_hm)
    }
  })
  
  output$routesSelected_hm <- renderText({
    paste("Route(s) selected:", paste0(input$route_hm, collapse = ", "))
  })
  observeEvent(c(input$route_hm, input$measure), {
    leafletProxy('heatmap', session) %>% clearHeatmap() %>% clearShapes() %>% 
      addPolylines(data = for_heatmap(), color = "#008144", weight = 4, opacity = 1) %>%
      addHeatmap(data = tbi_hm(), lng = ~lon, lat = ~lat, blur = 9, radius = 6)
  })
  
  output$dlhm <- downloadHandler(
    filename = "Heatmap.html", 
    content = function(file) {
      src <- normalizePath('heatmap.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'heatmap.Rmd')
      
      out <- rmarkdown::render('heatmap.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_hm <- downloadHandler(
    filename = "Heatmap.pdf", 
    content = function(file) {
      src <- normalizePath('heatmap.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'heatmap.Rmd')
      
      out <- webshot(rmarkdown::render('heatmap.Rmd',
                                       output_format = "html_document"
      ), file = file)
      out
    }
  )
  # O-D map by block -----
  rv <- reactiveValues(curr_tracts = NULL, curr_blocks = NULL, curr_taz = NULL)
  
  output$mapGeo <- renderLeaflet({
    leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11) 
  })
  
  # get all o-d pairs corresponding to selected area(s)
  observeEvent(input$mapGeo_click, {
    click <- input$mapGeo_click
    if (is.null(click)) return()
    # turn lat/lon into a spatial point
    latLon <- data.frame(lon = click$lng, lat = click$lat)
    spPoint <- SpatialPointsDataFrame(latLon, data = data.frame(Type = "Address"))
    proj4string(spPoint) <- proj4string(blocks)
    mergePoint <- data.table(cbind(spPoint@data, over(spPoint, blocks)))
    if (nrow(na.omit(mergePoint)) == 0) return() # do nothing if no intersection with blocks 
    rv$curr_blocks <- c(rv$curr_blocks, mergePoint$GEOID10) # add selected block to the array 
    get_od(fm = blocks_fm, cluster = input$block_cluster, od = input$od_block, var = "block", 
           r = input$rf_bl, rt = input$rtf_bl, tp = input$tpf_bl, linked_odb = input$linked_odb, 
           ignoreN = input$ignoreN_block, heat = input$heat_block, curr_poly = rv$curr_blocks, mapId = 'mapGeo', 
           sp = blocks, tbi = tbi, session = session) # update 'mapGeo'
  })
  
  observeEvent(c(input$block_cluster, input$ignoreN_block, input$rf_bl, input$rtf_bl, input$tpf_bl, 
                 input$linked_odb, input$heat_block, input$od_block, input$exc_sel_block), {
    leafletProxy('mapGeo', session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    current_blocks <- rv$curr_blocks
    if (is.null(current_blocks)) return()
    get_od(fm = blocks_fm, cluster = input$block_cluster, od = input$od_block, var = "block", 
           r = input$rf_bl, rt = input$rtf_bl, tp = input$tpf_bl, linked_odb = input$linked_odb, 
           ignoreN = input$ignoreN_block, heat = input$heat_block, curr_poly = current_blocks, mapId = 'mapGeo', 
           sp = blocks, tbi = tbi, session = session) # update 'mapGeo'
  })
  
  observeEvent(input$resetGeo, {
    leafletProxy("mapGeo", session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    rv$curr_blocks <- NULL
  })
  
  output$dlodb <- downloadHandler(
    filename = "ODMapByBlock.html", 
    content = function(file) {
      src <- normalizePath('mymap_block.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_block.Rmd')
      
      out <- rmarkdown::render('mymap_block.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_odb <- downloadHandler(
    filename = "ODMapByBlock.pdf", 
    content = function(file) {
      src <- normalizePath('mymap_block.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_block.Rmd')
      
      out <- webshot(rmarkdown::render('mymap_block.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
  
  # o-d map by tracts ----
  output$mapTract <- renderLeaflet({
    leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      # addCircleMarkers(data = pnr, popup = ~paste("Name: ", Name), color = "#FFD200", opacity = .8, radius = 3, fillOpacity = .8) %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11) #%>%
      # addLegend(colors = c("#FFD200"), 
      #           labels = c("Park and Ride Lots"), position = 'bottomleft')
  })
  
  # get all o-d pairs corresponding to selected area(s)
  observeEvent(input$mapTract_click, {
    click <- input$mapTract_click
    if (is.null(click)) return()
    latLon <- data.frame(lon = click$lng, lat = click$lat)
    spPoint <- SpatialPointsDataFrame(latLon, data = data.frame(Type = "Address"))
    proj4string(spPoint) <- proj4string(tracts)
    mergePoint <- data.table(cbind(spPoint@data, over(spPoint, tracts)))
    rv$curr_tracts <- c(rv$curr_tracts, mergePoint$GEOID10)
    if (nrow(na.omit(mergePoint)) == 0) return()
    # if (input$stl_dat == "TBI Data Only") {
      get_od(fm = tracts_fm, cluster = input$tract_cluster, od = input$od_tract, var = "tract", 
             r = input$rf_tr, rt = input$rtf_tr, tp = input$tpf_tr, linked_odb = input$linked_odtr, 
             ignoreN = input$ignoreN, heat = input$heat_tract, curr_poly = rv$curr_tracts, mapId = 'mapTract', 
             sp = tracts, tbi = tbi, session = session, exc_sel = input$exc_sel_tract)
    # } else if (input$stl_dat == "Streetlight Data Only") {
    #   get_od_sl(tp = input$tpf_tr_sl, sl = sl_tract, dt = input$dtf_tr_sl, od = input$od_tract, 
    #             heat = input$heat_tr_sl, curr_poly = rv$curr_tracts, exc_sel = input$exc_sel_tr_sl, 
    #             sp = tracts, mapId = 'mapTract', session = session)
    # }
  })
  
  observeEvent(c(input$tract_cluster, input$ignoreN, input$rf_tr, input$rtf_tr, input$tpf_tr, input$linked_odtr, input$heat_tract,
                 input$stl_dat, input$od_tract, input$exc_sel_tract), {
    current_tracts <- rv$curr_tracts
    if (is.null(current_tracts)) return()
    leafletProxy('mapTract', session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    # if (input$stl_dat == "TBI Data Only") {
      get_od(fm = tracts_fm, cluster = input$tract_cluster, od = input$od_tract, var = "tract", 
             r = input$rf_tr, rt = input$rtf_tr, tp = input$tpf_tr, linked_odb = input$linked_odtr, 
             ignoreN = input$ignoreN, heat = input$heat_tract, curr_poly = rv$curr_tracts, mapId = 'mapTract', 
             sp = tracts, tbi = tbi, session = session, exc_sel = input$exc_sel_tract)
    # } else if (input$stl_dat == "Streetlight Data Only") {
    #   req(input$tpf_tr_sl, input$dtf_tr_sl)
    #   get_od_sl(tp = input$tpf_tr_sl, sl = sl_tract, dt = input$dtf_tr_sl, od = input$od_tract, 
    #             heat = input$heat_tr_sl, curr_poly = rv$curr_tracts, exc_sel = input$exc_sel_tr_sl, 
    #             sp = tracts, mapId = 'mapTract', session = session)
    # }
  })
  
  observeEvent(input$resetTract, {
    leafletProxy("mapTract", session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    rv$curr_tracts <- NULL
  })
  
  output$dlodtr <- downloadHandler(
    filename = "ODMapByTract.html", 
    content = function(file) {
      src <- normalizePath('mymap_tract.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_tract.Rmd')
      
      out <- rmarkdown::render('mymap_tract.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_odtr <- downloadHandler(
    filename = "ODMapByTract.pdf", 
    content = function(file) {
      src <- normalizePath('mymap_tract.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_tract.Rmd')
      
      out <- webshot(rmarkdown::render('mymap_tract.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
  
  # o-d map by taz ----
  output$mapTaz <- renderLeaflet({
    leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11)
  })
  
  # get all o-d pairs corresponding to selected area(s)
  observeEvent(input$mapTaz_click, {
    click <- input$mapTaz_click
    if (is.null(click)) return()
    latLon <- data.frame(lon = click$lng, lat = click$lat)
    spPoint <- SpatialPointsDataFrame(latLon, data = data.frame(Type = "Address"))
    proj4string(spPoint) <- proj4string(taz)
    mergePoint <- data.table(cbind(spPoint@data, over(spPoint, taz)))
    rv$curr_taz <- c(rv$curr_taz, mergePoint$GEOID10)
    if (nrow(na.omit(mergePoint)) == 0) return()
    # if (input$stl_dat_taz == "TBI Data Only") {
      get_od(fm = taz_fm, cluster = input$taz_cluster, od = input$od_taz, var = "taz", 
             r = input$rf_taz, rt = input$rtf_taz, tp = input$tpf_taz, linked_odb = input$linked_odtaz, 
             ignoreN = input$ignoreN_taz, heat = input$heat_taz, curr_poly = rv$curr_taz, mapId = 'mapTaz', 
             sp = taz, tbi = tbi, session = session, exc_sel = input$exc_sel_taz)
    # } else if (input$stl_dat_taz == "Streetlight Data Only") {
    #   get_od_sl(tp = input$tpf_taz_sl, sl = sl_taz, dt = input$dtf_taz_sl, od = input$od_taz, 
    #             heat = input$heat_taz_sl, curr_poly = rv$curr_taz, exc_sel = input$exc_sel_taz_sl, 
    #             sp = taz, mapId = 'mapTaz', session = session)
    # }
  })
  
  observeEvent(c(input$taz_cluster, input$ignoreN_taz, input$rf_taz, input$rtf_taz, input$tpf_taz, input$linked_odtaz, input$heat_taz, 
                 input$stl_dat_taz, input$od_taz, input$exc_sel_taz), {
    current_taz <- rv$curr_taz
    if (is.null(current_taz)) return()
    leafletProxy('mapTaz', session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    # if (input$stl_dat_taz == "TBI Data Only") {
      get_od(fm = taz_fm, cluster = input$taz_cluster, od = input$od_taz, var = "taz", 
             r = input$rf_taz, rt = input$rtf_taz, tp = input$tpf_taz, linked_odb = input$linked_odtaz, 
             ignoreN = input$ignoreN_taz, heat = input$heat_taz, curr_poly = current_taz, mapId = 'mapTaz', 
             sp = taz, tbi = tbi, session = session, exc_sel = input$exc_sel_taz)
    # } else if (input$stl_dat_taz == "Streetlight Data Only") {
    #   get_od_sl(tp = input$tpf_taz_sl, sl = sl_taz, dt = input$dtf_taz_sl, od = input$od_taz, 
    #             heat = input$heat_taz_sl, curr_poly = current_taz, exc_sel = input$exc_sel_taz_sl, 
    #             sp = taz, mapId = 'mapTaz', session = session)
    # }
  })
  
  observeEvent(input$resetTaz, {
    leafletProxy("mapTaz", session) %>% clearShapes() %>% removeMarker(layerId = paste0("dest", tbi$ID))
    rv$curr_taz <- NULL
  })
  
  output$dlodtaz <- downloadHandler(
    filename = "ODMapByTAZ.html", 
    content = function(file) {
      src <- normalizePath('mymap_taz.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_taz.Rmd')
      
      out <- rmarkdown::render('mymap_taz.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_odtaz <- downloadHandler(
    filename = "ODMapByTAZ.pdf", 
    content = function(file) {
      src <- normalizePath('mymap_taz.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_taz.Rmd')
      
      out <- webshot(rmarkdown::render('mymap_taz.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
  # o-d map by custom bounding box ----
  output$mapBbox <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
               under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
               Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
               under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
      setView(lat = 44.963, lng = -93.22, zoom = 11)
  })
  
  track_clicks <- reactiveValues(bbox_clicks = c(), curr_box = list())
  
  observeEvent(input$mapBbox_click, {
    click <- input$mapBbox_click
    if (is.null(click)) 
      return()
    track_clicks$bbox_clicks <- c(track_clicks$bbox_clicks, click$lng, click$lat)
    leafletProxy('mapBbox', session) %>% addCircleMarkers(lng = click$lng, lat = click$lat, radius = 2, opacity = 1)
  })
  
  # get all o-d pairs corresponding to custom drawn box(es)
  observeEvent(input$mapItBbox, {
    # convert coordinations into a minimum polygon
    bbox_coords <- track_clicks$bbox_clicks
    if (length(bbox_coords) == 0) return()
    bbox_coords <- c(bbox_coords, bbox_coords[1:2])
    # print(bbox_coords)
    bbox_mat <- matrix(bbox_coords, ncol = 2, byrow = TRUE)
    track_clicks$bbox_clicks <- c()
    p1 <- sp::Polygon(bbox_mat)
    sp_p <- SpatialPolygons(list(Polygons(list(p1), ID = "a")), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    sp_p <- SpatialPolygonsDataFrame(sp_p, data = data.frame(Type = "Custom Inputs"), match.ID = F)
    # get the center of the polygon
    cntrd <- centroid(sp_p)
    temp <- copy(tbi)
    setnames(temp, "TIME_PERIOD", "TimePeriod")
    temp <- filter_data(temp = temp, r = input$rf_bbox, rt = input$rtf_bbox, tp = input$tpf_bbox)
    if (input$dest_group == "None") {
      update_leaflet_ind(od = input$od_bbox, temp = temp, spp = sp_p, tbi = tbi, mapId = "mapBbox", session = session) # update 'mapBbox'
    } else if (input$dest_group == "Tract") {
      tmp <- get_tbi_od(od = input$od_bbox, temp = temp, spp = sp_p, tbi = tbi) # get all corresponding o-d pairs
      allLines <- tmp[[1]]
      endPoints <- tmp[[2]]
      if (length(allLines) == 0) return() # do nothing if there is no o-d pairs corresponding to the selected area
      if (input$linked_odbb) {
        all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
      } else {
        all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts)]
      }
      # convert all_points from data.table to spatial points 
      all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
      proj4string(all_points) <- proj4string(tracts)
      # project all points to tracts for summary
      tract_points <- data.table(cbind(all_points@data, sp::over(all_points, tracts)))
      # summarize by tract and destinations - all have the same origin (centroid of the polygon)
      temp2 <- na.omit(tract_points[, .(N = sum(N)), 
                                   by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), 
                                          dest_lat = as.numeric(INTPTLAT10))])[, `:=` (orig_lon = cntrd[,1], orig_lat = cntrd[,2])]
      temp2 <- temp2[N >= input$ignoreN_bbox]
      temp2[, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
      if (nrow(temp2) == 0) return()
      if (input$heat_bbox) {
        if (!is.null(track_clicks$curr_box) & length(track_clicks$curr_box) > 0) {
          for (i in 1:length(track_clicks$curr_box)) {
            sp_temp <- track_clicks$curr_box[[i]]
            if (input$od_bbox == 'orig') {
              # get all o-d pairs which destination is in within sp_temp
              spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                             data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
              proj4string(spOD) <- proj4string(sp_temp)
              od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
              ids <- od$ID
              endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            } else if (input$od_bbox == 'dest') {
              # get all o-d pairs which origin is in within sp_temp
              spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                   .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                              data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
              proj4string(spOD2) <- proj4string(sp_temp)
              od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
              ids <- od2$ID
              endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            } else {
              # get all o-d pairs which destination or origin is in within sp_temp
              spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                             data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
              proj4string(spOD) <- proj4string(sp_temp)
              od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
              spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                   .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                              data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
              proj4string(spOD2) <- proj4string(sp_temp)
              od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
              ids <- c(od$ID, od2$ID)
              ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
              ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
              endPoints <- rbind(ep1, ep2)
            }
            if (i == 1) {
              all_ids <- ids
              all_points <- endPoints
            } else {
              all_ids <- c(all_ids, ids)
              all_points <- rbind(all_points, endPoints)
            }
          }
          if (input$linked_odbb) {
            all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
          } else {
            all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts)]
          }
          # project points to tracts for summary at tract-level
          all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
          proj4string(all_points) <- proj4string(tracts)
          tract_points <- data.table(cbind(all_points@data, sp::over(all_points, tracts)))
          temp3 <- na.omit(tract_points[, .(N = sum(N)), by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])
          all_temp <- rbind(temp2[, .(GEOID10, dest_lon, dest_lat, N)], temp3)[, .(N = sum(N)), by = .(GEOID10)][N >= input$ignoreN_bbox][, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
          pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(all_temp$colN))
          dest_bbox <- subset(sp::merge(tracts, all_temp, by = "GEOID10"), !is.na(N)) # all destinations
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = do.call('rbind', track_clicks$curr_box), 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>% # all previous origins
            addPolygons(data = sp_p, 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>% # current origin
            addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                        label = ~paste(round(N, 0))) # all destinations
        } else {
          # if no previous box has been drawn, add current one only 
          dest_bbox <- subset(sp::merge(tracts, temp2[, .(GEOID10, N, colN)], by = "GEOID10"), !is.na(N))
          pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(temp2$colN))
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = sp_p, 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
            addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                        label = ~paste(round(N, 0)))
        }
      } else {
        # get flows from lat/lon of origins and destinations
        flows <- gcIntermediate(temp2[,5:6], temp2[,2:3], sp = TRUE, addStartEnd = TRUE) # return spatial lines
        flows$counts <- round(temp2$N, 0)
        flows$destinations <- temp2$GEOID10
        flows$colN <- temp2$colN
        hover <- paste0("To ", 
                        flows$destinations, ': ', 
                        as.character(flows$counts))
        pal <- colorNumeric(c("#a6bddb", "#0570b0", "#023858"), domain = range(temp2$colN))
        leafletProxy('mapBbox', session) %>% 
          addPolygons(data = subset(tracts, GEOID10 %in% temp2$GEOID10), weight = 1, opacity = 1, fillOpacity = .2, color = "#008144") %>%
          addPolygons(data = sp_p, weight = 1, opacity = 1, fillOpacity = .2, color = "#ED1B2E") %>%
          addPolylines(data = flows, weight = ~log(counts, 2)+1, label = hover, 
                       group = ~destinations, color = ~pal(colN), opacity = 1)
      }
    } else if (input$dest_group == "Block Group") {
      tmp <- get_tbi_od(od = input$od_bbox, temp = temp, spp = sp_p, tbi = tbi) # get all o-d pairs corresponding to sp_p
      allLines <- tmp[[1]]
      endPoints <- tmp[[2]]
      if (length(allLines) == 0) return()
      if (input$linked_odbb) {
        all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
      } else {
        all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts)]
      }
      # project points to block groups for summary
      all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
      proj4string(all_points) <- proj4string(blockgroups)
      bg_points <- data.table(cbind(all_points@data, sp::over(all_points, blockgroups)))
      temp2 <- na.omit(bg_points[, .(N = sum(N)), 
                                by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])[, `:=` (orig_lon = cntrd[,1], orig_lat = cntrd[,2])]
      temp2 <- temp2[N >= input$ignoreN_bbox]
      temp2[, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
      if (nrow(temp2) == 0) return()
      if (input$heat_bbox) {
        if (!is.null(track_clicks$curr_box) & length(track_clicks$curr_box) > 0) {
          for (i in 1:length(track_clicks$curr_box)) {
            sp_temp <- track_clicks$curr_box[[i]]
            if (input$od_bbox == 'orig') {
              spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                             data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
              proj4string(spOD) <- proj4string(sp_temp)
              od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
              ids <- od$ID
              endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            } else if (input$od_bbox == 'dest') {
              spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                   .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                              data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
              proj4string(spOD2) <- proj4string(sp_temp)
              od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
              ids <- od2$ID
              endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            } else {
              spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                             data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
              proj4string(spOD) <- proj4string(sp_temp)
              od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
              spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                   .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                              data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
              proj4string(spOD2) <- proj4string(sp_temp)
              od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
              ids <- c(od$ID, od2$ID)
              ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
              ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
              endPoints <- rbind(ep1, ep2)
            }
            if (i == 1) {
              all_ids <- ids
              all_points <- endPoints
            } else {
              all_ids <- c(all_ids, ids)
              all_points <- rbindlist(list(all_points, endPoints))
            }
          }
          if (input$linked_odbb) {
            all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
          } else {
            all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts)]
          }
          all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
          proj4string(all_points) <- proj4string(blockgroups)
          bg_points <- data.table(cbind(all_points@data, sp::over(all_points, blockgroups)))
          temp3 <- na.omit(bg_points[, .(N = sum(N)), by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])
          all_temp <- rbind(temp2[, .(GEOID10, dest_lon, dest_lat, N)], temp3)[, .(N = sum(N)), by = .(GEOID10)][N >= input$ignoreN_bbox][, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
          pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(all_temp$colN))
          dest_bbox <- subset(sp::merge(blockgroups, all_temp, by = "GEOID10"), !is.na(N))
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = do.call('rbind', track_clicks$curr_box), 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
            addPolygons(data = sp_p, 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
            addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                        label = ~paste(round(N, 0)))
        } else {
          dest_bbox <- subset(sp::merge(blockgroups, temp2[, .(GEOID10, N, colN)], by = "GEOID10"), !is.na(N))
          pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(temp2$colN))
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = sp_p, 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
            addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                        label = ~paste(round(N, 0)))
        }
      } else {
        flows <- gcIntermediate(temp2[,5:6], temp2[,2:3], sp = TRUE, addStartEnd = TRUE)
        flows$counts <- round(temp2$N, 0)
        flows$destinations <- temp2$GEOID10
        flows$colN <- temp2$colN
        hover <- paste0("To ", 
                        flows$destinations, ': ', 
                        as.character(flows$counts))
        pal <- colorNumeric(c("#a6bddb", "#0570b0", "#023858"), domain = range(temp2$colN))
        leafletProxy('mapBbox', session) %>% 
          addPolygons(data = subset(blockgroups, GEOID10 %in% temp2$GEOID10), weight = 1, opacity = 1, fillOpacity = .2, color = "#008144") %>%
          addPolygons(data = sp_p, weight = 1, opacity = 1, fillOpacity = .2, color = "#ED1B2E") %>%
          addPolylines(data = flows, weight = ~log(counts, 2)+1, label = hover, 
                       group = ~destinations, color = ~pal(colN), opacity = 1)
      }
    }
    track_clicks$curr_box <- c(track_clicks$curr_box, sp_p)
  })
  
  observeEvent(c(input$dest_group, input$ignoreN_bbox, input$rf_bbox, input$rtf_bbox, input$tpf_bbox, input$linked_odbb, input$heat_bbox, input$od_bbox), {
    sp_p <- track_clicks$curr_box
    if (length(sp_p) == 0) return()
    leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers()
    if (!is.null(input$rf_bbox)) {
      temp <- tbi[route %in% input$rf_bbox]
    } else {
      temp <- tbi
    }
    if (!is.null(input$rtf_bbox)) {
      temp <- temp[RouteType %in% input$rtf_bbox]
    }
    if (!is.null(input$tpf_bbox)) {
      temp <- temp[TIME_PERIOD %in% input$tpf_bbox]
    }
    if (input$dest_group == "None") {
      for (i in 1:length(sp_p)) {
        sp_temp <- sp_p[[i]]
        if (input$od_bbox == 'orig') {
          spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                         data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
          proj4string(spOD) <- proj4string(sp_temp)
          od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
          ids <- od$ID
          allLines <- do.call('rbind', od_lines[ids])
          endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), TRANSFERS_FROM_FIRST_CODE, 
                                          TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                          TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                          TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route, unlinked_wgts, LINKED_MULTIPLIER)]
        } else if (input$od_bbox == 'dest') {
          spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                               .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                          data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
          proj4string(spOD2) <- proj4string(sp_temp)
          od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
          ids2 <- od2$ID
          allLines <- do.call('rbind', od_lines[ids2])
          endPoints <- tbi[ID %in% ids2, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), TRANSFERS_FROM_FIRST_CODE, 
                                           TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                           TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                           TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route, unlinked_wgts, LINKED_MULTIPLIER)]
        } else {
          spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                         data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
          proj4string(spOD) <- proj4string(sp_temp)
          od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
          ids <- od$ID
          spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                               .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                          data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
          proj4string(spOD2) <- proj4string(sp_temp)
          od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
          ids2 <- od2$ID
          allLines <- do.call('rbind', od_lines[c(ids, ids2)])
          ep1 <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), TRANSFERS_FROM_FIRST_CODE, 
                                    TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                    TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                    TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route, unlinked_wgts, LINKED_MULTIPLIER)]
          ep2 <- tbi[ID %in% ids2, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), TRANSFERS_FROM_FIRST_CODE, 
                                     TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                     TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                     TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route, unlinked_wgts, LINKED_MULTIPLIER)]
          endPoints <- rbind(ep1, ep2)
        }
        if (length(allLines) == 0) return()
        leafletProxy('mapBbox', session) %>% 
          addPolygons(data = sp_temp, color = "#ED1B2E", opacity = 0.5, weight = 1) %>%
          addCircleMarkers(data = endPoints, lng = ~lon, lat = ~lat, radius = 4, opacity = .7, weight = 1, color = "#0053A0", 
                           popup = ~paste0("Route taken: ", route, " ", ifelse(is.na(TRANSFERS_FROM_FIRST_CODE), "", TRANSFERS_FROM_FIRST_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_FROM_SECOND_CODE), "", TRANSFERS_FROM_SECOND_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_FROM_THIRD_CODE), "", TRANSFERS_FROM_THIRD_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_FROM_FOURTH_CODE), "", TRANSFERS_FROM_FOURTH_CODE), " ",
                                           ifelse(is.na(TRANSFERS_TO_FIRST_CODE), "", TRANSFERS_TO_FIRST_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_TO_SECOND_CODE), "", TRANSFERS_TO_SECOND_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_TO_THIRD_CODE), "", TRANSFERS_TO_THIRD_CODE), " ", 
                                           ifelse(is.na(TRANSFERS_TO_FOURTH_CODE), "", TRANSFERS_TO_FOURTH_CODE),
                                           "<br>Time period: ", TIME_PERIOD), layerId = ~paste0("dest", ID)) %>%
          addPolylines(data = allLines, weight = 0.7, opacity = .3, color = "#0053A0")
      }
    } else if (input$dest_group == "Tract") {
      if (input$heat_bbox) {
        for (i in 1:length(sp_p)) {
          sp_temp <- sp_p[[i]]
          if (input$od_bbox == 'orig') {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(sp_temp)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
            ids <- od$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else if (input$od_bbox == 'dest') {
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(sp_temp)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
            ids <- od2$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(sp_temp)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(sp_temp)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
            ids <- c(od$ID, od2$ID)
            ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            endPoints <- rbind(ep1, ep2)
          }
          if (i == 1) {
            all_ids <- ids
            all_points <- endPoints
          } else {
            all_ids <- c(all_ids, ids)
            all_points <- rbindlist(list(all_points, endPoints))
          }
        }
        if (input$linked_odbb) {
          all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
        } else {
          all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts)]
        }
        all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
        proj4string(all_points) <- proj4string(tracts)
        tract_points <- data.table(cbind(all_points@data, sp::over(all_points, tracts)))
        temp3 <- na.omit(tract_points[, .(N = sum(N)), by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])
        all_temp <- temp3 [, .(N = sum(N)), by = .(GEOID10)][N >= input$ignoreN_bbox][, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
        pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(all_temp$colN))
        dest_bbox <- subset(sp::merge(tracts, all_temp, by = "GEOID10"), !is.na(N))
        leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
          addPolygons(data = do.call('rbind', sp_p), 
                      weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
          addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                      label = ~paste(round(N, 0)))
      } else {
        for (i in 1:length(sp_p)) {
          tempi <- sp_p[[i]]
          cntrd <- centroid(tempi)
          if (input$od_bbox == 'orig') {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(tempi)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, tempi))))
            ids <- od$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else if (input$od_bbox == 'dest') {
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(tempi)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, tempi))))
            ids <- od2$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(tempi)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, tempi))))
            ids <- od$ID
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(tempi)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, tempi))))
            ids <- c(od$ID, od2$ID)
            ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            endPoints <- rbind(ep1, ep2)
          }
          if (input$linked_odbb) {
            all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
          } else {
            all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts)]
          }
          all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
          proj4string(all_points) <- proj4string(tracts)
          tract_points <- data.table(cbind(all_points@data, sp::over(all_points, tracts)))
          temp2 <- na.omit(tract_points[, .(N = sum(N)), 
                                        by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])[, `:=` (orig_lon = cntrd[,1], orig_lat = cntrd[,2])]
          temp2 <- temp2[N >= input$ignoreN_bbox]
          temp2[, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
          if (nrow(temp2) == 0) return()
          flows <- gcIntermediate(temp2[,5:6], temp2[,2:3], sp = TRUE, addStartEnd = TRUE)
          flows$counts <- round(temp2$N, 0)
          flows$destinations <- temp2$GEOID10
          flows$colN <- temp2$colN
          hover <- paste0("To ", flows$destinations, ': ', as.character(flows$counts))
          pal <- colorNumeric(c("#a6bddb", "#0570b0", "#023858"), domain = range(temp2$colN))
          leafletProxy('mapBbox', session) %>% 
            addPolygons(data = subset(tracts, GEOID10 %in% temp2$GEOID10), weight = 1, opacity = 1, fillOpacity = .2, color = "#008144") %>%
            addPolygons(data = tempi, weight = 1, opacity = 1, fillOpacity = .2, color = "#ED1B2E") %>%
            addPolylines(data = flows, weight = ~log(counts, 2)+1, label = hover, 
                         group = ~destinations, color = ~pal(colN), opacity = 1)
        }
      }
    } else if (input$dest_group == "Block Group") {
      if (input$heat_bbox) {
        for (i in 1:length(sp_p)) {
          sp_temp <- sp_p[[i]]
          if (input$od_bbox == 'orig') {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(sp_temp)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
            ids <- od$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else if (input$od_bbox == 'dest') {
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(sp_temp)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
            ids <- od2$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(sp_temp)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, sp_temp))))
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(sp_temp)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, sp_temp))))
            ids <- c(od$ID, od2$ID)
            ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            endPoints <- rbind(ep1, ep2)
          }
          if (i == 1) {
            all_ids <- ids
            all_points <- endPoints
          } else {
            all_ids <- c(all_ids, ids)
            all_points <- rbindlist(list(all_points, endPoints))
          }
        }
        if (input$linked_odbb) {
          all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
        } else {
          all_points <- all_points[, .(lon, lat, ID, N = unlinked_wgts)]
        }
        if (nrow(all_points) == 0 ) {
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = do.call('rbind', sp_p), 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black")
        } else {
          all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
          proj4string(all_points) <- proj4string(blockgroups)
          bg_points <- data.table(cbind(all_points@data, sp::over(all_points, blockgroups)))
          temp3 <- na.omit(bg_points[, .(N = sum(N)), by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])
          all_temp <- temp3 [, .(N = sum(N)), by = .(GEOID10)][N >= input$ignoreN_bbox][, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
          pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(all_temp$colN))
          dest_bbox <- subset(sp::merge(blockgroups, all_temp, by = "GEOID10"), !is.na(N))
          leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers() %>%
            addPolygons(data = do.call('rbind', sp_p), 
                        weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
            addPolygons(data = dest_bbox, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                        label = ~paste(round(N, 0)))
        }
      } else {
        for (i in 1:length(sp_p)) {
          tempi <- sp_p[[i]]
          cntrd <- centroid(tempi)
          if (input$od_bbox == 'orig') {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(tempi)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, tempi))))
            ids <- od$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else if (input$od_bbox == 'dest') {
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(tempi)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, tempi))))
            ids <- od2$ID
            endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
          } else {
            spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                           data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
            proj4string(spOD) <- proj4string(tempi)
            od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, tempi))))
            ids <- od$ID
            spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                                 .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                            data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
            proj4string(spOD2) <- proj4string(tempi)
            od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, tempi))))
            ids <- c(od$ID, od2$ID)
            ep1 <- tbi[ID %in% od$ID, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            ep2 <- tbi[ID %in% od2$ID, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), unlinked_wgts, LINKED_MULTIPLIER)]
            endPoints <- rbind(ep1, ep2)
          }
          if (input$linked_odbb) {
            all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts * LINKED_MULTIPLIER)]
          } else {
            all_points <- endPoints[, .(lon, lat, ID, N = unlinked_wgts)]
          }
          if (nrow(all_points) > 0) {
            all_points <- SpatialPointsDataFrame(all_points[, .(lon, lat)], all_points[, .(ID, N)])
            proj4string(all_points) <- proj4string(blockgroups)
            bg_points <- data.table(cbind(all_points@data, sp::over(all_points, blockgroups)))
            temp2 <- na.omit(bg_points[, .(N = sum(N)), 
                                       by = .(GEOID10, dest_lon = as.numeric(INTPTLON10), dest_lat = as.numeric(INTPTLAT10))])[, `:=` (orig_lon = cntrd[,1], orig_lat = cntrd[,2])]
            temp2 <- temp2[N >= input$ignoreN_bbox]
            temp2[, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
            if (nrow(temp2) == 0) return()
            flows <- gcIntermediate(temp2[,5:6], temp2[,2:3], sp = TRUE, addStartEnd = TRUE)
            flows$counts <- round(temp2$N, 0)
            flows$destinations <- temp2$GEOID10
            flows$colN <- temp2$colN
            hover <- paste0("To ", flows$destinations, ': ', as.character(flows$counts))
            pal <- colorNumeric(c("#a6bddb", "#0570b0", "#023858"), domain = range(temp2$colN))
            leafletProxy('mapBbox', session) %>% 
              addPolygons(data = subset(blockgroups, GEOID10 %in% temp2$GEOID10), weight = 1, opacity = 1, fillOpacity = .2, color = "#008144") %>%
              addPolygons(data = tempi, weight = 1, opacity = 1, fillOpacity = .2, color = "#ED1B2E") %>%
              addPolylines(data = flows, weight = ~log(counts, 2)+1, label = hover, 
                           group = ~destinations, color = ~pal(colN), opacity = 1)
          }
        }
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$resetBbox, {
    leafletProxy('mapBbox', session) %>% clearShapes() %>% clearMarkers()
    track_clicks$bbox_clicks <- c()
    track_clicks$curr_box <- list()
  })
  
  output$dlodbb <- downloadHandler(
    filename = "ODMapByBoundingBox.html", 
    content = function(file) {
      src <- normalizePath('mymap_bbox.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_bbox.Rmd')
      
      out <- rmarkdown::render('mymap_bbox.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_odbb <- downloadHandler(
    filename = "ODMapByBoundingBox.pdf", 
    content = function(file) {
      src <- normalizePath('mymap_bbox.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mymap_bbox.Rmd')
      
      out <- webshot(rmarkdown::render('mymap_bbox.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
  
  # dot density map ----
  # output$ddMap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
  #              attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
  #              under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
  #              Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
  #              under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  #     setView(lat = 44.963, lng = -93.22, zoom = 11)
  # })
  # 
  # observeEvent(input$action_dd, {
  #   if (length(input$route_dd) == 0) return()
  #   route_map <- subset(routes, route %in% input$route_dd)
  #   if (input$measure_dd == "Origin") {
  #     temp <- na.omit(tbi[route %in% input$route_dd, .(route, lat = ORIGIN_LAT, lon = ORIGIN_LON, BOARDING_LOCATION, ALIGHTING_LOCATION, ID)])
  #   } else if (input$measure_dd == "Destination") {
  #     temp <- na.omit(tbi[route %in% input$route_dd, .(route, lat = DESTINATION_LAT, lon = DESTINATION_LON, BOARDING_LOCATION, ALIGHTING_LOCATION, ID)])
  #   } else if (input$measure_dd == "Home/Hotel") {
  #     temp <- na.omit(tbi[route %in% input$route_dd, .(route, lat = HOME_OR_HOTEL_ADDR_LAT, lon = HOME_OR_HOTEL_ADDR_LON, BOARDING_LOCATION, ALIGHTING_LOCATION, ID)])
  #   }
  #   pal <- colorNumeric(c("#0053A0", "#008144", "#F68A1E"), domain = c(min(as.numeric(input$route_dd)), max(as.numeric(input$route_dd))))
  #   if (input$plot_cluster_dd) {
  #     leafletProxy('ddMap', session) %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls() %>% clearShapes() %>%
  #       addPolylines(data = route_map, color = "#008144", weight = 4, opacity = 1) %>%
  #       addCircleMarkers(data = temp, lng = ~as.numeric(lon), lat = ~as.numeric(lat), color = ~pal(as.numeric(route)),
  #                        weight = 1, opacity = 0.5, radius = 5, popup = ~paste0("From: ", BOARDING_LOCATION, "<br>To: ", ALIGHTING_LOCATION, "<br>ID: ", ID), 
  #                        clusterOptions = markerClusterOptions(freezeAtZoom = 15)) %>% # add this option to plot clusters instead of individual points
  #       addLegend(position = 'bottomleft', colors = pal(sort(as.numeric(input$route_dd))), labels = paste0("Route", input$route_dd))
  #   } else {
  #     leafletProxy('ddMap', session) %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls() %>% clearShapes() %>%
  #       addPolylines(data = route_map, color = "#008144", weight = 4, opacity = 1) %>%
  #       addCircleMarkers(data = temp, lng = ~as.numeric(lon), lat = ~as.numeric(lat), color = ~pal(as.numeric(route)),
  #                        weight = 1, opacity = 0.5, radius = 5, popup = ~paste0("From: ", BOARDING_LOCATION, "<br>To: ", ALIGHTING_LOCATION, "<br>ID: ", ID)) %>%
  #       addLegend(position = 'bottomleft', colors = pal(sort(as.numeric(input$route_dd))), labels = paste0("Route", input$route_dd))
  #   }
  # })
  # 
  # output$dlddm <- downloadHandler(
  #   filename = "DotDensityMap.html", 
  #   content = function(file) {
  #     src <- normalizePath('dotdensity.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'dotdensity.Rmd')
  #     
  #     out <- rmarkdown::render('dotdensity.Rmd',
  #                              output_format = "html_document"
  #     )
  #     file.rename(out, file)
  #   }
  # )
  # output$pdf_ddm <- downloadHandler(
  #   filename = "DotDensityMap.pdf", 
  #   content = function(file) {
  #     src <- normalizePath('dotdensity.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'dotdensity.Rmd')
  #     
  #     out <- webshot(rmarkdown::render('dotdensity.Rmd',
  #                              output_format = "html_document"
  #     ), file = file)
  #     out
  #   }
  # )
  
  # flow charts ----
  dataInputFlow <- reactive({
    if (input$time_flow == "All") {
      f_data <- on_off
    } else {
      f_data <- on_off[TIME_PERIOD == input$time_flow]
    }
    if (input$dir_flow != "Any") {
      f_data <- f_data[Dir == input$dir_flow]
    }
    f_data <- f_data[line_id == input$route_flow, .(boardingStopCluster, alightingStopCluster)][, .(N = .N), by = .(boardingStopCluster, alightingStopCluster)]
    if (input$route_flow == 10) {
      clusterLevels <- c("downtown", "CE4S - LWCE", "CE26 - 40CE", "41CE", "CE41 - 51CE", "53TA - 534S", "53UV - UV81", "CE52 - CEMI", "CE66 - CE76", "OSCE - OSQU", "OSMN - 85JF", "NOTW")
    } else if (input$route_flow == 2) {
      clusterLevels <- c("22HE - NIFR", "FRST - FRHI", "FRMI - CEWA", "ANHA - 4S15", "4S13 - 7E2E")
    } else if (input$route_flow == 3) {
      clusterLevels <- c("downtown MPLS", "CEWA - 8S15", "15RO - COES", "COBF - COWI", "COSN - RICH", "downtown SP")
    } else if (input$route_flow == 4) {
      clusterLevels <- c("82ST - 46BR", "45BR - DULI", "downtown", "5SHE - 37JO", "37HA - FRH8", "H83S - O81A")
    } else if (input$route_flow == 5) {
      clusterLevels <- c("MOA - Portland/78th", "Chicago/59th - Portland/77th", "56CH - ARCL", "CHLA - 8SCH", "downtown", "7th/Olson - 26th/W Broadway", "44FM - 26BR", "44th/Girard - BCTC")
    } else if (input$route_flow == 6) {
      clusterLevels <- c("78PI - 77SW", "77PA - FEDX", "YKHZ - 39SH", "RIWB - HEOG", "downtown", "UN2A - 27UN")
    } else if (input$route_flow == 9) {
      clusterLevels <- c("HEBE - CLNE", "LUTC - GLAL", "downtown", "POGR - 3633", "3634 - 46HI")
    } else if (input$route_flow == 11) {
      clusterLevels <- c("49WE - 464A", "454A - 4ALA", "284A - 183A", "downtown", "2S3A - 29GR", "30RA - 41CE")
    } else if (input$route_flow == 14) {
      clusterLevels <- c("6617 - BL54", "BL53 - 38BL", "38HI - BLLA", "BL29 - 7S10", "downtown", "WA5A - GVKN", "GVMO - MDOT", "DUAD - 36OR", "BRKN - RBTC")
    } else if (input$route_flow == 16) {
      clusterLevels <- c("27UN - RAST", "UNHA - SNUN", "UNSI - OXUN", "UNCH - UNDA", "UNKE - RAMA", "downtown")
    } else if (input$route_flow == 17) {
      clusterLevels <- c("TYLA - MTTE", "MTPE - LAFR", "37BR - LAFR", "BEL7 - 31IR", "HELA - NI15", "downtown", "CE4S - 27WA")
    } else if (input$route_flow == 18) {
      clusterLevels <- c("104BL - NI81", "AM2A - 66HC", "66NI - 47NI", "46NI - 32NI", "31NI - 32GR", "31GR - 15NI", "downtown")
    } else if (input$route_flow == 19) {
      clusterLevels <- c("downtown", "7SOL - PE55", "PE8A - PE30", "LWPE - 36YO", "LWPE - 42YK", "PEDO - 47OS", "BR49 - BCTC")
    } else if (input$route_flow == 21) {
      clusterLevels <- c("UPTS - BLLK", "1ALA - CELA", "19LA - 44LA", "LARP - UNSN", "SNUN - HAMA", "SLHA - SLNI", "downtown")
    } else if (input$route_flow == 22) {
      clusterLevels <- c("VETS - 3458", "58SA - 2842", "4228 - CELA", "LA17 - CEWA", "downtown", "7SOL - LNLY", "LY33 - 45AL", "45BR - 56DP", "57DP - 69HU", "57FR - BCTC")
    } else if (input$route_flow == 54) {
      clusterLevels <- c("MOA", "LI26 - 34AM", "Airport", "7MAY - 7SGR", "downtown")
    } else if (input$route_flow == 63) {
      clusterLevels <- c("BRUN - SUCR", "GRCR - GRSN", "GRSA - SMGR", "downtown", "3MAR - 3SWB", "OHHZ - MCKN", "MKLO - LAMK")
    } else if (input$route_flow == 64) {
      clusterLevels <- c("downtown", "LA600 - BUMI", "PARE - MYCL", "PRMA - 11WH", "WB2570 - MPWD")
    } else if (input$route_flow == 68) {
      clusterLevels <- c("CFCA - 80CH", "78CH - 70CL", "CL69 - 5ASP", "5ADA - SV9A", "SV11 - ROTH", "ROEM - ROCO", "downtown", "JAMA - CACM")
    } else if (input$route_flow == 74) {
      clusterLevels <- c("46HI - CLRA", "RAKE - 7RAN", "7TOR - 7SGR", "downtown", "7BAT - 7SVD", "7SHZ - SURA")
    } else if (input$route_flow == 84) {
      clusterLevels <- c("DASH - CLFO", "KEFO - RASN", "RABR - UNSN", "SNCH - ROSE")
    } else if (input$route_flow == 888) {
      clusterLevels <- c("Target Field", "Fridley", "Coon Rapids Riverdale", "Anoka", "Ramsey", "Elk River", "Big Lake")
    } else if (input$route_flow == 901) {
      clusterLevels <- c("AM34 - MAAM", "Airport terminals", "46HI - FTSN", "CDRV - 38HI", "downtown")
    } else if (input$route_flow == 902) {
      clusterLevels <- c("downtown Minneapolis", "U of M", "PSPK - SNUN", "HMUN - WEUN", "downtown St. Paul")
    } else if (input$route_flow == 903) {
      clusterLevels <- c("MOA", "Cedar Grove Transit Station", "Cedar Av at 140 St", "Cedar Av at 147 St", "Apple Valley Transitway")
    } else if (input$route_flow == 921) {
      clusterLevels <- c("46th St", "FORD", "Grand - Highland", "Minnehaha - Dayton", "SNLA - HEWITT", "ROSE - CO RD B")
    }
    f_data[, (1:2) := lapply(.SD, factor, levels = clusterLevels, ordered = TRUE), .SDcol = 1:2]
    f_data <- f_data[order(boardingStopCluster, alightingStopCluster)]
    setnames(f_data, c("Boarding Location", "Alighting Location", "N"))
    return(f_data)
  })
  
  output$flowchart <- renderParset({
    parset(dataInputFlow(), dimensions = c('Boarding Location', 'Alighting Location'),
           value = htmlwidgets::JS("function(d){return d.N}"),
           tension = 0.5)
  })
  
  output$text_flow <- renderUI({
    if (!input$route_flow %in% c(888, 903)) {
      HTML(paste0("<img src = 'route", input$route_flow, ".png' width=1000>"))
    }
    })
  
  output$n_flow <- renderUI({
    HTML(paste0("Sample size: <strong>", sum(dataInputFlow()$N), "</strong>"))
  })
  
  output$dlflow <- downloadHandler(
    filename = "FlowChart.html", 
    content = function(file) {
      src <- normalizePath('flowchart.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'flowchart.Rmd')
      
      out <- rmarkdown::render('flowchart.Rmd',
                               output_format = "html_document"
      )
      file.rename(out, file)
    }
  )
  output$pdf_flow <- downloadHandler(
    filename = "FlowChart.pdf", 
    content = function(file) {
      src <- normalizePath('flowchart.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'flowchart.Rmd')
      
      out <- webshot(rmarkdown::render('flowchart.Rmd',
                               output_format = "html_document"
      ), file = file)
      out
    }
  )
    })
