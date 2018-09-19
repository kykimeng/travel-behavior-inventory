shinyUI(
  fluidPage(
    tags$img(src = "mt_bar.png", width = '100%'),
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 97vh; overflow-y: auto;
                      }"
      ) # close HTML
      )            # close tags$style
      ), 
    dashboardPage(
      skin = 'black', 
      dashboardHeader(title = tagList(tags$img(src = "t_logo.png", height = '35px'), "Travel Behavior Inventory On-Board Survey 2016"), titleWidth = 510),
      # sidebar ----
      dashboardSidebar(width = 250, div(style="overflow-y: scroll"),
        sidebarMenu(
          id = 'menu', 
          # each menuItem and menuSubItem has to have a corresponding tabItem 
          menuItem(text = "About", icon = icon("list-alt"), tabName = 'abt'), 
          menuItem(text = "Interactive Plots", icon = icon("bar-chart"), tabName = "plots"), 
          menuItem(text = "O-D Map", icon = icon("map"), 
                   menuSubItem("by Route", tabName = "odroute"), # sub tabs
                   menuSubItem("by Block", tabName = "odblock"), 
                   menuSubItem("by TAZ", tabName = 'odtaz'), 
                   menuSubItem("by Tract", tabName = "odtract"), 
                   menuSubItem("by Custom Bounding Box", tabName = 'odbbox')), 
          menuItem(text = "Heat Map", icon = icon("map-o"), tabName = 'hm'), 
          # menuItem(text = "Dot Density Map", icon = icon("map-marker"), tabName = 'ddm'), 
          menuItem(text = "Flow Chart", icon = icon("exchange"), tabName = 'flow'), 
          menuItem(text = "Feedback", icon = icon("envelope-open"), tabName = 'fb')
        ), 
        tags$hr(),
        # input panel for Interactive Plots tab ----
        conditionalPanel(
          condition = "input.menu == 'plots'", 
          box(width = 12, background = 'blue',
          selectizeInput("y_var", "Primary:", c('None', varLabels)),
          selectizeInput('x_var', 'Secondary:', c('None', varLabels), selected = "ORIGIN_PLACE_TYPE"),
          actionBttn('swap', "Swap", size = 'xs'),
          # options to facet and plot percentages only available when both x and y are chosen
          conditionalPanel(
            condition = "!(input.x_var == 'None' | input.y_var == 'None' | input.x_var == input.y_var)", 
            selectizeInput('facet_by', "Facet by:", c('None', varLabels), selected = 'None'),
            checkboxInput('freq', "Plot Percentages", value = FALSE)),
          checkboxInput('linked', "Use Linked Weights", value = FALSE),
          tags$hr(), 
          actionButton('resetInputs', label = 'Reset'), 
         tags$hr(), 
         downloadButton('downloadPlot', label = "Save Plot"))
        ), 
        # input panel for by Route tab ----
        conditionalPanel(
          condition = "input.menu == 'odroute'",
          box(width = 12, background = 'blue',
          selectizeInput('measureOD', "Measure", choices = c("Origin - Destination", "Boarding Location - Alighting Location"), 
                         selected = "Origin - Destination"),
          selectInput('route', 'Route Surveyed', choices = sort(as.numeric(na.omit(unique(tbi_OD$route)))),
                      multiple = TRUE, selectize = FALSE, selected = '921'),
          h4(textOutput('routesSelected'), style = "color:#f7f7f7"),
          selectInput('od_time', "Time Period", choices = tbi[!is.na(TIME_PERIOD), unique(TIME_PERIOD)], multiple = TRUE),
          selectInput('od_age', "Age Group", choices = tbi[!is.na(AGE), unique(AGE)], multiple = TRUE),
          selectInput('od_race', "Race/Ethnicity", choices = tbi[!is.na(race_ethnicity), unique(race_ethnicity)], multiple = TRUE),
          selectInput("od_income", "Income Level", choices = tbi[!is.na(INCOME), unique(INCOME)], multiple = TRUE),
          tags$hr(),
          actionButton('plotButton', "Map it!"), 
          tags$hr(), 
          downloadButton('dlodr', "Download Interactive Map"), 
          downloadButton('pdf_odr', "Download PDF"))
        ), 
        # input panel for by Block tab ----
      conditionalPanel(
        condition = "input.menu == 'odblock'", 
        box(width = 12, background = 'blue',
            radioButtons('od_block', "Show:", choices = c("Destinations Only" = 'orig', "Origins Only" = 'dest', "Both Origins and Destinations" = 'both'), 
                         selected = 'both'),
        checkboxInput('block_cluster', "Group Origins/Destinations by Block", value = TRUE),
        p(em("Note: Expansion Factors are only applied when Origins/Destinations are grouped.")), 
        conditionalPanel(condition = "input.block_cluster == true", 
                         checkboxInput('heat_block', "Show Heat Map"),
                         checkboxInput('linked_odb', "Use Linked Weights"),
                         numericInput('ignoreN_block', "Ignore Blocks with Observations fewer than: ", value = 0)), 
        selectInput('rf_bl', "Route Filter:", choices = sort(as.numeric(na.omit(unique(tbi_OD$route)))),
                    multiple = TRUE, selectize = FALSE, selected = NULL),
        selectInput('rtf_bl', "Route Type Filter:", choices = sort(na.omit(unique(tbi$RouteType))), multiple = TRUE, selected = NULL), 
        selectInput('tpf_bl', "Time Period Filter:", choices = sort(na.omit(unique(tbi$TIME_PERIOD))), multiple = TRUE, selected = NULL), 
        tags$hr(),
        actionButton("resetGeo", "Clear Map"), 
        tags$hr(), 
        downloadButton('dlodb', "Download Interactive Map"), 
        downloadButton('pdf_odb', "Download PDF"))
      ), 
      # input panel for by TAZ tab ----
      conditionalPanel(
        condition = "input.menu == 'odtaz'", 
        box(width = 12, background = 'blue',
            radioButtons('stl_dat_taz', "Show:", choices = c("TBI Data Only", "Streetlight Data Only"), selected = "TBI Data Only"),
            radioButtons('od_taz', "Show:", choices = c("Destinations Only" = 'orig', "Origins Only" = 'dest', "Both Origins and Destinations" = 'both'), 
                         selected = 'both'),
            # inputs specific to TBI data
            conditionalPanel(
              condition = "input.stl_dat_taz == 'TBI Data Only'", 
              checkboxInput('taz_cluster', "Group Origins/Destinations by TAZ", value = TRUE),
              p(em("Note: Expansion Factors are only applied when Origins/Destinations are grouped.")), 
              conditionalPanel(condition = "input.taz_cluster == true", 
                               checkboxInput('heat_taz', "Show Heat Map"),
                               conditionalPanel(
                                 condition = "input.heat_taz == true", 
                                 checkboxInput('exc_sel_taz', "Exclude Selected TAZ", value = F)
                               ),
                               checkboxInput('linked_odtaz', "Use Linked Weights"),
                               numericInput('ignoreN_taz', "Ignore TAZs with Observations fewer than: ", value = 0)), 
              selectInput('rf_taz', "Route Filter:", choices = sort(as.numeric(na.omit(unique(tbi_OD$route)))),
                          multiple = TRUE, selectize = FALSE, selected = NULL),
              selectInput('rtf_taz', "Route Type Filter:", choices = sort(na.omit(unique(tbi$RouteType))), multiple = TRUE, selected = NULL), 
              selectInput('tpf_taz', "Time Period Filter:", choices = sort(na.omit(unique(tbi$TIME_PERIOD))), multiple = TRUE, selected = NULL)
            ), 
            # inputs specific to streetlight data
            conditionalPanel(
              condition = "input.stl_dat_taz == 'Streetlight Data Only'", 
              checkboxInput('heat_taz_sl', "Show Heat Map"),
              conditionalPanel(
                condition = "input.heat_taz_sl == true", 
                checkboxInput('exc_sel_taz_sl', "Exclude Selected TAZ", value = F)
              ),
              selectInput('dtf_taz_sl', "Day Type Filter:", choices = sort(na.omit(unique(sl_tract$`Day Type`))), selected = "1: Average Weekday (M-F)"),
              selectInput('tpf_taz_sl', "Time Period Filter:", choices = sort(na.omit(unique(sl_tract$`Day Part`))), 
                          selected = "0: All Day (12am-12am)", multiple = TRUE)
            ),
        tags$hr(), 
        actionButton("resetTaz", "Clear Map"), 
        tags$hr(), 
        downloadButton('dlodtaz', "Download Interactive Map"), 
        downloadButton('pdf_odtaz', "Download PDF"))
      ), 
      # input panel for by Tract tab ----
      conditionalPanel(
        condition = "input.menu == 'odtract'", 
        box(width = 12, background = 'blue',
            radioButtons('stl_dat', "Show:", choices = c("TBI Data Only", "Streetlight Data Only"), selected = "TBI Data Only"),
            radioButtons('od_tract', "Show:", choices = c("Destinations Only" = 'orig', "Origins Only" = 'dest', "Both Origins and Destinations" = 'both'), 
                         selected = 'both'),
            # inputs specific to TBI data
            conditionalPanel(
              condition = "input.stl_dat == 'TBI Data Only'",
              checkboxInput('tract_cluster', "Group Origins/Destinations by Tract", value = TRUE),
              p(em("Note: Expansion Factors are only applied when Origins/Destinations are grouped.")),
              conditionalPanel(condition = "input.tract_cluster == true", 
                               checkboxInput('heat_tract', "Show Heat Map"),
                               conditionalPanel(
                                 condition = "input.heat_tract == true", 
                                 checkboxInput('exc_sel_tract', "Exclude Selected Tract", value = F)
                               ),
                               checkboxInput('linked_odtr', "Use Linked Weights"),
                               numericInput('ignoreN', "Ignore Tracts with Observations fewer than: ", value = 0)),
              selectInput('rf_tr', "Select Route to Filter:", choices = sort(as.numeric(na.omit(unique(tbi_OD$route)))),
                          multiple = TRUE, selectize = FALSE, selected = NULL),
              selectInput('rtf_tr', "Route Type Filter:", choices = sort(na.omit(unique(tbi$RouteType))), multiple = TRUE, selected = NULL), 
              selectInput('tpf_tr', "Time Period Filter:", choices = sort(na.omit(unique(tbi$TIME_PERIOD))), multiple = TRUE, selected = NULL)
            ), 
            # inputs specific to streetlight data
            conditionalPanel(
              condition = "input.stl_dat == 'Streetlight Data Only'", 
              checkboxInput('heat_tr_sl', "Show Heat Map"),
              conditionalPanel(
                condition = "input.heat_tr_sl == true", 
                checkboxInput('exc_sel_tr_sl', "Exclude Selected Tract", value = F)
              ),
              selectInput('dtf_tr_sl', "Day Type Filter:", choices = sort(na.omit(unique(sl_tract$`Day Type`))), selected = "1: Average Weekday (M-F)"),
              selectInput('tpf_tr_sl', "Time Period Filter:", choices = sort(na.omit(unique(sl_tract$`Day Part`))), 
                          selected = "0: All Day (12am-12am)", multiple = TRUE)
            ), 
        tags$hr(), 
        actionButton("resetTract", "Clear Map"), 
        tags$hr(), 
        downloadButton('dlodtr', "Download Interactive Map"), 
        downloadButton('pdf_odtr', "Download PDF"))
      ), 
      # input panel for by Custom Bounding Box tab ----
      conditionalPanel(
        condition = "input.menu == 'odbbox'", 
        box(width = 12, background = 'blue',
            radioButtons('od_bbox', "Show:", choices = c("Destinations Only" = 'orig', "Origins Only" = 'dest', "Both Origins and Destinations" = 'both'), 
                         selected = 'both'),
        selectInput('dest_group', "Group Origins/Destinations by:", choices = c("None", "Block Group", "Tract")),
        p(em("Note: Expansion Factors are only applied when Origins/Destinations are grouped.")), 
        # further option when o/d is grouped
        conditionalPanel(condition = "input.dest_group != 'None'", 
                         checkboxInput('heat_bbox', "Show Heat Map"),
                         checkboxInput('linked_odbb', "Use Linked Weights"),
                         numericInput('ignoreN_bbox', "Ignore Block Group/Tract with Observations fewer than: ", value = 0)), 
        selectInput('rf_bbox', "Route Filter:", choices = sort(as.numeric(na.omit(unique(tbi_OD$route)))),
                    multiple = TRUE, selectize = FALSE, selected = NULL),
        selectInput('rtf_bbox', "Route Type Filter:", choices = sort(na.omit(unique(tbi$RouteType))), multiple = TRUE, selected = NULL), 
        selectInput('tpf_bbox', "Time Period Filter:", choices = sort(na.omit(unique(tbi$TIME_PERIOD))), multiple = TRUE, selected = NULL), 
        tags$hr(), 
        actionButton('mapItBbox', "Draw Box"),
        tags$hr(),
        actionButton('resetBbox', "Reset"), 
        tags$hr(), 
        downloadButton('dlodbb', "Download Interactive Map"), 
        downloadButton('pdf_odbb', "Download PDF"))
      ), 
      # input panel for Heat Map tab ----
      conditionalPanel(
        condition = "input.menu == 'hm'", 
        box(width = 12, background = 'blue',
            p(em("Note: Expansion factors are not applied here.")),
        selectizeInput('measure', "Measure", choices = c("Destination", "Origin", "Home/Hotel", "Boarding Location", 
                                                         "Alighting Location"), selected = "Home/Hotel"),
        selectInput('route_hm', 'Route Surveyed', choices = c("All", sort(as.numeric(na.omit(unique(tbi$route))))), 
                    selected = '921', multiple = TRUE, selectize = FALSE),
        h4(textOutput('routesSelected_hm'), style = "color:#f7f7f7"), 
        tags$hr(), 
        downloadButton('dlhm', "Download Interactive Map"), 
        downloadButton('pdf_hm', "Download PDF"))
      ), 
      # input panel for Dot Density Map tab ----
      # conditionalPanel(
      #   condition = "input.menu == 'ddm'", 
      #   box(width = 12, background = 'blue',
      #       p(em("Note: Expansion factors are not applied here.")),
      #   selectInput('route_dd', "Select Route:", choices = sort(as.numeric(na.omit(unique(tbi$route)))), multiple = TRUE, 
      #               selectize = TRUE), 
      #   selectInput('measure_dd', "Select Measure to Plot:", choices = c("Origin", "Destination", "Home/Hotel")),
      #   checkboxInput('plot_cluster_dd', "Plot clusters", value = T),
      #   tags$hr(), 
      #   actionButton('action_dd', "Map it!"), 
      #   tags$hr(), 
      #   downloadButton('dlddm', "Download Interactive Map"), 
      #   downloadButton('pdf_ddm', "Download PDF"), 
      #   br(),
      #   tags$em("Download PDF only works when dots are not clustered. If you want the dot cluster map, we recommend downloading the interactive map, and take a screenshot. Sorry for the inconvenience."))
      # ), 
      # input panel for Flow Chart tab ----
      conditionalPanel(
        condition = "input.menu == 'flow'", 
        box(width = 12, background = 'blue',
            p(em("Note: On and Off data do not have expansion factors.")),
        selectInput('route_flow', "Select Route:", choices = sort(unique(na.omit(on_off$line_id)))), 
        radioButtons('time_flow', "Time of Day:", choices = c("All", "AM PEAK", "MIDDAY", "PM PEAK", "EVENING"), inline = TRUE), 
        selectInput('dir_flow', "Direction:", choices = c("Any", "Northbound" = "NB", "Southbound" = "SB", "Westbound" = "WB", "Eastbound" = "EB"), selected = "Any"), 
        tags$hr(), 
        downloadButton('dlflow', "Download Interactive Flow Chart"), 
        downloadButton('pdf_flow', "Download PDF"))
      )
      ), 
      dashboardBody(
        tabItems(
          # about tab ----
          tabItem(
            'abt', 
            box(width = 6, status = 'primary', solidHeader = TRUE, title = "What this app does", 
                h5(strong("Interactive Plots"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("Users can plot, either by frequency or absolute values, for up to three variables at the same time.")), 
                  tags$li(p("Plots can be saved as PDFs."))
                ), 
                h5(strong("O-D Map"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("User can visualize the origin-destination data by route, census block, TAZ, census tract, or custom boundary box.")), 
                  tags$li(p("The origin-destination pairs can be grouped by geographic areas (block, TAZ, or tract), and/or further filtered by route or route type, and time period.")), 
                  tags$li(p("When looking at O-D pairs by Tract or TAZ, users have the options to use Streetlight data or TBI data.")),
                  tags$li(p("For a selected geographic area (i.e. censur block, TAZ, census tract, or custom bounding box), 'origins' refers to those o-d pairs
                             whose origins are within the selected area, 'destinations' refers to those o-d pairs whose destinations are within the 
                            selected area."))
                ), 
                h5(strong("Heat Map"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("Users can visualize the dense area of riders' homes/hotels, origins, destinations, boarding locations, or alighting locations."))
                ),
                h5(strong("Dot Density Map"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("Users can visualize origins, destinations, home/hotel addresses as dots on a map, or cluster them for better visualization."))
                ),
                h5(strong("Flow Chart"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("Users can look at flows between segments of a given route.")), 
                  tags$li(p("These flow charts use on-off data collection."))
                ),
                h5(strong("Feedback"), style = "color:#0053A0"), 
                tags$ul(
                  tags$li(p("Users can send the author feedback, comments or questions."))
                ), 
                tags$hr(), 
                p(em(paste0("This app was last updated on ", max(file.info('ui.R')$mtime, file.info('server.R')$mtime, 
                                                                 file.info('global.R')$mtime), 
                            " by Kim Ky (kykimeng@gmail.com).")))
                ), 
            box(
              width = 6, status = 'success', solidHeader = TRUE, title = "Credits where credits are due",
              p("The data, images and code supporting this Shiny app are all the property of Metro Transit, a division of 
                Metropolitan Council. The author built and designed this Shiny app during her tenure at 
                Metro Transit."), 
                br(),
                p("There may be some modifications to this app that the author has made during her own time which she 
                takes full responsibility for.")
            ),
            box(
              width = 6, status = 'primary', solidHeader = TRUE, title = "Data Sources", 
              h5(strong("TBI"), style = "color:#0053A0"),
              p("The Travel Behavior Inventory (TBI) is a comprehensive survey conducted every 10 years by the Metropolitan Council to assess how and how much 
                people in Minneapolis-St. Paul region and surrounding counties travel, including what mode of transportation they use, where they go, 
                and when. For a complete report, ", tags$a(href = "https://metrocouncil.org/Transportation/Publications-And-Resources/Travel-Behavior-Inventory.aspx", 
                                                           "CLICK HERE")), 
              h5(strong("Streetlight"), style = "color:#0053A0"), 
              p("Every month, Streetlight team processes over 60 billion new location records to keep their analytics up-to-date. Their sample size represents 
                about 23% of travel activity in the U.S. and Canada. Archival data from past month remain in their database, so you can track trends. They 
                evaluate and incorporate new data providers regularly so that they have best resources available. The type of data they use to infer 
                origin-destination include Location-Based Services data (from smartphone apps; used in this app) and Navigation-GPS data (from devices that 
                help people navigate). For additional information on Streetlight data, ", 
                tags$a(href = "https://www.streetlightdata.com/", "CLICK HERE"))
            )
          ), 
          # interactive plots tab ----
          tabItem(
            'plots', 
            box(width = 12, height = 850,
              plotlyOutput('statPlot')
            ), 
            fluidRow(
              htmlOutput("warningSmallSample", inline = FALSE),
              tags$head(tags$style("#warningSmallSample{color: #ED1B2E;
                                   font-size: 14px;
                                   font-style: italic;
                                   }"
                 )
              )
            ),
            box(width = 12, solidHeader = TRUE, status = 'primary', title = "Filter by:", 
                fluidRow(
                  column(4,
                         checkboxGroupInput('route_type', "Route Type Surveyed:",
                                            choices = init[, levels(RouteType)],
                                            inline = TRUE),
                         checkboxGroupInput('transfers_from', "Number of Prior Transfers:",
                                            choices = init[, levels(TRANSFERS_FROM)],
                                            inline = TRUE),
                         checkboxGroupInput('orig_type', "Origin Place Type:",
                                            choices = init[, levels(ORIGIN_PLACE_TYPE)],
                                            inline = TRUE),
                         checkboxGroupInput('access_mode', "Access Mode:",
                                            choices = init[, levels(ACCESS_MODE)],
                                            inline = TRUE), 
                         checkboxGroupInput('transfers_to', "Number of Transfers to:",
                                            choices = init[, levels(TRANSFERS_TO)],
                                            inline = TRUE),
                         checkboxGroupInput('dest_type', "Destination Place Type:", 
                                            choices = init[, levels(DESTIN_PLACE_TYPE)],
                                            inline = TRUE),
                         checkboxGroupInput('license', "Has Driver's License:",
                                            choices = init[, levels(HAS_DRIVE_LICENSE)],
                                            inline = TRUE),
                         checkboxGroupInput('age_group', "Age Group:",
                                            choices = init[, levels(AGE)],
                                            inline = TRUE),
                         checkboxGroupInput('race', "Race/Ethnicity:",
                                            choices = init[, levels(race_ethnicity)],
                                            inline = TRUE),
                         checkboxGroupInput('disab', "Have Disabilities:",
                                            choices = init[, levels(DISABILITY)],
                                            inline = TRUE)
                  ),
                  column(4,
                         selectInput('route_p', "Route:", choices = sort(as.numeric(unique(tbi$route))), 
                                     multiple = TRUE, selectize = FALSE),
                         actionButton('clear_route', "Clear Route Filter"),
                         checkboxGroupInput('gender', "Gender:",
                                            choices = init[, levels(GENDER)],
                                            inline = TRUE),
                         checkboxGroupInput('egress_mode', "Egress Mode:",
                                            choices = init[, levels(EGRESS_MODE)],
                                            inline = TRUE),
                         checkboxGroupInput('time_period', "Time Period:",
                                            choices = init[, levels(TIME_PERIOD)],
                                            inline = TRUE),
                         checkboxGroupInput('trip_opp_dir', "Trip in Opposite Direction:",
                                            choices = init[, levels(TRIP_IN_OPPOSITE_DIR)],
                                            inline = TRUE),
                         checkboxGroupInput('payment', "Payment Method:",
                                            choices = init[, levels(PAYMENT_METHOD)],
                                            inline = TRUE),
                         checkboxGroupInput('fare_type', "Fare Type:", 
                                            choices = init[, levels(FARE_TYPE)], 
                                            inline = TRUE), 
                         checkboxGroupInput('trip_purpose', "Trip Purpose:", 
                                            choices = init[, levels(trip_purpose)], 
                                            inline = TRUE)
                  ),
                  column(4,
                         checkboxGroupInput('is_visitor', "Is Visitor:",
                                            choices = init[, levels(VISITOR)],
                                            inline = TRUE),
                         checkboxGroupInput('hh_veh', "Household Vehicle Count:",
                                            choices = init[, levels(COUNT_VH_HH)],
                                            inline = TRUE),
                         checkboxGroupInput('use_veh_trip', "Can Use Vehicle for Trip:",
                                            choices = init[, levels(CAN_USE_VEH_TRIP)],
                                            inline = TRUE),
                         checkboxGroupInput('hh_members', "Number of Members in Household:",
                                            choices = init[, levels(COUNT_MEMBER_HH)],
                                            inline = TRUE),
                         checkboxGroupInput('hh_employed', "Number Employed in Household:",
                                            choices = init[, levels(COUNT_EMPLOYED_HH)],
                                            inline = TRUE),
                         checkboxGroupInput('employ_status', "Employment Status:",
                                            choices = init[, levels(STATUS_EMPLOYMENT)],
                                            inline = TRUE),
                         checkboxGroupInput('stud_status', "Student Status:",
                                            choices = init[, levels(STUDENT_STATUS)],
                                            inline = TRUE),
                         checkboxGroupInput('fare_subsidy', "Fare Subsidy:",
                                            choices = init[, levels(FARE_SUBSIDY)],
                                            inline = TRUE),
                         checkboxGroupInput('income', "Income Level:",
                                            choices = init[, levels(INCOME)],
                                            inline = TRUE),
                         checkboxGroupInput('home_lang', "Speak Language Other Than English at Home:",
                                            choices = init[, levels(HOME_LANG_OTHER)],
                                            inline = TRUE),
                         checkboxGroupInput('eng_ability', "English Ability:",
                                            choices = init[, levels(ENGLISH_ABILITY)],
                                            inline = TRUE)
                  )
                ))
          ), 
          # o-d route tab ----
          tabItem(
            'odroute',
            tags$style(type = "text/css", "#myMap {height: calc(100vh - 100px) !important;}"), # myMap height adjusts with the screen
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading... Please wait.",id="loadmessage")
            ), # loading message
            leafletOutput('myMap', width = '100%'), 
            absolutePanel(p("Little circles are destination points. Click on them for additional info."), 
                          top = 80, right = 20, draggable = F, background = "#ffffff")
          ), 
          # o-d block tab ----
          tabItem(
            'odblock', 
            tags$style(type = "text/css", "#mapGeo {height: calc(100vh - 100px) !important;}"),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading... Please wait.",id="loadmessage")
            ),
            leafletOutput('mapGeo', width = '100%')
          ), 
          # o-d taz tab ----
          tabItem(
            'odtaz',
            tags$style(type = "text/css", "#mapTaz {height: calc(100vh - 100px) !important;}"),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading... Please wait.",id="loadmessage")
            ),
            leafletOutput('mapTaz', width = '100%')
          ), 
          # o-d tract tab ----
          tabItem(
            'odtract',
            tags$style(type = "text/css", "#mapTract {height: calc(100vh - 100px) !important;}"),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading... Please wait.",id="loadmessage")
            ),
            leafletOutput('mapTract', width = '100%')
          ), 
          # o-d custom bounding box tab ----
          tabItem(
            'odbbox', 
            tags$style(type = "text/css", "#mapBbox {height: calc(100vh - 100px) !important;}"),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading... Please wait.",id="loadmessage")
            ),
            leafletOutput('mapBbox', width = '100%')
          ), 
          # heatmap tab ----
          tabItem(
            'hm',
            tags$style(type = "text/css", "#heatmap {height: calc(100vh - 100px) !important;}"),
            leafletOutput('heatmap', width = '100%')
          ), 
          # dot-density map tab ----
          # tabItem(
          #   'ddm',
          #   tags$style(type = "text/css", "#ddMap {height: calc(100vh - 100px) !important;}"),
          #   leafletOutput('ddMap', width = '100%')
          # ), 
          # flow chart tab ----
          tabItem(
            'flow',
            parsetOutput('flowchart', height = "600px"), 
            tags$hr(), 
            htmlOutput('n_flow'), 
            tags$hr(),
            htmlOutput('text_flow')
          ), 
          # feedback tab ----
          tabItem(
            'fb', 
            box(width = 6, 
                p('Have you found a problem with the app? Do you have questions about anything at all?'),
                tags$a(href = "mailto:kykimeng@gmail.com?
                       body=''
                       &subject='TBI App Feedback'", "Email Us!"),
                br(),
                # p('Please email', strong("Kim Ky (KimEng.Ky@metrotransit.org)"), 'and she will get back to you within a week.'),
                # br(),
                p('If you are emailing about a problem with the app, please include a screenshot of the error message/the page you are on,
                  and/or explain how you got to the error.')
                # )
          ))
        )
      )
    )
  )
)
