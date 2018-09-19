suppressMessages(library(htmlwidgets))
suppressMessages(library(geosphere))
suppressMessages(library(data.table))
suppressMessages(library(shiny))
suppressMessages(library(leaflet.extras))
suppressMessages(library(leaflet))
suppressMessages(library(plotly))
suppressMessages(library(sendmailR))
suppressMessages(library(sp))
suppressMessages(library(maptools))
suppressMessages(library(parsetR))
suppressMessages(library(shinyjs))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyWidgets))
suppressMessages(library(webshot))
options(warn = -1)

tbi <- readRDS('Data/tbi.RDS')
tbi[, TIME_PERIOD := factor(TIME_PERIOD, levels = c("AM Peak", "Midday", "PM Peak", "Evening"))]
l <- readRDS("Data/spatial_lines.RDS")
tbi_OD <- readRDS("Data/tbi_OD.RDS")
od_lines <- readRDS("Data/od_lines.RDS")
ba_lines <- readRDS("Data/ba_lines.RDS")
routes <- readRDS("Data/routes_sp.RDS")
pnr <- readRDS('Data/pnr.RDS')
tracts <- readRDS("Data/tracts.RDS")
blocks <- readRDS("Data/blocks.RDS")
taz <- readRDS("Data/taz.RDS")
flow_data <- readRDS("Data/flow_data.RDS")
tracts_fm <- readRDS("Data/tract_fm.RDS")
blocks_fm <- readRDS("Data/block_fm.RDS")
taz_fm <- readRDS("Data/taz_fm.RDS")
on_off <- readRDS("Data/on_off.RDS")
blockgroups <- readRDS("Data/blockgroups.RDS")
# tract_to_tract_personal <- readRDS("Data/tract_to_tract_personal.RDS")
sl_tract <- readRDS("Data/sl_tract.RDS")
sl_taz <- readRDS("Data/sl_taz.RDS")

varLabels <- c("Route Type Surveyed" = "RouteType", 
               "Number of Prior Transfers" = "TRANSFERS_FROM",
               "Origin Place Type" = "ORIGIN_PLACE_TYPE",
               "Access Mode" = "ACCESS_MODE", 
               "Number of Transfer to" = "TRANSFERS_TO", 
               "Destination Place Type" = "DESTIN_PLACE_TYPE",
               "Egress Mode" = "EGRESS_MODE",
               "Time Period" = "TIME_PERIOD", 
               "Trip in Opposite Direction" = "TRIP_IN_OPPOSITE_DIR",
               "Payment Method" = "PAYMENT_METHOD",
               "Fare Type" = "FARE_TYPE",
               "Trip Purpose" = "trip_purpose",
               "Is Visitor" = "VISITOR",
               "Household Vehicle Count" = "COUNT_VH_HH", 
               "Can Use Vehicle for Trip" = "CAN_USE_VEH_TRIP",
               "Number of Members in Household" = "COUNT_MEMBER_HH",
               "Number Employed in Household" = "COUNT_EMPLOYED_HH",
               "Employment Status" = "STATUS_EMPLOYMENT",
               "Student Status" = "STUDENT_STATUS", 
               "Fare Subsidy" = "FARE_SUBSIDY", 
               "Has Driver's License" = "HAS_DRIVE_LICENSE", 
               "Age Group" = "AGE", 
               "Race/Ethnicity" = "race_ethnicity", 
               "Income" = "INCOME", 
               "Speak Language Other Than English at Home" = "HOME_LANG_OTHER",
               "English Ability" = "ENGLISH_ABILITY",
               "Have Disabilities" = "DISABILITY",
               "Gender" = "GENDER")

init <- copy(tbi)

mt_blue <- "#0053A0"
light_blue <- "#92c5de"
mt_red <- "#ED1B2E"
light_red <- "#f4a582"
mt_yellow <- "#FFD200"
mt_green <- "#008144"
mt_orange <- "#F68A1E"
light_gray <- "#f7f7f7"

# FUNCTIONS ---------------------------------------------------------------
plot_stats <- function(newdata, x = 'None', y = 'None', f_v = 'None', freq = FALSE) {
  # inputs:
  ## newdata (data.table): output of dataInput() reactive in server.R
  ## x (char): name of variable on x-axis
  ## y (char): name of variable on y-axis
  ## f_v (char): name of variable to facet by
  ## freq (boolean): TRUE if plot percentages; plot counts otherwise
  # output (ggplot): bar plot
  if (x == y) y <- 'None'
  if (x == f_v) f_v <- 'None'
  if (y == f_v) f_v <- 'None'
  
  x_class <- tail(tryCatch(class(newdata[, get(x)]), error = function(v) {return('None')}), 1)
  y_class <- tail(tryCatch(class(newdata[, get(y)]), error = function(v) {return('None')}), 1)
  f_class <- tail(tryCatch(class(newdata[, get(f_v)]), error = function(v) {return('None')}), 1)
  barText <- 6
  
  if (x != 'None' & y !='None') {
    if (f_v == 'None') {
      newdata[, `:=` (percent = N/sum(N)), by = x][, pct := scales::percent(percent)]
    } else {
      newdata[, percent := N/sum(N), by = c(x, f_v)][, pct := scales::percent(percent)]
    }
  } else {
    newdata[, percent := N/sum(N)][, pct := scales::percent(percent)]
  }
  
  ## Make plot depending on x and y ####
  # titles 
  # ylab ----
  ylab <- ifelse(freq, "Percent", "N")
  # x lab ----
  xlab <- switch(y_class,
                 None = switch(x_class,
                               None = "Number of Observations",
                               factor = switch(x, 
                                               "RouteType" = "Route Type Surveyed", 
                                               "TRANSFERS_FROM" = "Number of Prior Transfers",
                                               "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                               "ACCESS_MODE" = "Access Mode", 
                                               "TRANSFERS_TO" = "Number of Transfer to", 
                                               "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                               "EGRESS_MODE" = "Egress Mode",
                                               "TIME_PERIOD" = "Time Period", 
                                               "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                               "PAYMENT_METHOD" = "Payment Method",
                                               "FARE_TYPE" = "Fare Type",
                                               "trip_purpose" = "Trip Purpose",
                                               "VISITOR" = "Is Visitor",
                                               "COUNT_VH_HH" = "Household Vehicle Count", 
                                               "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                               "COUNT_MEMBER_HH" = "Number of Members in Household",
                                               "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                               "STATUS_EMPLOYMENT" = "Employment Status",
                                               "STUDENT_STATUS" = "Student Status", 
                                               "FARE_SUBSIDY" = "Fare Subsidy", 
                                               "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                               "AGE" = "Age Group", 
                                               "race_ethnicity" = "Race/Ethnicity", 
                                               "INCOME" = "Income", 
                                               "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                               "ENGLISH_ABILITY" = "English Ability",
                                               "DISABILITY" = "Have Disabilities",
                                               "GENDER" = "Gender")
                 ),
                 factor = switch(x_class,
                                 None = switch(y, 
                                               "RouteType" = "Route Type Surveyed", 
                                               "TRANSFERS_FROM" = "Number of Prior Transfers",
                                               "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                               "ACCESS_MODE" = "Access Mode", 
                                               "TRANSFERS_TO" = "Number of Transfer to", 
                                               "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                               "EGRESS_MODE" = "Egress Mode",
                                               "TIME_PERIOD" = "Time Period", 
                                               "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                               "PAYMENT_METHOD" = "Payment Method",
                                               "FARE_TYPE" = "Fare Type",
                                               "trip_purpose" = "Trip Purpose",
                                               "VISITOR" = "Is Visitor",
                                               "COUNT_VH_HH" = "Household Vehicle Count", 
                                               "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                               "COUNT_MEMBER_HH" = "Number of Members in Household",
                                               "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                               "STATUS_EMPLOYMENT" = "Employment Status",
                                               "STUDENT_STATUS" = "Student Status", 
                                               "FARE_SUBSIDY" = "Fare Subsidy", 
                                               "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                               "AGE" = "Age Group", 
                                               "race_ethnicity" = "Race/Ethnicity", 
                                               "INCOME" = "Income", 
                                               "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                               "ENGLISH_ABILITY" = "English Ability",
                                               "DISABILITY" = "Have Disabilities",
                                               "GENDER" = "Gender"),
                                 factor = switch(x, 
                                                 "RouteType" = "Route Type Surveyed", 
                                                 "TRANSFERS_FROM" = "Number of Prior Transfers",
                                                 "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                                 "ACCESS_MODE" = "Access Mode", 
                                                 "TRANSFERS_TO" = "Number of Transfer to", 
                                                 "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                                 "EGRESS_MODE" = "Egress Mode",
                                                 "TIME_PERIOD" = "Time Period", 
                                                 "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                                 "PAYMENT_METHOD" = "Payment Method",
                                                 "FARE_TYPE" = "Fare Type",
                                                 "trip_purpose" = "Trip Purpose",
                                                 "VISITOR" = "Is Visitor",
                                                 "COUNT_VH_HH" = "Household Vehicle Count", 
                                                 "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                                 "COUNT_MEMBER_HH" = "Number of Members in Household",
                                                 "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                                 "STATUS_EMPLOYMENT" = "Employment Status",
                                                 "STUDENT_STATUS" = "Student Status", 
                                                 "FARE_SUBSIDY" = "Fare Subsidy", 
                                                 "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                                 "AGE" = "Age Group", 
                                                 "race_ethnicity" = "Race/Ethnicity", 
                                                 "INCOME" = "Income", 
                                                 "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                                 "ENGLISH_ABILITY" = "English Ability",
                                                 "DISABILITY" = "Have Disabilities",
                                                 "GENDER" = "Gender")
                 ))
  # legend title ----
  legendtitle <- switch(y_class,
                        None = switch(x_class,
                                      None = "",
                                      factor = ""
                        ),
                        factor = switch(x_class,
                                        None = "",
                                        factor = switch(y, 
                                                        "RouteType" = "Route Type Surveyed", 
                                                        "TRANSFERS_FROM" = "Number of Prior Transfers",
                                                        "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                                        "ACCESS_MODE" = "Access Mode", 
                                                        "TRANSFERS_TO" = "Number of Transfer to", 
                                                        "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                                        "EGRESS_MODE" = "Egress Mode",
                                                        "TIME_PERIOD" = "Time Period", 
                                                        "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                                        "PAYMENT_METHOD" = "Payment Method",
                                                        "FARE_TYPE" = "Fare Type",
                                                        "trip_purpose" = "Trip Purpose",
                                                        "VISITOR" = "Is Visitor",
                                                        "COUNT_VH_HH" = "Household Vehicle Count", 
                                                        "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                                        "COUNT_MEMBER_HH" = "Number of Members in Household",
                                                        "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                                        "STATUS_EMPLOYMENT" = "Employment Status",
                                                        "STUDENT_STATUS" = "Student Status", 
                                                        "FARE_SUBSIDY" = "Fare Subsidy", 
                                                        "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                                        "AGE" = "Age Group", 
                                                        "race_ethnicity" = "Race/Ethnicity", 
                                                        "INCOME" = "Income", 
                                                        "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                                        "ENGLISH_ABILITY" = "English Ability",
                                                        "DISABILITY" = "Have Disabilities",
                                                        "GENDER" = "Gender")
                        ))
  # main title ----
  maintitle <- switch(y_class,
                      None = switch(x_class,
                                    None = "",
                                    factor = xlab
                      ),
                      factor = switch(x_class,
                                      None = xlab,
                                      factor = if (freq) {
                                        switch(f_class, 
                                               None = paste(xlab, "by", legendtitle), 
                                               factor = paste(xlab, "by", legendtitle, "faceted by", switch(f_v, 
                                                                                                            "RouteType" = "Route Type Surveyed", 
                                                                                                            "TRANSFERS_FROM" = "Number of Prior Transfers",
                                                                                                            "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                                                                                            "ACCESS_MODE" = "Access Mode", 
                                                                                                            "TRANSFERS_TO" = "Number of Transfer to", 
                                                                                                            "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                                                                                            "EGRESS_MODE" = "Egress Mode",
                                                                                                            "TIME_PERIOD" = "Time Period", 
                                                                                                            "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                                                                                            "PAYMENT_METHOD" = "Payment Method",
                                                                                                            "FARE_TYPE" = "Fare Type",
                                                                                                            "trip_purpose" = "Trip Purpose",
                                                                                                            "VISITOR" = "Is Visitor",
                                                                                                            "COUNT_VH_HH" = "Household Vehicle Count", 
                                                                                                            "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                                                                                            "COUNT_MEMBER_HH" = "Number of Members in Household",
                                                                                                            "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                                                                                            "STATUS_EMPLOYMENT" = "Employment Status",
                                                                                                            "STUDENT_STATUS" = "Student Status", 
                                                                                                            "FARE_SUBSIDY" = "Fare Subsidy", 
                                                                                                            "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                                                                                            "AGE" = "Age Group", 
                                                                                                            "race_ethnicity" = "Race/Ethnicity", 
                                                                                                            "INCOME" = "Income", 
                                                                                                            "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                                                                                            "ENGLISH_ABILITY" = "English Ability",
                                                                                                            "DISABILITY" = "Have Disabilities",
                                                                                                            "GENDER" = "Gender")))
                                      } else {
                                        switch(f_class, 
                                               None = paste(xlab, "by", legendtitle), 
                                               factor = paste(xlab, "by", legendtitle, "faceted by", switch(f_v, 
                                                                                                            "RouteType" = "Route Type Surveyed", 
                                                                                                            "TRANSFERS_FROM" = "Number of Prior Transfers",
                                                                                                            "ORIGIN_PLACE_TYPE" = "Origin Place Type",
                                                                                                            "ACCESS_MODE" = "Access Mode", 
                                                                                                            "TRANSFERS_TO" = "Number of Transfer to", 
                                                                                                            "DESTIN_PLACE_TYPE" = "Destination Place Type",
                                                                                                            "EGRESS_MODE" = "Egress Mode",
                                                                                                            "TIME_PERIOD" = "Time Period", 
                                                                                                            "TRIP_IN_OPPOSITE_DIR" = "Trip in Opposite Direction",
                                                                                                            "PAYMENT_METHOD" = "Payment Method",
                                                                                                            "FARE_TYPE" = "Fare Type",
                                                                                                            "trip_purpose" = "Trip Purpose",
                                                                                                            "VISITOR" = "Is Visitor",
                                                                                                            "COUNT_VH_HH" = "Household Vehicle Count", 
                                                                                                            "CAN_USE_VEH_TRIP" = "Can Use Vehicle for Trip",
                                                                                                            "COUNT_MEMBER_HH" = "Number of Members in Household",
                                                                                                            "COUNT_EMPLOYED_HH" = "Number Employed in Household",
                                                                                                            "STATUS_EMPLOYMENT" = "Employment Status",
                                                                                                            "STUDENT_STATUS" = "Student Status", 
                                                                                                            "FARE_SUBSIDY" = "Fare Subsidy", 
                                                                                                            "HAS_DRIVE_LICENSE" = "Has Driver's License", 
                                                                                                            "AGE" = "Age Group", 
                                                                                                            "race_ethnicity" = "Race/Ethnicity", 
                                                                                                            "INCOME" = "Income", 
                                                                                                            "HOME_LANG_OTHER" = "Speak Language Other Than English at Home",
                                                                                                            "ENGLISH_ABILITY" = "English Ability",
                                                                                                            "DISABILITY" = "Have Disabilities",
                                                                                                            "GENDER" = "Gender")))
                                      }
                      ))
  # plot ----
  p <- switch(y_class,
              None = switch(x_class,
                            None = ggplot(newdata, aes(x = factor(1L), y = N, text = sprintf("N: %s<br>N obs: %s", prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ',')))) +
                              geom_bar(width = I(0.5), fill = mt_blue, stat = 'identity') + 
                              labs(y = ylab, x = xlab, title = maintitle),
                            factor = ggplot(newdata, aes(x = get(x), y = N, 
                                                         text = sprintf(paste0(names(varLabels[varLabels == x]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                        get(x), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                              geom_bar(fill = mt_blue, stat = 'identity') + 
                              labs(y = ylab, x = xlab, title = maintitle)
              ),
              factor = switch(x_class,
                              None = ggplot(newdata, aes(x = get(y), y = N, text = sprintf(paste0(names(varLabels[varLabels == y]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                                           get(y), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                                geom_bar(fill = mt_blue, stat = 'identity') + 
                                labs(y = ylab, x = xlab, title = maintitle),
                              factor = if (freq) {
                                switch(f_class, 
                                       None = ggplot(newdata, aes(y = N, x = get(x), fill = get(y), text = sprintf(paste0(names(varLabels[varLabels == y]), ": %s<br>", 
                                                                                                                          names(varLabels[varLabels == x]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                                                                   get(y), get(x), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                                         geom_bar(position = position_fill(), stat = 'identity', 
                                                  colour = I('grey18'), size = .1) +
                                         scale_fill_manual(values = get_colors(length(levels(newdata[[y]])), y), name = y) +
                                         scale_y_continuous(labels = scales::percent)+ 
                                         labs(y = ylab, x = xlab, title = maintitle) +
                                         guides(fill=guide_legend(title=legendtitle)), 
                                       factor = ggplot(newdata, aes(y = N, x = get(x), fill = get(y), text = sprintf(paste0(names(varLabels[varLabels == y]), ": %s<br>", 
                                                                                                                            names(varLabels[varLabels == x]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                                                                     get(y), get(x), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                                         geom_bar(position = position_fill(), stat = 'identity', 
                                                  colour = I('grey18'), size = .1) +
                                         scale_fill_manual(values = get_colors(length(levels(newdata[[y]])), y), name = y) +
                                         facet_grid(~get(f_v)) +
                                         scale_y_continuous(labels = scales::percent) + 
                                         labs(y = ylab, x = xlab, title = maintitle) +
                                         guides(fill=guide_legend(title=legendtitle)))
                              } else {
                                switch(f_class, 
                                       None = ggplot(newdata, aes(y = N, x = get(x), fill = get(y), text = sprintf(paste0(names(varLabels[varLabels == y]), ": %s<br>", 
                                                                                                                          names(varLabels[varLabels == x]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                                                                   get(y), get(x), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                                         geom_bar(position = position_dodge(width = 0.9), stat = 'identity', 
                                                  colour = I('grey18'), size = .1) +
                                         scale_fill_manual(values = get_colors(length(levels(newdata[[y]])), y))+ 
                                         labs(y = ylab, x = xlab, title = maintitle) +
                                         guides(fill=guide_legend(title=legendtitle)), 
                                       factor = ggplot(newdata, aes(y = N, x = get(x), fill = get(y), text = sprintf(paste0(names(varLabels[varLabels == y]), ": %s<br>", 
                                                                                                                            names(varLabels[varLabels == x]), ": %s<br>N: %s<br>N obs: %s <br>Percent: %s"), 
                                                                                                                     get(y), get(x), prettyNum(round(N, 0), big.mark = ','), prettyNum(obs, big.mark = ','), pct))) +
                                         geom_bar(position = position_dodge(width = 0.9), stat = 'identity', 
                                                  colour = I('grey18'), size = .1) +
                                         scale_fill_manual(values = get_colors(length(levels(newdata[[y]])), y)) +
                                         facet_grid(~get(f_v))+ 
                                         labs(y = ylab, x = xlab, title = maintitle) +
                                         guides(fill=guide_legend(title=legendtitle)))
                              }
              ))
  # adjust text size
  p <- p + theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10), 
          strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
}

get_colors <- function(n, x) {
  # if (n > 5) {
  #   if (x %in% c('mode', "race", "hh_income", "age_group", "tod", "payment", "dow", 
  #                "gender", "transit_for_special_events", "days_taken_transit", "purpose",
  #                "get_to_transit", "after_trip", "if_trip_not_exist", "number_automobiles",
  #                "n_household", "reasons", "use_transit", "influenced_by", "info_sources")) {
  #     colors <- c(mt_blue, mt_green, light_gray, mt_orange, mt_yellow)
  #   } else {
  #     colors <- c(mt_blue, mt_green, light_gray, mt_orange, mt_red)
  #   }
  # } else {
  #   if (x %in% c('mode', "race", "hh_income", "age_group", "tod", "payment", "dow", 
  #                "gender", "transit_for_special_events", "days_taken_transit", "purpose",
  #                "get_to_transit", "after_trip", "if_trip_not_exist", "number_automobiles",
  #                "n_household", "reasons", "use_transit", "influenced_by", "info_sources", 'have_disabilities',
  #                'fear', 'harassed')) {
  #     colors <- c(mt_blue, light_gray, mt_yellow)
  #   } else {
  #     colors <- c(mt_blue, light_gray, mt_red)
  #   }
  # }
  colors <- c(mt_blue, mt_green, light_gray, mt_orange, mt_yellow)
  col_ramp <- colorRampPalette(colors)
  return(col_ramp(n))
}

# map ---------
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    return(sp_lines)
  }
}

filter_data <- function(temp, r, rt, tp) {
  if (!is.null(r)) {
    temp <- temp[route %in% r]
  }
  if (!is.null(rt)) {
    temp <- temp[RouteType %in% rt]
  } 
  if (!is.null(tp)) {
    temp <- temp[TimePeriod %in% tp]
  }
  return(temp)
}

get_pairs_cluster <- function(od, var, fm, r, rt, tp, curr_poly, linked_odb, ignoreN, exc_sel = FALSE) {
  # return clustered o-d pairs after applying filters 
  # inputs:
  ## od (char): orig, dest, both
  ## var (char): block, tract, taz
  ## fm (data.table): with columns: ID, unlinked_wgts, LINKED_MULTIPLIER, dest_block, dest_lat, dest_lon, orig_block, orig_lat, orig_lon, 
  ##                                route, RouteType, TimePeriod
  ## r (num): route to filter
  ## rt (char): route type to filter
  ## tp (char): time period to filter
  ## curr_poly (vector): identification for 'var' that is currently selected
  ## linked_odb (boolean): TRUE if linked expansion factor is used; FALSE otherwise
  ## ignoreN (num): minimum N; only use when cluster is TRUE
  if (od == 'orig') {
    temp <- copy(fm)
    setnames(temp, c(paste0('orig_', var), paste0('dest_', var)), c("orig", "dest"))
  } else if (od == 'dest') {
    temp <- copy(fm)
    setnames(temp, c(paste0('orig_', var), paste0('dest_', var)), c("dest", "orig"))
  } else if (od == 'both') {
    temp <- copy(fm)
    setnames(temp, c(paste0('orig_', var), paste0('dest_', var)), c("orig", "dest"))
    temp2 <- copy(fm)
    setnames(temp2, c(paste0('orig_', var), paste0('dest_', var)), c("dest", "orig"))
    temp <- rbindlist(list(temp, temp2))
  }
  temp <- filter_data(temp = temp, r = r, rt = rt, tp = tp)
  if (exc_sel) {
    temp <- temp[orig %in% curr_poly & !(dest %in% curr_poly)]
  } else {
    temp <- temp[orig %in% curr_poly]
  }
  if (linked_odb) {
    temp <- na.omit(temp[, .(N = sum(unlinked_wgts * LINKED_MULTIPLIER)), 
                         by = .(orig, orig_lat, orig_lon, dest, dest_lat, dest_lon)][N >= ignoreN])
  } else {
    temp <- na.omit(temp[, .(N = sum(unlinked_wgts)), 
                         by = .(orig, orig_lat, orig_lon, dest, dest_lat, dest_lon)][N >= ignoreN])
  }
  temp[, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
  return(temp)
}

update_leaflet_heat <- function(temp, sp, mapId, session, curr_poly) {
  # update leaflet when grouping and showing heat map
  # inputs:
  ## temp (data.table): output of get_pairs_cluster()
  ## mapId (char): leaflet mapId to update
  ## session: shiny session
  ## curr_poly (vector): identification for 'var' that is currently selected
  temp <- temp[, .(N = sum(N)), by = .(dest)][, colN := ifelse(N >= quantile(N, .95), quantile(N, .95), N)]
  pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(temp$colN))
  dest <- subset(sp::merge(sp, temp, by.x = "GEOID10", by.y = "dest"), !is.na(N))
  leafletProxy(mapId, session) %>% clearShapes() %>% clearMarkers() %>%
    addPolygons(data = subset(sp, GEOID10 %in% curr_poly), weight = 3, opacity = 1, fillOpacity = .5, color = "black") %>%
    addPolygons(data = dest, weight = 1, opacity = 1, fillOpacity = .7, color = ~pal(colN), 
                label = ~paste(round(N, 0)))
}

get_flows <- function(temp) {
  # inputs:
  ## temp (data.table): with 3rd column = lat, 2nd = lon, 6th = lat, 5th = lon
  # output (SpatialLinesDataFrame): flow lines between each pair of origin-destination
  flows <- gcIntermediate(temp[,3:2], temp[,6:5], sp = TRUE, addStartEnd = TRUE)
  flows$counts <- round(temp$N, 0)
  flows$origins <- temp$orig
  flows$destinations <- temp$dest
  flows$colN <- temp$colN
  return(flows)
}

update_leaflet_flows <- function(temp, mapId, session, sp) {
  # inputs:
  ## temp (SpatialLinesDataFrame): output of get_flows()
  ## mapId (char): map id to update
  ## session: current session
  ## sp (SpatialPolygonsDataFrame)
  # output: add flows to leaflet map in shiny app with id = mapId
  flows <- get_flows(temp = temp)
  hover <- paste0(flows$origins, " to ", 
                  flows$destinations, ': ', 
                  as.character(flows$counts))
  pal <- colorNumeric(c("#a6bddb", "#0570b0", "#023858"), domain = range(temp$colN))
  leafletProxy(mapId, session) %>% 
    addPolygons(data = subset(sp, GEOID10 %in% temp$dest), weight = 1, opacity = 1, fillOpacity = .2, color = "#008144") %>%
    addPolygons(data = subset(sp, GEOID10 %in% temp$orig), weight = 1, opacity = 1, fillOpacity = .2, color = "#ED1B2E") %>%
    addPolylines(data = flows, weight = ~log(counts, 2)+1, label = hover, 
                 group = ~destinations, color = ~pal(colN), opacity = 1)
}

get_tbi_od <- function(od, temp, spp, tbi) {
  # inputs:
  ## od (char): orig, dest, or both
  ## temp (data.table): output of filter_data()
  ## spp (SpatialPolygonsDataFrame): selected region to get orig/dest from
  ## tbi (data.table): cleaned tbi data table
  # output (list): allLines - all correspodning o-d pairs; endPoints - end point locations of each o-d pair
  if (od == 'orig') {
    # find origins from the tract
    spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                   data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
    proj4string(spOD) <- proj4string(spp)
    od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, spp))))
    ids <- od$ID
    allLines <- do.call('rbind', od_lines[ids])
    endPoints <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), TRANSFERS_FROM_FIRST_CODE, 
                                    TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                    TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                    TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route)]
  } else if (od == 'dest') {
    spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                         .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                    data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
    proj4string(spOD2) <- proj4string(spp)
    od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, spp))))
    ids2 <- od2$ID
    allLines <- do.call('rbind', od_lines[ids2])
    endPoints <- tbi[ID %in% ids2, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), TRANSFERS_FROM_FIRST_CODE, 
                                     TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                                     TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                                     TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route)]
  } else {
    spOD <- SpatialPointsDataFrame(na.omit(temp[, .(ORIGIN_LON, ORIGIN_LAT)]),
                                   data = temp[!is.na(ORIGIN_LON) & !is.na(ORIGIN_LAT), .(ID)])
    proj4string(spOD) <- proj4string(spp)
    od <- na.omit(data.table(cbind(spOD@data, sp::over(spOD, spp))))
    ids <- od$ID
    spOD2 <- SpatialPointsDataFrame(temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON),
                                         .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                    data = temp[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
    proj4string(spOD2) <- proj4string(spp)
    od2 <- na.omit(data.table(cbind(spOD2@data, sp::over(spOD2, spp))))
    ids2 <- od2$ID
    allLines <- do.call('rbind', od_lines[c(ids, ids2)])
    ep1 <- tbi[ID %in% ids, .(ID, lat = as.numeric(DESTINATION_LAT), lon = as.numeric(DESTINATION_LON), TRANSFERS_FROM_FIRST_CODE, 
                              TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                              TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                              TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route)]
    ep2 <- tbi[ID %in% ids2, .(ID, lat = as.numeric(ORIGIN_LAT), lon = as.numeric(ORIGIN_LON), TRANSFERS_FROM_FIRST_CODE, 
                               TRANSFERS_FROM_SECOND_CODE, TRANSFERS_FROM_THIRD_CODE, TRANSFERS_FROM_FOURTH_CODE, 
                               TRANSFERS_TO_FIRST_CODE, TRANSFERS_TO_SECOND_CODE, TRANSFERS_TO_THIRD_CODE, 
                               TRANSFERS_TO_FOURTH_CODE, TIME_PERIOD, route)]
    endPoints <- rbind(ep1, ep2)
  }
  return(list(allLines, endPoints))
}

update_leaflet_ind <- function(od, temp, spp, tbi, mapId, session) {
  # inputs:
  ## od (char): orig, dest, or both
  ## temp (data.table): output of filter_data()
  ## spp (SpatialPolygonsDataFrame): selected region to get orig/dest from
  ## tbi (data.table): cleaned tbi data table
  ## mapId (char): map id to update
  ## session: current session
  # output: add corresponding o-d pairs to leaflet map in shiny app
  tmp <- get_tbi_od(od = od, temp = temp, spp = spp, tbi = tbi)
  allLines <- tmp[[1]]
  endPoints <- tmp[[2]]
  if (length(allLines) == 0) return()
  leafletProxy(mapId, session) %>% 
    addPolygons(data = spp, color = "#ED1B2E", opacity = 0.5, weight = 1) %>%
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

get_od <- function(fm, cluster, od, var, r = NULL, rt = NULL, tp = NULL, linked_odb, ignoreN = 0, heat, 
                   curr_poly, mapId, sp, tbi, session, exc_sel = FALSE) {
  # update leaflet od map
  # inputs:
  ## cluster (boolean): TRUE if origins/destinations are grouped; FALSE otherwise
  ## heat (boolean): TRUE if origins/destinations should be plotted as heat map; only use when cluster is TRUE
  ## od (char): orig, dest, both
  ## var (char): block, tract, taz
  ## fm (data.table): with columns: ID, unlinked_wgts, LINKED_MULTIPLIER, dest_block, dest_lat, dest_lon, orig_block, orig_lat, orig_lon, 
  ##                                route, RouteType, TimePeriod
  ## r (num): route to filter
  ## rt (char): route type to filter
  ## tp (char): time period to filter
  ## curr_poly (vector): identification for 'var' that is currently selected
  ## linked_odb (boolean): TRUE if linked expansion factor is used; FALSE otherwise
  ## ignoreN (num): minimum N; only use when cluster is TRUE
  ## mapId (char): leaflet mapId to update
  ## sp (SpatialPolygonsDataFrame): blocks, tracts, taz
  ## tbi (data.table): clean tbi data table
  ## session: shiny session
  # output: update the leaflet map 
  if (cluster) {
    temp <- get_pairs_cluster(od = od, var = var, fm = fm, r = r, rt = rt, tp = tp, 
                              curr_poly = curr_poly, linked_odb = linked_odb, ignoreN = ignoreN,
                              exc_sel = exc_sel)
    if (nrow(temp) == 0) return()
    if (heat) {
      update_leaflet_heat(temp = temp, sp = sp, mapId = mapId, session = session, curr_poly = curr_poly)
    } else {
      update_leaflet_flows(temp = temp, mapId = mapId, session = session, sp = sp)
    }
  } else {
    spp <- subset(sp, GEOID10 %in% curr_poly)
    temp <- copy(tbi)
    setnames(temp, "TIME_PERIOD", "TimePeriod")
    temp <- filter_data(temp = temp, r = r, rt = rt, tp = tp)
    update_leaflet_ind(od = od, temp = temp, spp = spp, tbi = tbi, mapId = mapId, session = session)
  }
}

filter_stl_data <- function(tp, sl, dt, od) {
  # inputs:
  ## tp (char): vector of time periods 
  ## sl (data.table): StreetLight o-d data; with at least columns: Destination Zone ID, dest_lat, dest_lon, Origin Zone ID, 
  ##                  orig_lat, orig_lon, Day Type, Day Part, O-D Traffic (StL Index)
  ## dt (char): day type
  ## od (char): orig, dest, or both
  # output (data.table): filtered StreetLight data table
  if ("0: All Day (12am-12am)" %in% tp) {
    temp <- sl[`Day Part` == "0: All Day (12am-12am)" & `Day Type` == dt, .(`Destination Zone ID`, dest_lat, dest_lon, 
                                                                            `Origin Zone ID`, orig_lat, orig_lon, 
                                                                            `Day Type`, `Day Part`, `O-D Traffic (StL Index)`)]
  } else {
    temp <- sl[`Day Part` %in% tp & `Day Type` == dt, .(`Destination Zone ID`, dest_lat, dest_lon, 
                                                        `Origin Zone ID`, orig_lat, orig_lon, 
                                                        `Day Type`, `Day Part`, `O-D Traffic (StL Index)`)]
  }
  if (od == 'dest') {
    temp <- temp[, .(`Destination Zone ID` = `Origin Zone ID`, dest_lat = orig_lat, dest_lon = orig_lon, 
                     `Origin Zone ID` = `Destination Zone ID`, orig_lat = dest_lat, orig_lon = dest_lon,
                     `Day Type`, `Day Part`, `O-D Traffic (StL Index)`)]
  } else if (od == 'both') {
    temp2 <- temp[, .(`Destination Zone ID` = `Origin Zone ID`, dest_lat = orig_lat, dest_lon = orig_lon, 
                      `Origin Zone ID` = `Destination Zone ID`, orig_lat = dest_lat, orig_lon = dest_lon,
                      `Day Type`, `Day Part`, `O-D Traffic (StL Index)`)]
    temp <- rbindlist(list(temp, temp2))
  }
  return(temp)
}

update_leaflet_heat_stl <- function(exc_sel, temp, curr_poly, sp, mapId, session) {
  # inputs: 
  ## exc_sel (boolean): TRUE if excluding curr_poly as destination; FALSE otherwise
  ## temp (data.table): StreetLight o-d data table
  ## curr_poly (SpatialPolygons): selected area (tract or TAZ)
  ## sp (SpatialPolygonsDataFrame): all areas (tracts or TAZs)
  ## mapId (char): leaflet map in shiny app to update
  ## session: current shiny session
  # output: add heat map to leaflet map using StreetLight Data
  if (exc_sel) {
    temp <- temp[`Origin Zone ID` %in% curr_poly & !(`Destination Zone ID` %in% curr_poly)]
  } else {
    temp <- temp[`Origin Zone ID` %in% curr_poly]
  }
  temp2 <- temp[, .(N = sum(`O-D Traffic (StL Index)`, na.rm = TRUE)), by = .(`Destination Zone ID`)]
  temp2 <- subset(sp::merge(sp, temp2, by.x = "GEOID10", by.y = "Destination Zone ID"), !is.na(N))
  pal <- colorNumeric(c("#c6dbef", "#6baed6", "#2171b5", "#08306b"), domain = range(temp2$N))
  leafletProxy(mapId, session) %>% clearShapes() %>% clearMarkers() %>%
    addPolygons(data = subset(sp, GEOID10 %in% curr_poly), weight = 3, fillOpacity = .5, opacity = 1, color = 'black') %>% 
    addPolygons(data = temp2, color = ~pal(N), weight = 1, fillOpacity = .7, opacity = 1, label = ~paste("Estimated: ", N))
}

get_od_sl <- function(tp, sl, dt, od, heat, curr_poly, exc_sel, sp, mapId, session) {
  # update leaflet od map with streetlight data
  # inputs:
  ## tp (char): 0: All Day (12am-12am), 1: Owl (12am-4am), 2: Early (4am-6am), 3: AM Peak (6am-9am), 
  ##            4: Midday (9am-3pm), 5: PM Peak (3pm-7pm), 6: Evening (7pm-12am)
  ## sl (data.table): with columns: Destination Zone ID, dest_lat, dest_lon, Origin Zone ID, orig_lat, orig_lon, 
  ##                                Day Type, Day Part, O-D Traffic (StL Index)
  ## dt (char): 1: Average Weekday (M-F), 2: Average Weekend Day (Sa-Su)
  ## heat (boolean): TRUE if origins/destinations should be plotted as heat map; only use when cluster is TRUE
  ## curr_poly (vector): identification for zones that are currently selected
  ## exc_sel (boolean): TRUE if excluding origin zones 
  ## sp (SpatialPolygonsDataFrame): zone set that o-d is based on
  ## mapId (char): leaflet map to be updated
  ## session: shiny session
  # output: update leaflet map
  temp <- filter_stl_data(tp = tp, sl = sl, dt = dt, od = od)
  if (heat) {
    update_leaflet_heat_stl(exc_sel = exc_sel, temp = temp, curr_poly = curr_poly, sp = sp, mapId = mapId, session = session)
  } else {
    temp <- temp[`Origin Zone ID` %in% curr_poly][, colN := `O-D Traffic (StL Index)`]
    setnames(temp, c('O-D Traffic (StL Index)', 'Origin Zone ID', 'Destination Zone ID'), c('N', 'orig', 'dest'))
    update_leaflet_flows(temp = temp, mapId = mapId, session = session, sp = sp)
  }
}
