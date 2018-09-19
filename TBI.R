library(data.table)
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
library(zipcode)
library(rgdal)
library(ggmap)
library(stplanr)
library(sp)
library(leaflet)
library(maptools)
library(RODBC)
# library(MetroTransitr)

# clean tbi data ----
tbi <- fread("Data/20170715_Minneapolis_OD_Database_FINAL.csv", stringsAsFactors = FALSE)
new_tbi <- fread("Data/OBS16Release0918/OBS16Release0918.csv", stringsAsFactors = FALSE)[, .(ID, unlinked_wgts = `Unlinked Weight-AE`, LINKED_MULTIPLIER)]
tbi <- new_tbi[tbi, on = .(ID)] # merge instead of using the most current tbi file because the new tbi file does not include lat/lon
tbi[, origin := paste(ORIGIN_ADDRESS, ORIGIN_CITY, ORIGIN_STATE, ORIGIN_ZIP)]
tbi[, destination := paste(DESTINATION_ADDRESS, DESTINATION_CITY, DESTINATION_STATE, DESTINATION_ZIP)]
tbi[, home_hotel_address := paste(HOME_OR_HOTEL_ADDR, HOME_OR_HOTEL_ADDR_CITY, HOME_OR_HOTEL_ADDR_STATE, HOME_OR_HOTEL_ADDR_ZIP)]
# create race_ethnicity variable
temp <- tbi[, AMERICAN_INDIAN_ALASKAN_NATIVE:RACE_ETHNICITY_OTHER]
temp[, n := rowSums(temp == 'Yes', na.rm = TRUE)]
temp[n > 1, race_ethnicity := "Mixed"]
temp[n == 0, race_ethnicity := NA]
temp[n == 1 & AMERICAN_INDIAN_ALASKAN_NATIVE == 'Yes', race_ethnicity := "American Indian/Alaskan Native"]
temp[n == 1 & ASIAN == 'Yes', race_ethnicity := "Asian"]
temp[n == 1 & BLACK_AFRICAN_AMERICAN == 'Yes', race_ethnicity := "Black/African American"]
temp[n == 1 & HISPANIC_LATINO == 'Yes', race_ethnicity := "Hispanic/Latino"]
temp[n == 1 & NATIVE_HAWAIIAN_PACIFIC_ISLANDER == 'Yes', race_ethnicity := "Native Hawaiian/Pacific Islander"]
temp[n == 1 & WHITE == 'Yes', race_ethnicity := "White"]
temp[n == 1 & RACE_ETHNICITY_OTHER == 'Yes', race_ethnicity := 'Other']
tbi[, race_ethnicity := temp$race_ethnicity]
tbi[tbi == ""] <- NA

# route type
tbi[, route := gsub("\\ .*", "", ROUTE_SURVEYED)]
routeClass <- fread("Data/RouteClassifications.csv", stringsAsFactors = FALSE)
tbi[, routeTemp := as.numeric(route)]
tbi <- routeClass[, .(Route, RouteType = `2040 TPP RouteType`)][tbi, on = .(Route = routeTemp)]
tbi[route %in% c("ALINE", "BLUE", "GREEN", "RED"), RouteType := route]
tbi[route == 888, RouteType := "Northstar"]
# 3026 observations were not matched with current routes with classifications - figure out what to do here?!

# factorize variables
tbi[, RouteType := factor(RouteType, levels = c("Core Local", "Supporting Local", "Suburban Local", "Commuter and Express", 
                                                "ALINE", "BLUE", "GREEN", "RED", "Northstar"))]
tbi[, TRANSFERS_FROM := factor(TRANSFERS_FROM, levels = c("(0) None", "(1) One", "(2) Two", "(3) Three"))]
tbi[, ORIGIN_PLACE_TYPE := factor(ORIGIN_PLACE_TYPE, levels = c("Your HOME", "Work", "Work Related",
                                                                "College / University (students only)", 
                                                                "School K-12 (students only)", "Airport (passengers only)", 
                                                                "Recreation / Sightseeing / Restaurant", "Doctor / Clinic / Hospital (non-work)",
                                                                "Hotel", "Sporting or Special Event", 
                                                                "Social Visit / Community / Religious / Personal", 
                                                                "Shopping"))]
tbi[, ACCESS_MODE := factor(ACCESS_MODE, levels = c("Walk", "Bike", "Shuttle Bus", "Uber, Lyft, etc.", "Wheelchair, walker, motorized cart",
                                                    "Drove alone and parked", "Drove or rode with others and parked", "Car share",
                                                    "Taxi", "Was dropped off by someone", "Skateboard", "Dial-a-Ride"))]
tbi[, TRANSFERS_TO := factor(TRANSFERS_TO, levels = c("(0) None", "(1) One", "(2) Two", "(3) Three", "(4+) Four or more"))]
tbi[, DESTIN_PLACE_TYPE := factor(DESTIN_PLACE_TYPE, levels = c("Your HOME", "Work", "Work Related",
                                                                "College / University (students only)", 
                                                                "School K-12 (students only)", "Airport (passengers only)", 
                                                                "Recreation / Sightseeing / Restaurant", "Doctor / Clinic / Hospital (non-work)",
                                                                "Hotel", "Sporting or Special Event", 
                                                                "Social Visit / Community / Religious / Personal", 
                                                                "Shopping"))]
tbi[, EGRESS_MODE := factor(EGRESS_MODE, levels = c("Walk", "Bike", "Shuttle Bus", "Uber, Lyft, etc.", "Wheelchair, walker, motorized cart",
                                                    "Got in parked car and drove alone", "Drove or rode with others and parked", "Car share",
                                                    "Taxi", "Was picked up by someone", "Skateboard", "Dial-a-Ride",
                                                    "Scooter / Motorcycle"))]
tbi[, TIME_PERIOD := factor(TIME_PERIOD, levels = c("AM Peak", "Evening", "Midday", "PM Peak"))]
tbi[, TRIP_IN_OPPOSITE_DIR := factor(TRIP_IN_OPPOSITE_DIR, levels = c("Yes", "No"))]
tbi[, PAYMENT_METHOD := factor(PAYMENT_METHOD, levels = c("Go-To STORED VALUE", "U Pass", "Cash", "Metro Pass", "Weekly / Monthly Pass", 
                                                          "College Pass", "Student Pass", "Employee Pass", 
                                                          "Qualified Free Ride Pass (Service Connected Veteran)", 
                                                          "Credit/Debit", "Day Pass", "Free Fare Zone", "Mobile Ticket", 
                                                          "10 Ride", "Token", "Other"))]
tbi[, FARE_TYPE := factor(FARE_TYPE, levels = c("Regular (age 13-64)", "Senior (Over 65)", "Student /Youth (age 6-12)", 
                                                "Limited Mobility Pass"))]
tbi[, VISITOR := factor(VISITOR, levels = c("Yes", "No"))]
tbi[, COUNT_VH_HH := factor(COUNT_VH_HH, levels = c("None (0)", "One (1)", "Two (2)", "Three (3)", "Four (4)", "Five (5)", 
                                                    "Six (6)", "Seven (7)", "Eight (8)", "Nine (9)", "Ten or more (10+)"))]
tbi[, COUNT_MEMBER_HH := factor(COUNT_MEMBER_HH, levels = c("One (1)", "Two (2)", "Three (3)", "Four (4)", "Five (5)", 
                                                            "Six (6)", "Seven (7)", "Eight (8)", "Nine (9)", "Ten or More (10+)"))]
tbi[, COUNT_EMPLOYED_HH := factor(COUNT_EMPLOYED_HH, levels = c("None (0)", "One (1)", "Two (2)", "Three (3)", "Four (4)", "Five (5)", 
                                                                "Six (6)", "Seven (7)", "Eight (8)", "Nine (9)", "Ten or More (10+)"))]
tbi[, STATUS_EMPLOYMENT := factor(STATUS_EMPLOYMENT, levels = c("Employed Full-time", "Employed Part-time", "Retired", 
                                                                "Not currently employed - not seeking work", "Not currently employed - seeking work",
                                                                "Stay at home parent or caregiver"))]
tbi[, STUDENT_STATUS := factor(STUDENT_STATUS, levels = c("Yes - College / University / Community College",
                                                          "Yes - K - 12th grade", "Yes - Vocational / Technical / Trade school",
                                                          "Not a student", "Other"))]
tbi[, FARE_SUBSIDY := factor(FARE_SUBSIDY, levels = c("None of the cost", "Yes (some of the cost)", "Yes  (all of the cost)"))]
tbi[, AGE := factor(AGE, levels = c("Under 12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", 
                                    "85 and Over"))]
tbi[, race_ethnicity := factor(race_ethnicity, levels = c("White", "Black/African American", "Mixed",
                                                          "American Indian/Alaskan Native", "Native Hawaiian/Pacific Islander", 
                                                          "Hispanic/Latino", "Asian"))]
tbi[, INCOME := gsub("\\$", "", INCOME)]
tbi[, INCOME := factor(INCOME, levels = c("Less than 15,000", "15,000 - 24,999", "25,000 - 34,999", "35,000 -  59,999", 
                                          "60,000  - 99,999", "100,000 - 149,999", "150,000 - 199,999", "200,000 or more", "Don’t Know / Refuse"))]
tbi[, ENGLISH_ABILITY := factor(ENGLISH_ABILITY, levels = c("Not at all", "Less than well", "Well", "Very well"))]
tbi[, DISABILITY := factor(DISABILITY, levels = c("Yes", "No", "Don't Know / Refuse"))]
tbi[, GENDER := factor(GENDER, levels = c("Female", "Male"))]
# combine trip purpose variables to one 
purpose_cols <- c("ID", names(tbi)[grepl("TRIP_PURPOSE", names(tbi))])
purposes <- tbi[, (purpose_cols), with=FALSE]
purposes[, n := rowSums(purposes == "Yes", na.rm = TRUE)]
purposes[n > 1, trip_purpose := "Multiple purposes"]
purposes[n == 1 & TRIP_PURPOSE_NO_OTHER_TRIP == "Yes", trip_purpose := "No other trip"]
purposes[n == 1 & `TRIP_PURPOSE_BUY_A_MEAL_/_BEVERAGE` == "Yes", trip_purpose := "Buy a meal or beverage"]
purposes[n == 1 & TRIP_PURPOSE_GO_TO_WORK == "Yes", trip_purpose := "Go to work"]
purposes[n == 1 & `TRIP_PURPOSE_VISIT_FRIEND/RELATIVE_OR_ATTEND_RELIGIOUS/SOC_EVENT` == "Yes", trip_purpose := "Visit friend/relative or attend religious/social event"]
purposes[n == 1 & TRIP_PURPOSE_GO_TO_SCHOOL == "Yes", trip_purpose := "Go to school"]
purposes[n == 1 & TRIP_PURPOSE_GO_SHOPPING == "Yes", trip_purpose := "Go shopping"]
purposes[n == 1 & TRIP_PURPOSE_OTHER_ERRANDS == "Yes", trip_purpose := "Other errands"]
purposes[is.na(trip_purpose) & !is.na(TRIP_PURPOSE_OTHER), trip_purpose := "Other"]
purposes[, trip_purpose := factor(trip_purpose)]
tbi <- merge(tbi, purposes[, .(ID, trip_purpose)], by = "ID")

# factorize variables
cols <- c("VISITOR",
          "CAN_USE_VEH_TRIP",
          "HAS_DRIVE_LICENSE", 
          "HOME_LANG_OTHER")
tbi[, (cols) := lapply(.SD, factor), .SDcols = cols]
tbi[, INCOME := factor(INCOME, levels = c("Less than 15,000", "15,000 - 24,999", "25,000 - 34,999", "35,000 -  59,999", 
                                          "60,000  - 99,999", "100,000 - 149,999", "150,000 - 199,999", "200,000 or more"))]
# rename long variable names
setnames(tbi, c("TRIP_PURPOSE_BUY_A_MEAL_/_BEVERAGE", "TRIP_PURPOSE_VISIT_FRIEND/RELATIVE_OR_ATTEND_RELIGIOUS/SOC_EVENT"),
         c("Purpose_buy_a_meal_beverage", "Purpose_visit_friend_relative_or_attend_religious_social_event"))
# rename A Line, Blue Line, Green Line, and Red Line
tbi[, route := ifelse(route == "ALINE", "921", ifelse(route == "BLUE", "901", ifelse(route == "GREEN", "902", ifelse(route == "RED", "903", route))))]
tbi[ID == 17198, `:=` (DESTINATION_LAT = as.character(geocode(destination)$lat),
                       DESTINATION_LON = as.character(geocode(destination)$lon))]
# filter out observations which ID consists of 'DM'
tbi <- tbi[!grepl("DM", ID)]

# duplicate tables and only swap origin and destination for easy filtering 
tbi_orig <- tbi[, .(origin, destination, route, lon = as.numeric(ORIGIN_LON), lat = as.numeric(ORIGIN_LAT),
                    ba_lat = as.numeric(BOARDING_LAT), ba_lon = as.numeric(BOARDING_LON), ID, 
                    Key = seq(1, 2*nrow(tbi), 2), Type = "Orig")]
tbi_dest <- tbi[, .(origin, destination, route, lon = as.numeric(DESTINATION_LON), lat = as.numeric(DESTINATION_LAT),
                    ba_lat = as.numeric(ALIGHTING_LAT), ba_lon = as.numeric(ALIGHTING_LON), ID, 
                    Key = seq(2, 2*nrow(tbi), 2), Type = "Dest")]
tbi_OD <- rbind(tbi_orig, tbi_dest, fill = TRUE)
setkey(tbi_OD, Key)
tbi_OD[, route := ifelse(route == "ALINE", "921", ifelse(route == "BLUE", "901", ifelse(route == "GREEN", "902", ifelse(route == "RED", "903", route))))]

saveRDS(tbi_OD, "Data/tbi_OD.RDS")

# get o-d and boarding-alighting spatial lines ready for plotting ----
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL, route = NULL) {
  
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
    
    lines <- SpatialLines(list(Lines(list(Line(data)), paste0("id", route))))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), paste0(route, "r1"))))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0(route, "r", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# get spatial lines between origins and destinations
od_lines <- list()
for (i in na.omit(tbi$ID)) {
  print(i)
  od_lines[[i]] <- points_to_line(data = tbi_OD[ID == i], long = 'lon', lat = 'lat', route = i)
}
# get spatial lines between boarding and alighting locations
ba_lines <- list()
for (i in na.omit(tbi$ID)) {
  print(i)
  ba_lines[[i]] <- points_to_line(data = tbi_OD[ID == i], long = 'ba_lon', lat = 'ba_lat', route = i)
}
saveRDS(od_lines, "Data/od_lines.RDS")
saveRDS(ba_lines, "Data/ba_lines.RDS")

# read in and save transit routes and park and ride lots ----
routes <- readOGR('Data/shp_trans_transit_routes/', 'TransitRoutes', stringsAsFactors = F)
routes <- spTransform(routes, CRS("+proj=longlat +datum=WGS84"))
saveRDS(routes, "Data/routes_sp.RDS")
pnr <- readOGR('Data/shp_trans_park_and_ride_lots/', 'ParkAndRideLots', stringsAsFactors = F)
pnr <- spTransform(pnr, CRS("+proj=longlat +datum=WGS84"))
pnr <- subset(pnr, Active == 1)
saveRDS(pnr, 'Data/pnr.RDS')

# tracts ----
tracts <- readOGR('Data/shp_society_census2010tiger/', 'Census2010TigerTract', stringsAsFactors = F)
tracts <- spTransform(tracts, CRS("+proj=longlat +datum=WGS84"))
# odPoints1 <- SpatialPointsDataFrame(tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ORIGIN_LON, ORIGIN_LAT)], 
#                                     data = tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ID)])
# odPoints2 <- SpatialPointsDataFrame(tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), 
#                                         .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
#                                     data = tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
# odPoints <- do.call('rbind', list(odPoints1, odPoints2))
# proj4string(odPoints) <- proj4string(tracts)
# mergePoints <- data.table(cbind(odPoints@data, over(odPoints, tracts)))
# tracts <- subset(tracts, GEOID10 %in% mergePoints$GEOID10)
saveRDS(tracts, "Data/tracts.RDS")

# blocks ----
blocks <- readOGR('Data/shp_society_census2010tiger/', 'Census2010TigerBlock', stringsAsFactors = F)
blocks <- spTransform(blocks, CRS("+proj=longlat +datum=WGS84"))
odPoints1 <- SpatialPointsDataFrame(tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ORIGIN_LON, ORIGIN_LAT)], 
                                    data = tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ID)])
odPoints2 <- SpatialPointsDataFrame(tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), 
                                        .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
                                    data = tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
odPoints <- do.call('rbind', list(odPoints1, odPoints2))
proj4string(odPoints) <- proj4string(blocks)
mergePoints <- data.table(cbind(odPoints@data, over(odPoints, blocks)))
blocks <- subset(blocks, GEOID10 %in% mergePoints$GEOID10)
saveRDS(blocks, "Data/blocks.RDS")

# TAZ ----
taz <- readOGR('Data/shp_trans_anlys_zones_offical_curent/', 'TAZOfficialWCurrentForecasts', stringsAsFactors = F)
taz <- spTransform(taz, CRS("+proj=longlat +datum=WGS84"))
# odPoints1 <- SpatialPointsDataFrame(tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ORIGIN_LON, ORIGIN_LAT)], 
#                                     data = tbi[!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON), .(ID)])
# odPoints2 <- SpatialPointsDataFrame(tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), 
#                                         .(as.numeric(DESTINATION_LON), as.numeric(DESTINATION_LAT))], 
#                                     data = tbi[!is.na(DESTINATION_LAT) & !is.na(DESTINATION_LON), .(ID)])
# odPoints <- do.call('rbind', list(odPoints1, odPoints2))
# proj4string(odPoints) <- proj4string(taz)
# mergePoints <- data.table(cbind(odPoints@data, over(odPoints, taz)))
# taz <- subset(taz, TAZ %in% mergePoints$TAZ)
saveRDS(taz, "Data/taz.RDS")

# get stop sequence -----
# ch <- odbcConnect("db_prod_allstop", uid = "sdreport", pwd = "tdr3port")
# stopSeq <- sqlQuery(ch, "SELECT ss.line_id, ss.direction_number, ss.stop_sequence_number, ss.site_id, s.site_on, s.site_at, s.site_downtown_zone, s.site_parkride, s.node_id FROM stop_sequence ss INNER JOIN site s on ss.site_id = s.site_id")
# saveRDS(stopSeq, "Data/stopSequence.RDS")
stopSeq <- as.data.table(readRDS("Data/stopSequence.RDS"))
stopSeq <- stopSeq[order(line_id, direction_number, stop_sequence_number)]
stopSeq[, line_id := trimws(line_id)]
stopSeq[, stopCluster := ifelse(site_downtown_zone == "Y", "downtown", NA)]

# stopSeqDir <- data.table(rowID = as.numeric(), route = as.numeric(), dir = as.numeric(), boardSeq = as.numeric(), alightSeq = as.numeric())
# for (i in 1:nrow(tbi)) {
#   print(i)
#   temp <- tbi[i, ]
#   board <- stopSeq[site_id == temp$BOARDING_STOPID & line_id == temp$route]
#   alight <- stopSeq[site_id == temp$ALIGHTING_STOPID & line_id == temp$route]
#   boardDir <- unique(board$direction_number)
#   alightDir <- unique(alight$direction_number)
#   dir <- intersect(boardDir, alightDir)
#   if (length(dir) == 0) { dir <- NA }
#   if (length(dir) == 2) {
#     if (board[direction_number == dir[1], stop_sequence_number] < alight[direction_number == dir[1], stop_sequence_number]) {
#       dir <- dir[1]
#     } else if ((board[direction_number == dir[2], stop_sequence_number] < alight[direction_number == dir[2], stop_sequence_number])) {
#       dir <- dir[2]
#     } else {
#       dir <- -999
#     }
#   }
#   boardSeq <- board[direction_number == dir, mean(stop_sequence_number, na.rm = TRUE)]
#   alightSeq <- alight[direction_number == dir, mean(stop_sequence_number, na.rm = TRUE)]
#   stopSeqDir <- rbind(stopSeqDir, data.table(rowID = i, route = temp$route, dir = dir, boardSeq = boardSeq, alightSeq = alightSeq))
# }
# setkey(stopSeqDir, rowID)
# 
# tbi <- cbind(tbi, stopSeqDir[, .(dir, boardSeq, alightSeq)])
# tbi[boardSeq > alightDir & route == 901, dir := abs(dir - 1)]
# tbi[boardSeq > alightDir & route == 902, dir := abs(dir - 1)]
# # these ids have reverse direction - manually checked
# ids <- c(20616, 24988, 20042, 20061, 20538, 22731, 29407, 25581, 25647, 26855, 27076, 27153, 14267, 24415, 24540, 25829)
# tbi[ID %in% ids, dir := abs(dir - 1)]
# 
# # all obs that are going northbound/eastbound, keep their origin-destination
# # all obs that are going southbound/westbound, reverse their origin-destination to match
# tbi[, `:=` (orig_new_lat = ifelse(dir == 0, ORIGIN_LAT, DESTINATION_LAT),
#             orig_new_lon = ifelse(dir == 0, ORIGIN_LON, DESTINATION_LON),
#             dest_new_lat = ifelse(dir == 0, DESTINATION_LAT, ORIGIN_LAT),
#             dest_new_lon = ifelse(dir == 0, DESTINATION_LON, ORIGIN_LON))]
# 
# # assume home/hotel is the origin
# # 88.1% say either their origin or destination is their home
# newDest <- data.table(ID = as.numeric(), newDestLat = as.numeric(), newDestLon = as.numeric())
# for (i in nrow(tbi)) {
#   if (tbi[i, ORIGIN_PLACE_TYPE] == "Your HOME") {
#     temp <- data.table(ID = tbi[i, ID], newDestLat = tbi[i, as.numeric(DESTINATION_LAT)], newDestLon = tbi[i, as.numeric(DESTINATION_LON)])
#     newDest <- rbind(newDest, temp)
#   } else if (tbi[i, DESTIN_PLACE_TYPE] == "Your HOME") {
#     temp <- data.table(ID = tbi[i, ID], newDestLat = tbi[i, as.numeric(ORIGIN_LAT)], newDestLon = tbi[i, as.numeric(ORIGIN_LON)])
#     newDest <- rbind(newDest, temp)
#   } else if (tbi[i, DESTIN_PLACE_TYPE] == "Work" | tbi[i, DESTIN_PLACE_TYPE] == "Work Related") {
# 
#   } else {
#     home_to_orig <- haversine(tbi[i, HOME_OR_HOTEL_ADDR_LAT], tbi[i, HOME_OR_HOTEL_ADDR_LON], tbi[i, ORIGIN_LAT], tbi[i, ORIGIN_LON])
#     home_to_dest <- haversine(tbi[i, HOME_OR_HOTEL_ADDR_LAT], tbi[i, HOME_OR_HOTEL_ADDR_LON], tbi[i, DESTINATION_LAT], tbi[i, DESTINATION_LON])
#     if (home_to_orig > home_to_dest)
#   }
# }
saveRDS(tbi, 'Data/tbi.RDS')

# map <- get_map('minneapolis-st paul', source = "stamen", zoom = 11)
# ggmap(map, extent = "panel", maprange = FALSE, fullpage = TRUE) + geom_density2d(data = tbi_2, aes(x = lon_orig, y = lat_orig)) + 
#   stat_density2d(data = tbi_2, aes(x = lon_orig, y = lat_orig,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') + 
#   scale_fill_gradient(low = "green", high = "red") + theme(legend.position = 'none')
# 
# ggmap(map, extent = "panel", maprange = FALSE, fullpage = TRUE) + geom_density2d(data = tbi_2, aes(x = lon_dest, y = lat_dest)) + 
#   stat_density2d(data = tbi_2, aes(x = lon_dest, y = lat_dest,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') + 
#   scale_fill_gradient(low = "green", high = "red") + theme(legend.position = 'none')

# # flow chart example
# library(dplyr)
# 
# df <- tbi[RouteType == "ALINE", .(BOARDING_LOCATION, ALIGHTING_LOCATION)]
# 
# df2 <- df %>%
#   summarize(counts = n()) %>%
#   group_by(BOARDING_LOCATION, ALIGHTING_LOCATION) %>%
#   ungroup() %>%
#   arrange(desc(counts))
# 
# parset(df2, dimensions = c('BOARDING_LOCATION', 'ALIGHTING_LOCATION'), 
#        value = htmlwidgets::JS("function(d){return d.counts}"), 
#        tension = 0.5)

# Flow charts -----
sort(table(tbi$route))
# on off data ----
bus_on_off <- fread("Data/On2Off_Bus.csv", stringsAsFactors = FALSE)
rail_on_off <- fread("Data/On2Off_Rail.csv", stringsAsFactors = FALSE)
bus_on_off[, Route := (substr(ROUTE_DESCRIPTION, 1, 2))]
bus_on_off[Route == "AL", Route := "921"]
bus_on_off[, Route := as.numeric(Route)]
# get direction
bus_on_off[ROUTE_DESCRIPTION %in% c("10 Central Av - University Av - Northtown [NB]", "11 Columbia Heights - 2nd St NE - 4th Av S [NB]", 
                                    "14 Robbinsdale-West Broadway-Bloomington Av [NB]", "18 Nicollet Av - South Bloomington [NB]", 
                                    "19 Olson Memorial Hwy - Penn Av N - Brooklyn Center [NB]", "22 Brklyn Ctr - Lyndale Av N - Cedar - 28th Av S - VA [NB]", 
                                    "4 New Brighton - Johnson St - Bryant Av - Southtown [NB]", "5 Brklyn Center - Fremont - 26th Av - Chicago - MOA [NB]", 
                                    "6 U of M - Hennepin - Xerxes - France - Southdale [NB]", "68 Jackson St - Robert St - 5th Av - Inver Hills [NB]", 
                                    "84 Rosedale - Snelling - 46th St LRT - Sibley Plaza [NB]", "ALINE Northbound A-Line Rapid Transit"), Dir := "NB"]
bus_on_off[ROUTE_DESCRIPTION %in% c("10 Central Av - University Av - Northtown [SB]", "11 Columbia Heights - 2nd St NE - 4th Av S [SB]", 
                                    "14 Robbinsdale-West Broadway-Bloomington Av [SB]", "18 Nicollet Av - South Bloomington [SB]", 
                                    "19 Olson Memorial Hwy - Penn Av N - Brooklyn Center [SB]", "22 Brklyn Ctr - Lyndale Av N - Cedar - 28th Av S - VA [SB]", 
                                    "4 New Brighton - Johnson St - Bryant Av - Southtown [SB]", "5 Brklyn Center - Fremont - 26th Av - Chicago - MOA [SB]", 
                                    "6 U of M - Hennepin - Xerxes - France - Southdale [SB]", "68 Jackson St - Robert St - 5th Av - Inver Hills [SB]", 
                                    "84 Rosedale - Snelling - 46th St LRT - Sibley Plaza [SB]", "ALINE Southbound A-Line Rapid Transit"), Dir := "SB"]
bus_on_off[ROUTE_DESCRIPTION %in% c("16 U of M - University Av - Midway [EB]", "17 Minnetonka Blvd - Uptown - Washington St NE [EB]", 
                                    "2 Franklin Av - Riverside Av - U of M - 8th St SE [EB]", "21 Uptown - Lake St - Selby  Av [EB]", 
                                    "3 U of M - Como Av - Energy Park Dr - Maryland Av [EB]", "54 Ltd Stop - W 7St - Airport - MOA [EB]", 
                                    "63 Grand Av - Raymond Sta - Sunray - McKnight Rd [EB]", "64 Payne - Maryland - White Bear Av - Maplewood [EB]", 
                                    "74 46St - Randolph - W 7St - E 7St - Sunray [EB]", "9 Glenwood Av - Wayzata Blvd - Cedar Lk Rd -46St LRT [EB]"), Dir := "EB"]
bus_on_off[ROUTE_DESCRIPTION %in% c("16 U of M - University Av - Midway [WB]", "17 Minnetonka Blvd - Uptown - Washington St NE [WB]", 
                                    "2 Franklin Av - Riverside Av - U of M - 8th St SE [WB]", "21 Uptown - Lake St - Selby  Av [WB]", 
                                    "3 U of M - Como Av - Energy Park Dr - Maryland Av [WB]", "54 Ltd Stop - W 7St - Airport - MOA [WB]", 
                                    "63 Grand Av - Raymond Sta - Sunray - McKnight Rd [WB]", "64 Payne - Maryland - White Bear Av - Maplewood [WB]", 
                                    "74 46St - Randolph - W 7St - E 7St - Sunray [WB]", "9 Glenwood Av - Wayzata Blvd - Cedar Lk Rd -46St LRT [WB]"), Dir := "WB"]
# rename rail lines
rail_on_off[, line_id := ifelse(ROUTE == "METRO Blue Line", 901, ifelse(ROUTE == "METRO Green Line", 902, ifelse(ROUTE == "METRO Red Line", 903, ifelse(ROUTE == "Northstar-Big Lk-Elk Rv-Anoka-Coon Rp-Mpls", 888, -99))))]
rail_on_off[DIRECTION == "Eastbound", Dir := "EB"]
rail_on_off[DIRECTION == "Northbound", Dir := "NB"]
rail_on_off[DIRECTION == "Southbound", Dir := "SB"]
rail_on_off[DIRECTION == "Westbound", Dir := "WB"]

# manually clustering stops ----
# route 10
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 14:26, stopCluster := "CE4S - LWCE"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 27:39, stopCluster := "CE26 - 40CE"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 40:41, stopCluster := "41CE"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 42:52, stopCluster := "CE41 - 51CE"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 53:58, stopCluster := "53TA - 534S"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 59:66, stopCluster := "53UV - UV81"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 67:75, stopCluster := "CE52 - CEMI"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 76:86, stopCluster := "CE66 - CE76"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 87:94, stopCluster := "OSCE - OSQU"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 95:106, stopCluster := "OSMN - 85JF"]
stopSeq[line_id == 10 & direction_number == 0 & stop_sequence_number %in% 107:108, stopCluster := "NOTW"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 1, stopCluster := "NOTW"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 2:10, stopCluster := "53UV - UV81"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 11:16, stopCluster := "53TA - 534S"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 17:29, stopCluster := "OSMN - 85JF"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 30:35, stopCluster := "OSCE - OSQU"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 36:46, stopCluster := "CE66 - CE76"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 47:58, stopCluster := "CE52 - CEMI"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 59:69, stopCluster := "CE41 - 51CE"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 70, stopCluster := "41CE"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 71:81, stopCluster := "CE26 - 40CE"]
stopSeq[line_id == 10 & direction_number == 1 & stop_sequence_number %in% 82:94, stopCluster := "CE4S - LWCE"]

stop_seq_fc <- copy(stopSeq)
# blue line
stop_seq_fc[line_id == 901 & node_id %in% c("MAAM", "28AV", "BLCT", "AM34"), stopCluster := "AM34 - MAAM"]
stop_seq_fc[line_id == 901 & node_id %in% c("HHTE", "LIND"), stopCluster := "Airport terminals"]
stop_seq_fc[line_id == 901 & node_id %in% c("FTSN", "VAMC", "50HI", "46HI"), stopCluster := "46HI - FTSN"]
stop_seq_fc[line_id == 901 & node_id %in% c("38HI", "LAHI", "FRHI", "HIWA", "CDRV", "JUCR"), stopCluster := "CDRV - 38HI"]
# green line
stop_seq_fc[line_id == 902 & node_id %in% c("TF22", "TF12", "TF11", "WAR1", "WAR2", "WHSX", "5SNI", "GOVT", "USB2"), stopCluster := "downtown Minneapolis"]
stop_seq_fc[line_id == 902 & node_id %in% c("JUCR", "WEBK", "EABK", "STVI"), stopCluster := "U of M"]
stop_seq_fc[line_id == 902 & node_id %in% c("PSPK", "WGAT", "RAST", "SNUN", "FAUN"), stopCluster := "PSPK - SNUN"]
stop_seq_fc[line_id == 902 & node_id %in% c("HMUN", "LXUN", "VIUN", "UNDA", "WEUN"), stopCluster := "HMUN - WEUN"]
stop_seq_fc[line_id == 902 & node_id %in% c("UNRI", "ROST", "10CE", "CNST", "UNDP"), stopCluster := "downtown St. Paul"]
# A line 
stop_seq_fc[line_id == 921 & site_id %in% c(52531, 50015, 50013), stopCluster := "ROSE - CO RD B"]
stop_seq_fc[line_id == 921 & site_id %in% c(50032, 56121, 18523, 15114, 50010, 56120, 56119, 18508), stopCluster := "SNLA - HEWITT"]
stop_seq_fc[line_id == 921 & site_id %in% c(15116, 56122, 17307, 56118, 56117, 17371), stopCluster := "Minnehaha - Dayton"]
stop_seq_fc[line_id == 921 & site_id %in% c(17312, 17318, 56123, 17332, 17366, 56116, 56115, 56114), stopCluster := "Grand - Highland"]
stop_seq_fc[line_id == 921 & site_id %in% c(56124, 56125, 56126, 56127, 56113, 4283, 56112, 56111), stopCluster := "FORD"]
stop_seq_fc[line_id == 921 & site_id %in% c(56128, 16607, 51544, 51514, 53362), stopCluster := "46th St"]

# route 5
# ch2 <- odbcConnect("db_SDWarehouse", uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))
# dstop <- as.data.table(sqlQuery(ch2, "SELECT DISTINCT ds.stop_sequence_number, ds.node_id, ds.site_id, ds.site_downtown_zone, ds.SITE_LONGITUDE, ds.SITE_LATITUDE, dt.line_id, dt.line_direction_number, 
#                                                       dt.pat_id FROM FACT_APC apc
#                                 INNER JOIN DIM_STOP ds ON apc.STOPSK = ds.STOPSK 
#                                 INNER JOIN DIM_TRIP_TM dt ON apc.TRIP_TMSK = dt.TRIP_TMSK"))
# saveRDS(dstop, "Data/stopsAPC.RDS")
stop_seq_fc[line_id == 5 & site_id %in% c(48337, 44641, 4511, 4513, 4514, 4515, 4516, 4517, 4518, 47447, 4520, 618, 619, 687, 4531, 
                                          4530, 4529, 42318, 4525, 4524, 4523, 4521, 450), stopCluster := "MOA - Portland/78th"]
stop_seq_fc[line_id == 5 & site_id %in% c(449, 447, 446, 445, 444, 443, 442, 441, 440, 439, 437, 436, 435, 434, 47787, 432, 431, 429, 
                                          79, 78, 77, 76, 620, 633, 634, 635, 636, 637, 638, 639, 640, 641, 642, 643, 733, 47786, 735, 736, 737, 739, 741, 
                                          742, 743, 744), stopCluster := "Chicago/59th - Portland/77th"]
stop_seq_fc[line_id == 5 & site_id %in% c(51212, 775, 776, 777, 778, 779, 780, 781, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792,
                                          793, 794, 795, 796, 797, 798, 799, 800, 801, 52258, 40472, 74, 73, 72, 71, 70, 69, 68, 67, 
                                          66, 65, 64, 63, 62, 61, 60, 59, 58, 43274, 57, 56, 55, 54, 53, 52, 51, 50), stopCluster := "56CH - ARCL"]
stop_seq_fc[line_id == 5 & site_id %in% c(52300, 803, 806, 807, 808, 809, 810, 811, 813, 815, 817, 819, 52261, 48, 47, 46, 45, 44, 43, 42, 40, 39, 
                                          37, 36, 34, 56432), stopCluster := "CHLA - 8SCH"]
stop_seq_fc[line_id == 5 & site_id %in% c(8916, 8918, 8919, 8920, 8922, 8923, 8925, 43275, 8927, 8928, 8929, 8930, 56519, 11084, 11091, 50661, 
                                          52708, 11167, 51791, 11163, 11161, 11160, 11159, 11158, 11157, 11156, 11155, 11154, 11153, 11152, 11151, 
                                          56518, 11085, 11089, 50660), stopCluster := "7th/Olson - 26th/W Broadway"]
stop_seq_fc[line_id == 5 & site_id %in% c(56515, 8931, 8932, 8933, 8934, 8935, 8936, 8937, 8938, 8939, 8940, 8941, 8942, 8943, 8944, 8945, 8946, 
                                          8947, 56515, 11150, 11149, 11148, 11147, 11146, 53493, 11145, 11144, 11143, 11142, 11141, 9619, 9617, 
                                          9615, 9613, 9611, 9609, 9607), stopCluster := "44FM - 26BR"]
stop_seq_fc[line_id == 5 & site_id %in% c(8948, 8949, 9636, 9642, 9646, 9648, 9652, 9656, 9658, 9257, 9259, 9263, 8974, 52008, 9089, 9283, 9284, 51847, 
                                          52709, 9472, 9086, 9087, 50142, 9090, 9173, 9176, 9575, 9577, 9582, 9584, 9586, 9589, 9593, 9599, 9603, 9605), 
            stopCluster := "44th/Girard - BCTC"]
# route 18 
stop_seq_fc[line_id == 18 & site_id %in% c(41541, 1397, 1399, 1400, 1401, 1402, 1403, 1404, 1405, 50026, 1406, 1407, 51631, 51821, 51858, 43664, 43666, 1417, 1418, 
                                           1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426, 1427, 1428, 1429, 1430, 1431, 1432, 1433, 40946, 40942, 1393, 1391, 
                                           42516, 42515, 41514, 51854, 51821, 42099, 43616, 43614, 1380, 1379, 1378, 1377, 1376, 1375, 1374, 1373, 1372, 1371, 
                                           1370, 1369, 1368, 1367, 1366, 1365, 1364, 42514), stopCluster := "104BL - NI81"]
stop_seq_fc[line_id == 18 & site_id %in% c(51852, 1435, 1436, 1437, 1438, 1439, 1440, 1441, 1442, 1443, 1444, 1445, 1446, 53185, 1751, 1753, 48129, 1361, 1360, 
                                           1359, 1358, 1357, 1356, 1355, 1354, 1353, 1352, 1351, 1350, 4361, 4360, 4359, 4357), stopCluster := "AM2A - 66HC"]
stop_seq_fc[line_id == 18 & site_id %in% c(1754, 1756, 1758, 1759, 1760, 1762, 1763, 1764, 1765, 1766, 3392, 1768, 1769, 1770, 1863, 1869, 1870, 1349, 1348, 1347, 1346, 1345, 1344, 1342, 1341, 1340, 1339, 1338, 1336, 1335, 1231, 1230, 1229, 
                                           1228, 1868, 1867, 1866, 1865), stopCluster := "66NI - 47NI"]
stop_seq_fc[line_id == 18 & direction_number == 0 & stop_sequence_number %in% 68:82, stopCluster := "46NI - 32NI"]
stop_seq_fc[line_id == 18 & direction_number == 0 & stop_sequence_number %in% 83:100, stopCluster := "31NI - 32GR"]
stop_seq_fc[line_id == 18 & direction_number == 0 & stop_sequence_number %in% 101:116, stopCluster := "31GR - 15NI"]
stop_seq_fc[line_id == 18 & direction_number == 1 & stop_sequence_number %in% 12:26, stopCluster := "31GR - 15NI"]
stop_seq_fc[line_id == 18 & direction_number == 1 & stop_sequence_number %in% 27:45, stopCluster := "31NI - 32GR"]
# route 11
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number %in% 1:11, stopCluster := "49WE - 464A"]
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number %in% 12:27, stopCluster := "454A - 4ALA"]
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number %in% 28:39, stopCluster := "284A - 183A"]
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number == 41, stopCluster := "downtown"]
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number %in% 54:69, stopCluster := "2S3A - 29GR"]
stop_seq_fc[line_id == 11 & direction_number == 0 & stop_sequence_number %in% 70:87, stopCluster := "30RA - 41CE"]
stop_seq_fc[line_id == 11 & direction_number == 1 & stop_sequence_number %in% 1:21, stopCluster := "30RA - 41CE"]
stop_seq_fc[line_id == 11 & direction_number == 1 & stop_sequence_number %in% 22:37, stopCluster := "2S3A - 29GR"]
stop_seq_fc[line_id == 11 & direction_number == 1 & stop_sequence_number %in% 54:64, stopCluster := "284A - 183A"]
stop_seq_fc[line_id == 11 & direction_number == 1 & stop_sequence_number %in% 65:80, stopCluster := "454A - 4ALA"]
stop_seq_fc[line_id == 11 & direction_number == 1 & stop_sequence_number %in% 81:90, stopCluster := "49WE - 464A"]
# route 14
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 1:21, stopCluster := "6617 - BL54"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 22:37, stopCluster := "BL53 - 38BL"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 38:62, stopCluster := "38HI - BLLA"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 63:77, stopCluster := "BL29 - 7S10"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number == 85, stopCluster := "downtown"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 94:107, stopCluster := "WA5A - GVKN"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 108:139, stopCluster := "GVMO - MDOT"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 140:162, stopCluster := "DUAD - 36OR"]
stop_seq_fc[line_id == 14 & direction_number == 0 & stop_sequence_number %in% 163:191, stopCluster := "BRKN - RBTC"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 165:186, stopCluster := "6617 - BL54"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 149:164, stopCluster := "BL53 - 38BL"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 122:148, stopCluster := "38HI - BLLA"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 108:121, stopCluster := "BL29 - 7S10"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 82:97, stopCluster := "WA5A - GVKN"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% c(29:40, 62:81), stopCluster := "GVMO - MDOT"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 41:61, stopCluster := "DUAD - 36OR"]
stop_seq_fc[line_id == 14 & direction_number == 1 & stop_sequence_number %in% 1:28, stopCluster := "BRKN - RBTC"]
# route 16
stop_seq_fc[line_id == 16 & direction_number == 0 & stop_sequence_number %in% 1:9, stopCluster := "27UN - RAST"]
stop_seq_fc[line_id == 16 & direction_number == 0 & stop_sequence_number %in% 10:19, stopCluster := "UNHA - SNUN"]
stop_seq_fc[line_id == 16 & direction_number == 0 & stop_sequence_number %in% 20:28, stopCluster := "UNSI - OXUN"]
stop_seq_fc[line_id == 16 & direction_number == 0 & stop_sequence_number %in% 29:35, stopCluster := "UNCH - UNDA"]
stop_seq_fc[line_id == 16 & direction_number == 0 & stop_sequence_number %in% 36:43, stopCluster := "UNKE - RAMA"]
stop_seq_fc[line_id == 16 & direction_number == 1 & stop_sequence_number %in% 43:52, stopCluster := "27UN - RAST"]
stop_seq_fc[line_id == 16 & direction_number == 1 & stop_sequence_number %in% 34:42, stopCluster := "UNHA - SNUN"]
stop_seq_fc[line_id == 16 & direction_number == 1 & stop_sequence_number %in% 25:33, stopCluster := "UNSI - OXUN"]
stop_seq_fc[line_id == 16 & direction_number == 1 & stop_sequence_number %in% 18:24, stopCluster := "UNCH - UNDA"]
stop_seq_fc[line_id == 16 & direction_number == 1 & stop_sequence_number %in% 10:17, stopCluster := "UNKE - RAMA"]
# route 17
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 1:14, stopCluster := "TYLA - MTTE"]
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 15:34, stopCluster := "MTPE - LAFR"]
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 35:52, stopCluster := "37BR - LAFR"]
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 53:62, stopCluster := "BEL7 - 31IR"]
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 63:81, stopCluster := "HELA - NI15"]
stop_seq_fc[line_id == 17 & direction_number == 0 & stop_sequence_number %in% 94:114, stopCluster := "CE4S - 27WA"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 102:118, stopCluster := "TYLA - MTTE"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 81:101, stopCluster := "MTPE - LAFR"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 60:80, stopCluster := "37BR - LAFR"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 53:59, stopCluster := "BEL7 - 31IR"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 35:52, stopCluster := "HELA - NI15"]
stop_seq_fc[line_id == 17 & direction_number == 1 & stop_sequence_number %in% 1:20, stopCluster := "CE4S - 27WA"]
# route 19
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number == 2, stopCluster := "downtown"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 10:16, stopCluster := "7SOL - PE55"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 17:31, stopCluster := "PE8A - PE30"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 32:43, stopCluster := "LWPE - 36YO"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 44:57, stopCluster := "LWPE - 42YK"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 58:68, stopCluster := "PEDO - 47OS"]
stop_seq_fc[line_id == 19 & direction_number == 0 & stop_sequence_number %in% 69:77, stopCluster := "BR49 - BCTC"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number == 81, stopCluster := "downtown"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 65:72, stopCluster := "7SOL - PE55"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 50:64, stopCluster := "PE8A - PE30"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 39:49, stopCluster := "LWPE - 36YO"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 23:38, stopCluster := "LWPE - 42YK"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 10:22, stopCluster := "PEDO - 47OS"]
stop_seq_fc[line_id == 19 & direction_number == 1 & stop_sequence_number %in% 1:9, stopCluster := "BR49 - BCTC"]
# route 2
stop_seq_fc[line_id == 2 & direction_number == 0 & stop_sequence_number %in% 1:9, stopCluster := "22HE - NIFR"]
stop_seq_fc[line_id == 2 & direction_number == 0 & stop_sequence_number %in% 10:19, stopCluster := "FRST - FRHI"]
stop_seq_fc[line_id == 2 & direction_number == 0 & stop_sequence_number %in% 20:30, stopCluster := "FRMI - CEWA"]
stop_seq_fc[line_id == 2 & direction_number == 0 & stop_sequence_number %in% 31:37, stopCluster := "ANHA - 4S15"]
stop_seq_fc[line_id == 2 & direction_number == 0 & stop_sequence_number %in% 38:46, stopCluster := "4S13 - 7E2E"]
stop_seq_fc[line_id == 2 & direction_number == 1 & stop_sequence_number %in% 40:46, stopCluster := "22HE - NIFR"]
stop_seq_fc[line_id == 2 & direction_number == 1 & stop_sequence_number %in% 29:39, stopCluster := "FRST - FRHI"]
stop_seq_fc[line_id == 2 & direction_number == 1 & stop_sequence_number %in% 18:28, stopCluster := "FRMI - CEWA"]
stop_seq_fc[line_id == 2 & direction_number == 1 & stop_sequence_number %in% 10:17, stopCluster := "ANHA - 4S15"]
stop_seq_fc[line_id == 2 & direction_number == 1 & stop_sequence_number %in% 1:9, stopCluster := "4S13 - 7E2E"]
# route 21
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 1:8, stopCluster := "UPTS - BLLK"]
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 9:21, stopCluster := "1ALA - CELA"]
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 22:35, stopCluster := "19LA - 44LA"]
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 36:54, stopCluster := "LARP - UNSN"]
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 55:64, stopCluster := "SNUN - HAMA"]
stop_seq_fc[line_id == 21 & direction_number == 0 & stop_sequence_number %in% 65:82, stopCluster := "SLHA - SLNI"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 83:90, stopCluster := "UPTS - BLLK"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 70:82, stopCluster := "1ALA - CELA"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 58:69, stopCluster := "19LA - 44LA"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 41:57, stopCluster := "LARP - UNSN"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 32:40, stopCluster := "SNUN - HAMA"]
stop_seq_fc[line_id == 21 & direction_number == 1 & stop_sequence_number %in% 14:31, stopCluster := "SLHA - SLNI"]
# route 22
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 1:15, stopCluster := "VETS - 3458"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 16:42, stopCluster := "58SA - 2842"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 43:62, stopCluster := "4228 - CELA"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 63:73, stopCluster := "LA17 - CEWA"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 87:102, stopCluster := "7SOL - LNLY"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 103:115, stopCluster := "LY33 - 45AL"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 116:140, stopCluster := "45BR - 56DP"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 141:152, stopCluster := "57DP - 69HU"]
stop_seq_fc[line_id == 22 & direction_number == 0 & stop_sequence_number %in% 153:175, stopCluster := "57FR - BCTC"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 167:181, stopCluster := "VETS - 3458"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 140:166, stopCluster := "58SA - 2842"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 120:139, stopCluster := "4228 - CELA"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 109:119, stopCluster := "LA17 - CEWA"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 79:93, stopCluster := "7SOL - LNLY"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 64:78, stopCluster := "LY33 - 45AL"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 30:63, stopCluster := "45BR - 56DP"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 1:13, stopCluster := "57DP - 69HU"]
stop_seq_fc[line_id == 22 & direction_number == 1 & stop_sequence_number %in% 14:29, stopCluster := "57FR - BCTC"]
# route 3
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 1:7, stopCluster := "downtown MPLS"]
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 8:13, stopCluster := "CEWA - 8S15"]
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 14:35, stopCluster := "15RO - COES"]
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 36:48, stopCluster := "COBF - COWI"]
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 49:115, stopCluster := "COSN - RICH"]
stop_seq_fc[line_id == 3 & direction_number == 0 & stop_sequence_number %in% 116:125, stopCluster := "downtown SP"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 120:132, stopCluster := "downtown MPLS"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 113:119, stopCluster := "CEWA - 8S15"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 90:112, stopCluster := "15RO - COES"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 77:89, stopCluster := "COBF - COWI"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 49:76, stopCluster := "COSN - RICH"]
stop_seq_fc[line_id == 3 & direction_number == 1 & stop_sequence_number %in% 1:11, stopCluster := "downtown SP"]
# route 4
stop_seq_fc[line_id == 4 & direction_number == 0 & stop_sequence_number %in% 1:80, stopCluster := "82ST - 46BR"]
stop_seq_fc[line_id == 4 & direction_number == 0 & stop_sequence_number %in% 81:108, stopCluster := "45BR - DULI"]
stop_seq_fc[line_id == 4 & direction_number == 0 & stop_sequence_number %in% 124:154, stopCluster := "5SHE - 37JO"]
stop_seq_fc[line_id == 4 & direction_number == 0 & stop_sequence_number %in% 155:183, stopCluster := "37HA - FRH8"]
stop_seq_fc[line_id == 4 & direction_number == 0 & stop_sequence_number %in% 184:205, stopCluster := "H83S - O81A"]
stop_seq_fc[line_id == 4 & direction_number == 1 & stop_sequence_number %in% 118:195, stopCluster := "82ST - 46BR"]
stop_seq_fc[line_id == 4 & direction_number == 1 & stop_sequence_number %in% 92:117, stopCluster := "45BR - DULI"]
stop_seq_fc[line_id == 4 & direction_number == 1 & stop_sequence_number %in% 46:75, stopCluster := "5SHE - 37JO"]
stop_seq_fc[line_id == 4 & direction_number == 1 & stop_sequence_number %in% 21:45, stopCluster := "37HA - FRH8"]
stop_seq_fc[line_id == 4 & direction_number == 1 & stop_sequence_number %in% 1:20, stopCluster := "H83S - O81A"]
# route 54
stop_seq_fc[line_id == 54 & direction_number == 0 & stop_sequence_number %in% 1, stopCluster := "MOA"]
stop_seq_fc[line_id == 54 & direction_number == 0 & stop_sequence_number %in% 2:6, stopCluster := "LI26 - 34AM"]
stop_seq_fc[line_id == 54 & direction_number == 0 & stop_sequence_number %in% 7, stopCluster := "Airport"]
stop_seq_fc[line_id == 54 & direction_number == 0 & stop_sequence_number %in% 8:17, stopCluster := "7MAY - 7SGR"]
stop_seq_fc[line_id == 54 & direction_number == 1 & stop_sequence_number %in% 29, stopCluster := "MOA"]
stop_seq_fc[line_id == 54 & direction_number == 1 & stop_sequence_number %in% 24:28, stopCluster := "LI26 - 34AM"]
stop_seq_fc[line_id == 54 & direction_number == 1 & stop_sequence_number %in% 23, stopCluster := "Airport"]
stop_seq_fc[line_id == 54 & direction_number == 1 & stop_sequence_number %in% 12:22, stopCluster := "7MAY - 7SGR"]
# route 6
stop_seq_fc[line_id == 6 & direction_number == 0 & stop_sequence_number %in% 1:16, stopCluster := "78PI - 77SW"]
stop_seq_fc[line_id == 6 & direction_number == 0 & stop_sequence_number %in% 17:34, stopCluster := "77PA - FEDX"]
stop_seq_fc[line_id == 6 & direction_number == 0 & stop_sequence_number %in% 35:118, stopCluster := "YKHZ - 39SH"]
stop_seq_fc[line_id == 6 & direction_number == 0 & stop_sequence_number %in% 119:138, stopCluster := "RIWB - HEOG"]
stop_seq_fc[line_id == 6 & direction_number == 0 & stop_sequence_number %in% 154:164, stopCluster := "UN2A - 27UN"]
stop_seq_fc[line_id == 6 & direction_number == 1 & stop_sequence_number %in% 143:158, stopCluster := "78PI - 77SW"]
stop_seq_fc[line_id == 6 & direction_number == 1 & stop_sequence_number %in% 131:142, stopCluster := "77PA - FEDX"]
stop_seq_fc[line_id == 6 & direction_number == 1 & stop_sequence_number %in% 54:130, stopCluster := "YKHZ - 39SH"]
stop_seq_fc[line_id == 6 & direction_number == 1 & stop_sequence_number %in% 31:53, stopCluster := "RIWB - HEOG"]
stop_seq_fc[line_id == 6 & direction_number == 1 & stop_sequence_number %in% 1:15, stopCluster := "UN2A - 27UN"]
# route 63
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 1:14, stopCluster := "BRUN - SUCR"]
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 15:23, stopCluster := "GRCR - GRSN"]
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 24:45, stopCluster := "GRSA - SMGR"]
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 63:80, stopCluster := "3MAR - 3SWB"]
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 81:88, stopCluster := "OHHZ - MCKN"]
stop_seq_fc[line_id == 63 & direction_number == 0 & stop_sequence_number %in% 89:101, stopCluster := "MKLO - LAMK"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 85:97, stopCluster := "BRUN - SUCR"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 76:84, stopCluster := "GRCR - GRSN"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 53:75, stopCluster := "GRSA - SMGR"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 20:37, stopCluster := "3MAR - 3SWB"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 14:19, stopCluster := "OHHZ - MCKN"]
stop_seq_fc[line_id == 63 & direction_number == 1 & stop_sequence_number %in% 1:13, stopCluster := "MKLO - LAMK"]
# route 64
stop_seq_fc[line_id == 64 & direction_number == 0 & stop_sequence_number %in% 16:21, stopCluster := "LA600 - BUMI"]
stop_seq_fc[line_id == 64 & direction_number == 0 & stop_sequence_number %in% 22:42, stopCluster := "PARE - MYCL"]
stop_seq_fc[line_id == 64 & direction_number == 0 & stop_sequence_number %in% 43:115, stopCluster := "PRMA - 11WH"]
stop_seq_fc[line_id == 64 & direction_number == 0 & stop_sequence_number %in% 116:122, stopCluster := "WB2570 - MPWD"]
stop_seq_fc[line_id == 64 & direction_number == 1 & stop_sequence_number %in% 106:110, stopCluster := "LA600 - BUMI"]
stop_seq_fc[line_id == 64 & direction_number == 1 & stop_sequence_number %in% 83:105, stopCluster := "PARE - MYCL"]
stop_seq_fc[line_id == 64 & direction_number == 1 & stop_sequence_number %in% 7:82, stopCluster := "PRMA - 11WH"]
stop_seq_fc[line_id == 64 & direction_number == 1 & stop_sequence_number %in% 1:6, stopCluster := "WB2570 - MPWD"]
# route 68 
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 1:13, stopCluster := "CFCA - 80CH"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 14:41, stopCluster := "78CH - 70CL"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 42:57, stopCluster := "CL69 - 5ASP"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 58:78, stopCluster := "5ADA - SV9A"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 79:117, stopCluster := "SV11 - ROTH"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 118:136, stopCluster := "ROEM - ROCO"]
stop_seq_fc[line_id == 68 & direction_number == 0 & stop_sequence_number %in% 147:169, stopCluster := "JAMA - CACM"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 160:172, stopCluster := "CFCA - 80CH"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 137:159, stopCluster := "78CH - 70CL"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 118:136, stopCluster := "CL69 - 5ASP"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 97:117, stopCluster := "5ADA - SV9A"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 56:96, stopCluster := "SV11 - ROTH"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 36:55, stopCluster := "ROEM - ROCO"]
stop_seq_fc[line_id == 68 & direction_number == 1 & stop_sequence_number %in% 1:24, stopCluster := "JAMA - CACM"]

# route 9
stop_seq_fc[line_id == 9 & direction_number == 0 & stop_sequence_number %in% 1:21, stopCluster := "HEBE - CLNE"]
stop_seq_fc[line_id == 9 & direction_number == 0 & stop_sequence_number %in% 22:116, stopCluster := "LUTC - GLAL"]
stop_seq_fc[line_id == 9 & direction_number == 0 & stop_sequence_number %in% 128:157, stopCluster := "POGR - 3633"]
stop_seq_fc[line_id == 9 & direction_number == 0 & stop_sequence_number %in% 158:176, stopCluster := "3634 - 46HI"]
stop_seq_fc[line_id == 9 & direction_number == 1 & stop_sequence_number %in% 154:178, stopCluster := "HEBE - CLNE"]
stop_seq_fc[line_id == 9 & direction_number == 1 & stop_sequence_number %in% 49:153, stopCluster := "LUTC - GLAL"]
stop_seq_fc[line_id == 9 & direction_number == 1 & stop_sequence_number %in% 21:48, stopCluster := "POGR - 3633"]
stop_seq_fc[line_id == 9 & direction_number == 1 & stop_sequence_number %in% 1:20, stopCluster := "3634 - 46HI"]

# route 74
stop_seq_fc[line_id == 74 & direction_number == 0 & stop_sequence_number %in% 1:14, stopCluster := "46HI - CLRA"]
stop_seq_fc[line_id == 74 & direction_number == 0 & stop_sequence_number %in% 15:41, stopCluster := "RAKE - 7RAN"]
stop_seq_fc[line_id == 74 & direction_number == 0 & stop_sequence_number %in% 42:50, stopCluster := "7TOR - 7SGR"]
stop_seq_fc[line_id == 74 & direction_number == 0 & stop_sequence_number %in% 66:99, stopCluster := "7BAT - 7SVD"]
stop_seq_fc[line_id == 74 & direction_number == 0 & stop_sequence_number %in% 100:134, stopCluster := "7SHZ - SURA"]
stop_seq_fc[line_id == 74 & direction_number == 1 & stop_sequence_number %in% 125:139, stopCluster := "46HI - CLRA"]
stop_seq_fc[line_id == 74 & direction_number == 1 & stop_sequence_number %in% 98:124, stopCluster := "RAKE - 7RAN"]
stop_seq_fc[line_id == 74 & direction_number == 1 & stop_sequence_number %in% 88:97, stopCluster := "7TOR - 7SGR"]
stop_seq_fc[line_id == 74 & direction_number == 1 & stop_sequence_number %in% 40:72, stopCluster := "7BAT - 7SVD"]
stop_seq_fc[line_id == 74 & direction_number == 1 & stop_sequence_number %in% 1:39, stopCluster := "7SHZ - SURA"]

# route 84
stop_seq_fc[line_id == 84 & direction_number == 0 & stop_sequence_number %in% 1:20, stopCluster := "DASH - CLFO"]
stop_seq_fc[line_id == 84 & direction_number == 0 & stop_sequence_number %in% 21:38, stopCluster := "KEFO - RASN"]
stop_seq_fc[line_id == 84 & direction_number == 0 & stop_sequence_number %in% 39:53, stopCluster := "RABR - UNSN"]
stop_seq_fc[line_id == 84 & direction_number == 0 & stop_sequence_number %in% 54:69, stopCluster := "SNCH - ROSE"]
stop_seq_fc[line_id == 84 & direction_number == 1 & stop_sequence_number %in% 50:67, stopCluster := "DASH - CLFO"]
stop_seq_fc[line_id == 84 & direction_number == 1 & stop_sequence_number %in% 31:49, stopCluster := "KEFO - RASN"]
stop_seq_fc[line_id == 84 & direction_number == 1 & stop_sequence_number %in% 18:30, stopCluster := "RABR - UNSN"]
stop_seq_fc[line_id == 84 & direction_number == 1 & stop_sequence_number %in% 1:17, stopCluster := "SNCH - ROSE"]

# combine stop clusters to bus on off
stop_seq_fc[, line_id := as.numeric(line_id)]
bus_on_off <- stop_seq_fc[, .(site_id, stopCluster, line_id)][bus_on_off, on = .(site_id = BOARD_STOP_ID, line_id = Route)]
setnames(bus_on_off, "stopCluster", "boardingStopCluster")
bus_on_off <- stop_seq_fc[, .(site_id, stopCluster, line_id)][bus_on_off, on = .(site_id = ALIGHT_STOP_ID, line_id)]
setnames(bus_on_off, "stopCluster", "alightingStopCluster")
setnames(bus_on_off, c("site_id", "i.site_id"), c("BOARD_STOP_ID", "ALIGHT_STOP_ID"))

rail_on_off <- stop_seq_fc[, .(site_id, line_id, stopCluster)][rail_on_off, on = .(site_id = BOARDING_ID, line_id)]
setnames(rail_on_off, "stopCluster", "boardingStopCluster")
rail_on_off <- stop_seq_fc[, .(site_id, line_id, stopCluster)][rail_on_off, on = .(site_id = ALIGHTING_ID, line_id)]
setnames(rail_on_off, "stopCluster", "alightingStopCluster")
setnames(rail_on_off, c("site_id", "i.site_id"), c("BOARDING_ID", "ALIGHTING_ID"))

rail_on_off[BOARDING_LOCATION == "Downtown East Station & Platform" & line_id == 901, boardingStopCluster := "downtown"]
rail_on_off[BOARDING_LOCATION == "Downtown East Station & Platform" & line_id == 902, boardingStopCluster := "downtown Minneapolis"]
rail_on_off[ALIGHTING_LOCATION == "Downtown East Station & Platform" & line_id == 901, alightingStopCluster := "downtown"]
rail_on_off[ALIGHTING_LOCATION == "Downtown East Station & Platform" & line_id == 902, alightingStopCluster := "downtown Minneapolis"]
rail_on_off[BOARDING_LOCATION %in% c("Warehouse Hennepin Ave Station & Platform", "Target Field Station & Platform 1") & line_id == 901, 
            boardingStopCluster := "downtown"]
rail_on_off[BOARDING_LOCATION %in% c("Warehouse Hennepin Ave Station & Platform", "Target Field Station & Platform 1") & line_id == 902, 
            boardingStopCluster := "downtown Minneapolis"]
rail_on_off[ALIGHTING_LOCATION %in% c("Warehouse Hennepin Ave Station & Platform", "Target Field Station & Platform 1") & line_id == 901,  
            alightingStopCluster := "downtown"]
rail_on_off[ALIGHTING_LOCATION %in% c("Warehouse Hennepin Ave Station & Platform", "Target Field Station & Platform 1") & line_id == 902,  
            alightingStopCluster := "downtown Minneapolis"]

rail_on_off[BOARDING_LOCATION == "Target Field Station & Rail Platform", boardingStopCluster := "Target Field"]
rail_on_off[BOARDING_LOCATION == "Fridley Station & Platform", boardingStopCluster := "Fridley"]
rail_on_off[BOARDING_LOCATION == "Coon Rapids Riverdale Station & Platform", boardingStopCluster := "Coon Rapids Riverdale"]
rail_on_off[BOARDING_LOCATION == "Anoka Station & Platform", boardingStopCluster := "Anoka"]
rail_on_off[BOARDING_LOCATION == "Ramsey Station & Platform", boardingStopCluster := "Ramsey"]
rail_on_off[BOARDING_LOCATION == "Elk River Station & Platform", boardingStopCluster := "Elk River"]
rail_on_off[BOARDING_LOCATION == "Big Lake Station & Platform", boardingStopCluster := "Big Lake"]
rail_on_off[ALIGHTING_LOCATION == "Target Field Station & Rail Platform", alightingStopCluster := "Target Field"]
rail_on_off[ALIGHTING_LOCATION == "Fridley Station & Platform", alightingStopCluster := "Fridley"]
rail_on_off[ALIGHTING_LOCATION == "Coon Rapids Riverdale Station & Platform", alightingStopCluster := "Coon Rapids Riverdale"]
rail_on_off[ALIGHTING_LOCATION == "Anoka Station & Platform", alightingStopCluster := "Anoka"]
rail_on_off[ALIGHTING_LOCATION == "Ramsey Station & Platform", alightingStopCluster := "Ramsey"]
rail_on_off[ALIGHTING_LOCATION == "Elk River Station & Platform", alightingStopCluster := "Elk River"]
rail_on_off[ALIGHTING_LOCATION == "Big Lake Station & Platform", alightingStopCluster := "Big Lake"]

rail_on_off[BOARDING_LOCATION == "Mall of America Red Line Gate", boardingStopCluster := "MOA"]
rail_on_off[BOARDING_LOCATION == "Cedar Grove Transit Station", boardingStopCluster := "Cedar Grove Transit Station"]
rail_on_off[BOARDING_LOCATION == "Cedar Av at 147 St NW corner", boardingStopCluster := "Cedar Av at 147 St"]
rail_on_off[BOARDING_LOCATION == "Cedar Av at 147 St NE corner (Caribou Coffee)", boardingStopCluster := "Cedar Av at 147 St"]
rail_on_off[BOARDING_LOCATION == "Cedar Av at 140 St NE corner", boardingStopCluster := "Cedar Av at 140 St"]
rail_on_off[BOARDING_LOCATION == "Apple Valley Transitway Station NB", boardingStopCluster := "Apple Valley Transitway"]
rail_on_off[BOARDING_LOCATION == "Cedar Ave at 140 St SW corner (BRT)", boardingStopCluster := "Cedar Av at 140 St"]
rail_on_off[ALIGHTING_LOCATION == "Mall of America Red Line Gate", alightingStopCluster := "MOA"]
rail_on_off[ALIGHTING_LOCATION == "Cedar Grove Transit Station", alightingStopCluster := "Cedar Grove Transit Station"]
rail_on_off[ALIGHTING_LOCATION == "Cedar Av at 147 St NW corner", alightingStopCluster := "Cedar Av at 147 St"]
rail_on_off[ALIGHTING_LOCATION == "Cedar Av at 147 St NE corner (Caribou Coffee)", alightingStopCluster := "Cedar Av at 147 St"]
rail_on_off[ALIGHTING_LOCATION == "Cedar Av at 140 St NE corner", alightingStopCluster := "Cedar Av at 140 St"]
rail_on_off[ALIGHTING_LOCATION == "Apple Valley Transitway Station NB", alightingStopCluster := "Apple Valley Transitway"]
rail_on_off[ALIGHTING_LOCATION == "Cedar Ave at 140 St SW corner (BRT)", alightingStopCluster := "Cedar Av at 140 St"]
rail_on_off[ALIGHTING_LOCATION == "Apple Valley Transitway Station SB", alightingStopCluster := "Apple Valley Transitway"]

bus_on_off_sub <- na.omit(bus_on_off[, .(alightingStopCluster, boardingStopCluster, line_id, DATE, `TIME PERIOD`, Dir)])
rail_on_off_sub <- na.omit(rail_on_off[, .(alightingStopCluster, boardingStopCluster, line_id, DATE, TIME_PERIOD, Dir)])
setnames(bus_on_off_sub, "TIME PERIOD", "TIME_PERIOD")
on_off <- rbind(bus_on_off_sub, rail_on_off_sub)

saveRDS(on_off, "Data/on_off.RDS")

saveRDS(flow_data, "Data/flow_data.RDS")

# O-D matrices from Streetlight
tract_to_tract_personal <- fread("Data/Tract_to_Tract_flow_Fall_5533_Travel/Tract_to_Tract_5533_od_personal.csv", stringsAsFactors = FALSE)
tract_latlon <- as.data.table(tracts@data[, c("GEOID10", "INTPTLAT10", "INTPTLON10")])
setnames(tract_latlon, "GEOID10", "Origin Zone ID")
tract_to_tract_personal <- tract_latlon[tract_to_tract_personal[, `Origin Zone ID` := as.character(`Origin Zone ID`)], on = .(`Origin Zone ID`)]
setnames(tract_latlon, "Origin Zone ID", "Destination Zone ID")
tract_to_tract_personal <- tract_latlon[tract_to_tract_personal[, `Destination Zone ID` := as.character(`Destination Zone ID`)], on = .(`Destination Zone ID`)]
setnames(tract_to_tract_personal, c("INTPTLAT10", "INTPTLON10", "i.INTPTLAT10", "i.INTPTLON10"), c("dest_lat", "dest_lon", "orig_lat", "orig_lon"))
tract_to_tract_personal[, c(2, 3, 5, 6) := lapply(.SD, as.numeric), .SDcols = c(2, 3, 5, 6)]
saveRDS(tract_to_tract_personal, "Data/tract_to_tract_personal.RDS")

# LBS data 
sl_tract <- fread("Data/Tract_to_Tract_flows_Fall_5755_Travel/Tract_to_Tract_5755_od_all.csv", stringsAsFactors = FALSE)
tract_latlon <- as.data.table(tracts@data[, c("GEOID10", "INTPTLAT10", "INTPTLON10")])
setnames(tract_latlon, "GEOID10", "Origin Zone ID")
sl_tract <- tract_latlon[sl_tract[, `Origin Zone ID` := as.character(`Origin Zone ID`)], on = .(`Origin Zone ID`)]
setnames(tract_latlon, "Origin Zone ID", "Destination Zone ID")
sl_tract <- tract_latlon[sl_tract[, `Destination Zone ID` := as.character(`Destination Zone ID`)], on = .(`Destination Zone ID`)]
setnames(sl_tract, c("INTPTLAT10", "INTPTLON10", "i.INTPTLAT10", "i.INTPTLON10"), c("dest_lat", "dest_lon", "orig_lat", "orig_lon"))
sl_tract[, c(2, 3, 5, 6) := lapply(.SD, as.numeric), .SDcols = c(2, 3, 5, 6)]
saveRDS(sl_tract, "Data/sl_tract.RDS")

sl_taz <- fread("Data/TAZ_to_TAZ_flows_Fall_16_1397_Travel/TAZ_to_TAZ_flows_1397_od_all.csv", stringsAsFactors = FALSE)
taz_latlon <- as.data.table(taz@data[, c("TAZ", "lat", "lon")])
setnames(taz_latlon, c("Origin Zone ID", "orig_lat", "orig_lon"))
sl_taz <- taz_latlon[sl_taz, on = .(`Origin Zone ID`)]
setnames(taz_latlon, c("Destination Zone ID", "dest_lat", "dest_lon"))
sl_taz <- taz_latlon[sl_taz, on = .(`Destination Zone ID`)]
saveRDS(sl_taz, "Data/sl_taz.RDS")
