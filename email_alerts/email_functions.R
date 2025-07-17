require(tidyverse)
require(rinat)
require(lubridate)
require(leaflet)
require(bslib)
require(fresh)
require(png)
require(grid)
require(purrr)
require(readxl)
require(rebird)
require(downloader)
require(sf)
require(hashids)
require(shinyalert)


## List of functions
# inat_recent()
# ebird_recent()
# combine_citsci_data()
# filter_nps()
# te_species()
# watchlist_species()
# new_species()
# make_leaflet()
# leaflet_summary()




#' Function returns summaries and a data frame of recent iNaturalist observations 
#'
#' This function takes a recent time span and returns all iNaturalist records from
#' inside a desired location during that time span. Additionally, this 
#' function produces four data frames with summary statistics from the iNaturalist data.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param timespan: The recent time span of interest. Options 
#' are "week", "threedays", or "yesterday" as inputs for the "timespan" parameter.
#' @param place_id: An iNaturalist place ID in quotes. For example, the place id for 
#' Hancock County Maine would be "1647" as found in the place page url: 
#' https://www.inaturalist.org/observations?place_id=1647&subview=map
#' @seealso None
#' @export

inat_recent <- function(place_id, timespan, parkname) {
  
  ## Check to make sure that parameter inputs are correct
  # timespan
  if (timespan != "week") {
    if(timespan != "threeday") {
      if(timespan != "yesterday") {
        stop("Entered time span is not accepted. Must be 'week', 'threeday' or 'yesterday'")
      }
    }
  } 
  
  
  # Stop summarise output message
  options(dplyr.summarise.inform = FALSE)
  
  
  # Get the past week's dates and format
  date.filter <- format(Sys.Date()-1:7, "%Y-%m-%d") %>% 
    as_tibble() %>% 
    rename(date = value) %>% 
    mutate(year = as.numeric(year(date)),
           month = as.numeric(month(date)),
           day = as.numeric(day(date)))
  
  
  # List the month and year for get_inat_obs sub-function
  year <- date.filter$year
  month <- date.filter$month
  
  ym <- paste(year, month, sep = "-") %>% 
    unique() %>% 
    as.list()
  
  
  # This is the function that starts the download of inat data inside park boundary
  get_inat_data <- function(dates) {
    
    obs_year <- dates %>% 
      unlist() %>%
      str_remove(., "\\-\\d*$")
    
    obs_month <- dates %>% 
      unlist() %>% 
      str_remove(., "^\\d*\\-")
    
    get_inat_obs(#quality = "research",
                 place_id = place_id,
                 geo = TRUE,
                 year = obs_year, 
                 month = obs_month, 
                 maxresults = 10000) %>% 
      as_tibble() %>% 
      mutate(common_name = tolower(common_name)) %>% 
      rename_all( ~ str_replace_all(., "_", "."))
    
  }
  
  
  # Runs if week is called
  if (timespan == "week") {
    
    # Pull the previous week of inat data
    inat_obs <- map_dfr(ym, get_inat_data) %>% 
      filter(observed.on >= date.filter$date[7] & observed.on <= date.filter$date[1])
    
  }
  
  
  # Runs if threedays is called
  if (timespan == "threeday") {
    
    # Subset this to three days
    inat_obs <- map_dfr(ym, get_inat_data) %>% 
      filter(observed.on >= date.filter$date[3])
    
  }
  
  
  # Runs if yesterday is called
  if (timespan == "yesterday") {
    
    # Subset this to only yesterday
    inat_obs <- map_dfr(ym, get_inat_data) %>% 
      filter(observed.on == date.filter$date[1])
    
  }
  
  
  inat_obs <- inat_obs %>% 
    mutate(dup = duplicated(.),
           observed.on = as.Date(observed.on)) %>% 
    filter(dup == "FALSE") %>% 
    select(-dup) %>% 
    mutate(source = "iNaturalist",
           checklist = NA_character_,
           count = 1)
  
  
  
  # Select records inside the designated area
  filtered <- filter_nps(inat_obs, parkname, lat = "latitude", long = "longitude")
  
  
  if(length(filtered) >= 1) {
    message("Data retrieval successful!")
  } else {
    stop("There are no recent iNatuarlist records inside this park.")
  }
  
  return(filtered) 
  
}




#' Function returns a data frame of recent eBird observations.
#'
#' This function takes a recent time span and returns all eBird records from
#' inside a designated area during that time span.
#'
#' @inheritParams None
#' @return A data frame of recent eBird observations.
#' @param ebird_loc: An eBird place name as a single string with components separated by hyphens. 
#' For example, the Hancock County, Maine, USA is "US-ME-009". A full list of codes can be found 
#' here: https://support.ebird.org/en/support/solutions/articles/48000838205-download-ebird-data
#' #' @param parkname The quoted name of the national park/monument that you want to filter records by. Requires
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @export

ebird_recent <- function(ebird_loc, parkname) {
  
  # Get code list
  codelist <- ebirdregion(loc = ebird_loc, back = 7, key = "kjh86bnmkpfh") %>% 
    pull(speciesCode)
  
  
  # Create a run function to pull the data for each species in the code list
  run <- function(ebird_loc, code) {
    
    data <- ebirdregion(loc = ebird_loc, species = code, back = 7, key = "kjh86bnmkpfh") %>% 
      mutate(url = paste0("https://ebird.org/checklist/", subId))
    
    return(data)
  }
  
  
  # Map over this function and clean
  mid <- map2_dfr(ebird_loc, codelist, run) %>% 
    mutate(iconic.taxon.name = "Aves",
           obsDt = as.Date(obsDt)) %>% 
    rename(scientific.name = sciName, common.name = comName, count = howMany,
           observed.on = obsDt, place.guess = locName, latitude = lat, 
           longitude = lng, checklist = subId)
  
  
  # Select records inside the designated area
  filtered <- filter_nps(mid, parkname, lat = "latitude", long = "longitude")
  
  
  output <- filtered %>% 
    mutate(source = "eBird",
           license = NA_character_)
  
  
  if(length(output) >= 1) {
    message("Data retrieval successful!")
  } else {
    stop("There are no recent eBird records inside this park.")
  }
  
  
  return(output)
  
}




#' Function returns a combined data frame of recent citsci observations.
#'
#' @inheritParams None
#' @return A combined data frame of recent citsci observations.
#' @param x: One data frame of citsci data from either the inat_recent or the
#' ebird_recent functions.
#' @param y: Another data frame of citsci data from either the inat_recent or the
#' ebird_recent functions.
#' @param join: Another data frame of taxonomic group common names for joining
#' @export

combine_citsci_data <- function(x, y, join) {
  
  output <- bind_rows(x, y) %>% 
    mutate(common.name = tolower(common.name)) %>% 
    left_join(., join, by = "iconic.taxon.name") %>% 
    select(scientific.name, common.name, iconic.taxon.name, groups, everything()) %>%
    mutate(common.name = ifelse(common.name == "", scientific.name, common.name)) %>% 
    arrange(common.name)
  
}




#' @description A simple function that will take a data frame, filter by records inside ANP, and return a
#' cleaned data frame. IMPORTANT: This function only work for lat long data separated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the data frame you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. Requires
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a data frame of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat data frame to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, "Acadia National Park", lat = "y", long = "x")
#'
#' @export

filter_nps <- function(dat, park, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  if (park == "Acadia National Park") {
    
    acad.bounds <- sf::read_sf("email_alerts/www/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
      st_transform(4326)
    
    
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
    
    
    dat2 %>% 
      mutate(intersect = as.integer(st_intersects(geometry, acad.bounds))) %>% 
      filter(!is.na(intersect))
    
    
    output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>% 
      select(-c(CLASS, Acres, Hectares, SHAPE_Leng, SHAPE_Area)) %>% 
      select(everything(), latitude = latitude.keep, longitude = longitude.keep)
    
  } else {
    
    nps.bounds <- sf::read_sf("email_alerts/www/nps_boundary/nps_boundary.shp") %>% 
      st_transform(4326) %>% 
      filter(UNIT_NAME == paste(park))
    
    
    if (length(nps.bounds) < 1) {
      stop("Function returned a park with 0 polygons. The park name does not exist. Go to https://rpubs.com/klima21/filternps for a list of valid park names.")
    }
    
    
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(nps.bounds))
    
    
    dat2 %>% 
      mutate(intersect = as.integer(st_intersects(geometry, nps.bounds))) %>% 
      filter(!is.na(intersect))
    
    
    output <- sf::st_join(dat2, nps.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>%
      select(-c(OBJECTID:Shape_Area)) %>% 
      select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  }
  
  return(output)
}





#' Function summarizes iNaturalist observations for watchlist species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame listed as non-native and invasive as well 
#' as some summary statistics. This is done at both the federal and state levels. Data is 
#' written out to the provided directory.
#'
#' @inheritParams None
#' @return A dataframe of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @seealso None
#' @export
#' @examples  
#' watchlist_species(inat_lastweek, "outputs/te_species")

watchlist_species <- function(x, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ### THREATENED/ENDANGERED
  ## Federal
  # Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_csv("email_alerts/www/datasets/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status),
           listing.status = paste0("federally ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  ## State
  # Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("email_alerts/www/datasets/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           listing.status = tolower(listing.status),
           listing.status = paste0("state ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  # All T, E species from the last week
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, observed.on, place.guess, latitude, longitude, url) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, observed.on, location = place.guess, 
           listing.status, latitude, longitude, url)
  
  
  # All T, E species from the last week
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, observed.on, place.guess, latitude, longitude, url) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, observed.on, 
           location = place.guess, listing.status, latitude, longitude, url)
  
  # Combine and export
  all_te_sp <- dplyr::bind_rows(te_specieslist_federal, te_specieslist_state) #%>% 
  # mutate(link = paste0("<a href= ", "'", url, "' target='_blank'>view record here</a>")) %>% 
  # dplyr::select(-url)
  
  write.csv(all_te_sp, paste(output.path, "te_specieslist.csv", sep = "/"), row.names = F)
  
  
  
  ## RARE, INVASIVE, PESTS
  # Rare native species list
  listsp <- read_excel("email_alerts/www/datasets/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
  rares <- listsp %>% 
    filter(status == "rare native" | status == "insect")
  
  invasive_ne <- listsp %>% 
    filter(status == "invasive not established" |
             status == "invasive established" |
             status == "pest disease")
  
  
  # Native but rare
  rares_obs <- x %>% 
    filter(scientific.name %in% rares$scientific.name) %>% 
    arrange(desc(observed.on)) %>%
    dplyr::select(scientific.name, common.name, observed.on, 
                  location = place.guess, latitude, longitude, url)
  
  
  # Invasives and pests
  invasive_sp <- x %>% 
    filter(scientific.name %in% invasive_ne$scientific.name)
  
  inv2 <- listsp %>% 
    filter(grepl("spp.$", scientific.name)) %>% 
    mutate(genus = str_remove(scientific.name, "\\s\\w*\\."))
  
  invasive_gen <-  x %>% 
    mutate(genus = str_remove(scientific.name, "\\s\\w*")) %>% 
    filter(genus %in% inv2$genus) %>% 
    select(-genus)
  
  invasive_obs <- rbind(invasive_sp, invasive_gen) %>% 
    arrange(desc(observed.on))
  
  # Export
  write.csv(rares_obs, paste(output.path, "rare_specieslist.csv", sep = "/"), row.names = F)
  write.csv(invasive_obs, paste(output.path, "invasive_pestslist.csv", sep = "/"), row.names = F)

}




#' Function summarizes any new species observed within the designated area.
#'
#' This function takes a data frame of observations, path to the species list,
#' and the path for the outputs. It creates a CSV file of all the species in 
#' the data frame that have not been recorded in the park before. The results are
#' written out to the provided directory.
#'
#' @inheritParams None
#' @return A data frame of species not yet recorded in the designated area.
#' @param x: Data frame of citizen science observations.
#' @param species.list.path The path to your created species list for the designated area.
#' @param output.path: The path you want the summary CSV to be written to.
#' @seealso None
#' @export

new_npspecies <- function(x, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Get the full species list
  park_sp_list <- read.csv("email_alerts/www/datasets/acad_species_list.csv")
  
  
  # New species according to the NPSpecies list
  new_species <- x %>% 
    filter(scientific.name %in% park_sp_list$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    # group_by(scientific.name) %>% 
    # slice(1) %>% 
    #mutate(link = paste("<a href=", url, " target='_blank'>view record here</a>")) %>% 
    dplyr::select(scientific.name, common.name, location = place.guess, 
                  observed.on, latitude, longitude, url)
  
  
  write.csv(new_species, paste(output.path, "new_species.csv", sep = "/"), row.names = F)
  
  
}




#' Function to produce an interactive leaflet map widget of recent observations.
#'
#' This function takes a data frame of recent observations and produces a leaflet map widget 
#' with satellite imagery base layer, labels of common names that appear when the mouse is 
#' hovered over a marker, and a link to the observation online accessible by clicking a marker.
#'
#' @inheritParams None
#' @return A leaflet map widget of recent iNaturalist observations.
#' @param x: Data frame of citizen science observations.
#' @seealso None
#' @export

#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

species_leaflet <- function (x) {
  
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "' target='_blank' rel='noopener noreferrer'", 
                        ">View observation here</a></b>")) 

  
  maxLong = max(formap$longitude) + 0.1
  maxLat = max(formap$latitude) + 0.1
  minLong = min(formap$longitude) - 0.1
  minLat = min(formap$latitude) - 0.1
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
    #addProviderTiles(providers$Stamen.TerrainLabels) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               labelOptions = labelOptions(textsize = "15px"),
               clusterOptions = markerClusterOptions(),
               popup = formap$url) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
}




#' Function to produce an interactive leaflet map widget of iNaturalist observations
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and produces a leaflet map widget with satellite 
#' imagery base layer, labels of common names that appear when the mouse is hovered over a
#' marker, and a link to the observation on iNaturalist accessible by clicking a marker.
#'
#' @inheritParams None
#' @return A leaflet map widget of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

leaflet_summary <- function (x) {
  
  if (nrow(x) == 0) {
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      #addProviderTiles(providers$Stadia.StamenTonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
      #addProviderTiles(providers$Stamen.TerrainLabels) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels)
  }
  
  if (nrow(x) > 0) {
    
    formap <- x %>% 
      mutate(url = paste0("<b><a href='", url, "' target='_blank' rel='noopener noreferrer'", ">View observation here</a></b>")) 
    
    maxLong = max(formap$longitude) + 0.1
    maxLat = max(formap$latitude) + 0.1
    minLong = min(formap$longitude) - 0.1
    minLat = min(formap$latitude) - 0.1
    
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      #addProviderTiles(providers$Stadia.StamenTonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
      #addProviderTiles(providers$Stamen.TerrainLabels) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
      addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
                 labelOptions = labelOptions(textsize = "15px"),
                 clusterOptions = markerClusterOptions(),
                 popup = formap$url) %>%
      fitBounds(minLong, minLat, maxLong, maxLat)
  }
  
  return(map)
  
}



#' Function summarizes iNaturalist observations for watchlist species
#'
#' Threatened and endangered species (state and federal)
#'
#' @inheritParams None
#' @return A data frame of filtered iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

watchlist_te <- function(x) {
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ### THREATENED/ENDANGERED
  ## Federal
  # Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_csv("email_alerts/www/datasets/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status),
           listing.status = paste0("federally ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  ## State
  # Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("email_alerts/www/datasets/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           listing.status = tolower(listing.status),
           listing.status = paste0("state ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  # All T, E species federal
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    left_join(fed_te_sp, by = "scientific.name")
  
  
  # All T, E species state
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>%
    left_join(state_te_sp, by = "scientific.name")
  
  
  # Combine and export
  all_te_sp <- dplyr::bind_rows(te_specieslist_federal, te_specieslist_state) %>% 
    rename(common.name = common.name.x) %>% 
    select(-common.name.y) %>% 
    as_tibble()
  
  
  return(all_te_sp)
  
}




#' Function summarizes iNaturalist observations for watchlist species
#'
#' Rare or declining native species
#'
#' @inheritParams None
#' @return A data frame of filtered iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

watchlist_rn <- function(x) {
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ## RARE
  # Rare native species list
  listsp <- read_excel("email_alerts/www/datasets/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
  rares <- listsp %>% 
    filter(status == "rare native" | status == "insect")
  
  
  # Native but rare
  rares_obs <- x %>% 
    filter(scientific.name %in% rares$scientific.name) %>% 
    mutate(listing.status = "rare native") %>% 
    as_tibble()
  
  
  return(rares_obs)
  
}





#' Function summarizes iNaturalist observations for watchlist species
#'
#' Invasives, pests, and diseases
#'
#' @inheritParams None
#' @return A data frame of filtered iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

watchlist_inv <- function(x) {
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ## INVASIVE, PESTS
  # Rare native species list
  listsp <- read_excel("email_alerts/www/datasets/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
  invasive_ne <- listsp %>% 
    filter(status == "invasive not established" |
             status == "invasive established" |
             status == "pest disease")
  
  
  # Invasives and pests
  invasive_obs <- x %>% 
    filter(scientific.name %in% invasive_ne$scientific.name) %>% 
    mutate(listing.status = "invasive/pest/disease") %>% 
    as_tibble()
  
  
  return(invasive_obs)
  
}




## Buffer zone stuff in development: ####
# acad.bounds <- sf::read_sf("email_alerts/www/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
#   st_transform(4326)
# 
# acad.bounds_union <- acad.bounds %>%
#   st_union()
# 
# acad.bounds_union_proj <- acad.bounds_union %>%
#   st_transform(32619)
# 
# distance_meters <- 10 * 1609.34 # 30 miles in meters
# 
# acad.bounds_enlarged_proj <- acad.bounds_union_proj %>%
#   st_buffer(dist = distance_meters)
# 
# acad.bounds_enlarged_wgs84 <- acad.bounds_enlarged_proj %>%
#   st_transform(4326)
# 
# plot(st_geometry(acad.bounds_enlarged_wgs84), col = "lightblue", main = "Enlarged Park Boundary")
# plot(st_geometry(acad.bounds_union), add = TRUE, col = "red", border = "red", lwd = 2)
#   


