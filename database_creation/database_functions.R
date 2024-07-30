### Database creation functions

library(tidyverse)
library(sf)
library(readxl)




#' @description A simple function that will take a data frame, filter by records inside a park service property, 
#' and return a cleaned data frame. IMPORTANT: This function only work for lat long data separated
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
#' bird.anp <- filter_nps(bird.dat, "Acadia National ParK", lat = "y", long = "x")
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



